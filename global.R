# =============================================================================
# global.R  --  Daten, Vorverarbeitung und DLNM-Engine
# Wird einmal beim App-Start ausgefuehrt. Enthaelt die komplette Modelllogik
# aus DLNM_Masterarbeit_Alfter.R, aber so gekapselt, dass die App ein einzelnes
# Outcome "on demand" schaetzen kann (ein GLM ist schnell < 1 s).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dlnm)
  library(splines)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# -----------------------------------------------------------------------------
# 1) Daten einlesen & aufbereiten
# -----------------------------------------------------------------------------
DATA_PATH <- file.path("data", "Datensatz_Masterarbeit_Alfter.xlsx")

START_JAHR <- 2014
END_JAHR   <- 2023

df <- read_excel(DATA_PATH)
df$datum <- as.Date(as.character(df$datum), format = "%Y%m%d")
df <- df %>%
  arrange(datum) %>%
  filter(year(datum) >= START_JAHR & year(datum) <= END_JAHR)

# Wochentag als Faktor (Reihenfolge Mo..So). Sprache/Locale-unabhaengig ueber
# die Wochentagsnummer, Labels rein kosmetisch.
wd_labels <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
               "Freitag", "Samstag", "Sonntag")
df$wochentag <- factor(wd_labels[wday(df$datum, week_start = 1)], levels = wd_labels)

# -----------------------------------------------------------------------------
# 2) Feiertage (feste + bewegliche via Computus)
# -----------------------------------------------------------------------------
feiertage_fest <- c("01-01", "05-01", "10-03", "12-25", "12-26")

easter_sunday <- function(jahr) {
  a <- jahr %% 19; b <- jahr %/% 100; c <- jahr %% 100
  d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3; h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day <- ((h + l - 7 * m + 114) %% 31) + 1
  as.Date(paste(jahr, month, day, sep = "-"))
}

alle_feiertage <- c()
for (jahr in unique(year(df$datum))) {
  alle_feiertage <- c(alle_feiertage, as.Date(paste0(jahr, "-", feiertage_fest)))
  o <- easter_sunday(jahr)
  alle_feiertage <- c(alle_feiertage, o - 2, o + 1, o + 39, o + 50)
}
df$holiday <- as.numeric(df$datum %in% as.Date(alle_feiertage))

# -----------------------------------------------------------------------------
# 3) Temperatur-Schwellen + Extrem-/Schock-/Wellentage
# -----------------------------------------------------------------------------
df$lufttemp_avg <- as.numeric(df$lufttemp_avg)

SCHWELLE_HEISS <- quantile(df$lufttemp_avg, 0.95, na.rm = TRUE)
SCHWELLE_KALT  <- quantile(df$lufttemp_avg, 0.05, na.rm = TRUE)
MEDIAN_TEMP    <- median(df$lufttemp_avg, na.rm = TRUE)

df <- df %>%
  mutate(
    extremtag_heiss        = as.numeric(lufttemp_avg >= SCHWELLE_HEISS),
    extremtag_heiss_vortag = lag(extremtag_heiss, 1, default = 0),
    schocktag_heiss = ifelse(extremtag_heiss == 1 & extremtag_heiss_vortag == 0, 1, 0),
    wellentag_heiss = ifelse(extremtag_heiss == 1 & extremtag_heiss_vortag == 1, 1, 0),
    extremtag_kalt        = as.numeric(lufttemp_avg <= SCHWELLE_KALT),
    extremtag_kalt_vortag = lag(extremtag_kalt, 1, default = 0),
    schocktag_kalt = ifelse(extremtag_kalt == 1 & extremtag_kalt_vortag == 0, 1, 0),
    wellentag_kalt = ifelse(extremtag_kalt == 1 & extremtag_kalt_vortag == 1, 1, 0)
  ) %>%
  mutate(
    schocktag_heiss = ifelse(row_number() == 1, NA, schocktag_heiss),
    wellentag_heiss = ifelse(row_number() == 1, NA, wellentag_heiss),
    schocktag_kalt  = ifelse(row_number() == 1, NA, schocktag_kalt),
    wellentag_kalt  = ifelse(row_number() == 1, NA, wellentag_kalt)
  )

# Zusatz-Outcome "gesamt"
df$gesamt <- rowSums(df[, c("herz_gesamt", "pulmonal_gesamt", "volumenmangel_gesamt")],
                     na.rm = TRUE)
df$year <- year(df$datum)

ANZ_JAHRE <- as.numeric(difftime(max(df$datum), min(df$datum), units = "days")) / 365

# Spline-Knots fuer Taupunkt (Kontrollvariable)
KNOTS_TAUTEMP <- quantile(df$tautemp_avg, c(0.10, 0.75, 0.90), na.rm = TRUE)

# -----------------------------------------------------------------------------
# 4) Outcome-Metadaten (Label + Gruppierung fuer die UI)
# -----------------------------------------------------------------------------
outcome_meta <- tibble::tribble(
  ~key,                    ~label,                        ~gruppe,          ~dimension,
  "gesamt",                "All cases",                   "Total",          "Main groups",
  "herz_gesamt",           "Cardiovascular",              "Cardiovascular", "Main groups",
  "pulmonal_gesamt",       "Pulmonary",                   "Pulmonary",      "Main groups",
  "volumenmangel_gesamt",  "Volume depletion",            "Volume depletion","Main groups",
  "reg_herz_01",           "Cardiovascular – Region 1",  "Cardiovascular", "Region",
  "reg_herz_02",           "Cardiovascular – Region 2",  "Cardiovascular", "Region",
  "reg_herz_03",           "Cardiovascular – Region 3",  "Cardiovascular", "Region",
  "reg_pul_01",            "Pulmonary – Region 1",       "Pulmonary",      "Region",
  "reg_pul_02",            "Pulmonary – Region 2",       "Pulmonary",      "Region",
  "reg_pul_03",            "Pulmonary – Region 3",       "Pulmonary",      "Region",
  "reg_vol_01",            "Volume depletion – Region 1","Volume depletion","Region",
  "reg_vol_02",            "Volume depletion – Region 2","Volume depletion","Region",
  "reg_vol_03",            "Volume depletion – Region 3","Volume depletion","Region",
  "herz_m_gesamt",         "Cardiovascular – Men",       "Cardiovascular", "Sex / Age",
  "herz_m_15_64",          "Cardiovascular – Men 15–64", "Cardiovascular", "Sex / Age",
  "herz_m_64_plus",        "Cardiovascular – Men 65+",   "Cardiovascular", "Sex / Age",
  "herz_f_gesamt",         "Cardiovascular – Women",     "Cardiovascular", "Sex / Age",
  "herz_f_15_64",          "Cardiovascular – Women 15–64","Cardiovascular","Sex / Age",
  "herz_f_64_plus",        "Cardiovascular – Women 65+", "Cardiovascular", "Sex / Age",
  "pul_m_gesamt",          "Pulmonary – Men",            "Pulmonary",      "Sex / Age",
  "pul_m_15_64",           "Pulmonary – Men 15–64",      "Pulmonary",      "Sex / Age",
  "pul_m_64_plus",         "Pulmonary – Men 65+",        "Pulmonary",      "Sex / Age",
  "pul_f_gesamt",          "Pulmonary – Women",          "Pulmonary",      "Sex / Age",
  "pul_f_15_64",           "Pulmonary – Women 15–64",    "Pulmonary",      "Sex / Age",
  "pul_f_64_plus",         "Pulmonary – Women 65+",      "Pulmonary",      "Sex / Age",
  "vol_m_gesamt",          "Volume depletion – Men",     "Volume depletion","Sex / Age",
  "vol_m_15_64",           "Volume depletion – Men 15–64","Volume depletion","Sex / Age",
  "vol_m_64_plus",         "Volume depletion – Men 65+", "Volume depletion","Sex / Age",
  "vol_f_gesamt",          "Volume depletion – Women",   "Volume depletion","Sex / Age",
  "vol_f_15_64",           "Volume depletion – Women 15–64","Volume depletion","Sex / Age",
  "vol_f_64_plus",         "Volume depletion – Women 65+","Volume depletion","Sex / Age"
)

outcome_label <- function(key) {
  m <- outcome_meta$label[match(key, outcome_meta$key)]
  ifelse(is.na(m), key, m)
}

# Named list fuer selectInput, gruppiert nach Dimension
outcome_choices <- lapply(split(outcome_meta, outcome_meta$dimension), function(d) {
  setNames(d$key, d$label)
})

# -----------------------------------------------------------------------------
# 5) DLNM-Engine: ein Outcome schaetzen (mit Cache)
# -----------------------------------------------------------------------------
.fit_cache <- new.env(parent = emptyenv())

# Crossbasis haengt nur von lufttemp_avg + Lag/df ab -> einmal bauen und cachen
.make_cb <- function(lag = 21, var_df = 4, lag_df = 4) {
  crossbasis(df$lufttemp_avg, lag = lag,
             argvar = list(fun = "ns", df = var_df),
             arglag = list(fun = "ns", df = lag_df))
}

#' Schaetzt das DLNM-GLM fuer ein Outcome und liefert alle Plot-/Kennzahldaten.
#' @return list(model, mrt, curve, lag_hot, lag_cold, contour, effects, thresholds)
fit_outcome <- function(outcome, lag = 21, var_df = 4, lag_df = 4) {
  # A natural spline with lag_df df needs at least lag_df+1 lag points (0..lag),
  # so for very short lags the lag-spline df must be reduced or the fit fails.
  lag_df <- max(1, min(lag_df, lag))
  cache_key <- paste(outcome, lag, var_df, lag_df, sep = "_")
  if (!is.null(.fit_cache[[cache_key]])) return(.fit_cache[[cache_key]])

  cb_temp <- .make_cb(lag, var_df, lag_df)

  formel <- paste0(
    outcome, " ~ cb_temp + schocktag_heiss + wellentag_heiss + ",
    "schocktag_kalt + wellentag_kalt + ",
    "ns(datum, df = round(ANZ_JAHRE)*7) + wochentag + holiday + ",
    "ns(tautemp_avg, knots = KNOTS_TAUTEMP)"
  )

  model <- glm(as.formula(formel), family = quasipoisson(),
               data = df, na.action = na.omit)

  # RR-Kurve zunaechst auf Median zentrieren -> MRT (Min-Risk-Temp) im P50-P94
  pred_med <- crosspred(cb_temp, model = model,
                        coef = coef(model), vcov = vcov(model),
                        cen = MEDIAN_TEMP, by = 1)
  rrs   <- pred_med$allRRfit
  temps <- as.numeric(names(rrs))
  bereich <- quantile(df$lufttemp_avg, c(0.50, 0.94), na.rm = TRUE)
  sel <- temps >= bereich[1] & temps <= bereich[2]
  mrt <- temps[sel][which.min(rrs[sel])]

  # Finale Vorhersage auf MRT zentriert
  pred <- crosspred(cb_temp, model = model,
                    coef = coef(model), vcov = vcov(model),
                    cen = mrt, by = 1)

  curve <- data.frame(
    Temperature = as.numeric(names(pred$allRRfit)),
    RR      = pred$allRRfit,
    CI_low  = pred$allRRlow,
    CI_high = pred$allRRhigh
  )
  curve$Percentile <- ecdf(df$lufttemp_avg)(curve$Temperature) * 100

  # Punktschaetzer bei P5 / P95
  q <- quantile(df$lufttemp_avg, c(0.05, 0.95), na.rm = TRUE)
  rr_at <- function(t) {
    i <- which.min(abs(curve$Temperature - t))
    c(rr = curve$RR[i], lo = curve$CI_low[i], hi = curve$CI_high[i], temp = curve$Temperature[i])
  }
  rr_cold <- rr_at(q[1]); rr_hot <- rr_at(q[2])

  # Lag-Profile bei P5 / P95 (relativ zur MRT).
  # Diese dlnm-Version liefert das Lag-Profil in matRRfit (1 x n_lag-Matrix,
  # Spalten lag0..lagN), nicht in lagRRfit.
  lag_df_fun <- function(at, label) {
    p <- tryCatch(crosspred(cb_temp, model = model, at = at, cen = mrt, bylag = 1),
                  error = function(e) NULL)
    if (is.null(p) || length(p$matRRfit) == 0) return(NULL)
    lags <- as.numeric(sub("lag", "", colnames(p$matRRfit)))
    data.frame(Lag = lags,
               RR = as.vector(p$matRRfit),
               CI_low = as.vector(p$matRRlow),
               CI_high = as.vector(p$matRRhigh),
               Schwelle = label)
  }
  lag_hot  <- lag_df_fun(round(SCHWELLE_HEISS, 1), sprintf("Heat (P95: %.1f°C)", SCHWELLE_HEISS))
  lag_cold <- lag_df_fun(round(SCHWELLE_KALT, 1),  sprintf("Cold (P5: %.1f°C)", SCHWELLE_KALT))

  # Contour-Flaeche RR ueber Temperatur x Lag (aus matRRfit)
  mat <- pred$matRRfit
  contour <- expand.grid(Temperature = as.numeric(rownames(mat)),
                         Lag = as.numeric(sub("lag", "", colnames(mat))))
  contour$RR <- as.vector(mat)

  # Diskrete Effekte Schock-/Wellentage: RR = exp(coef), CI via confint
  eff <- function(term) {
    if (!(term %in% names(coef(model))) || is.na(coef(model)[term]))
      return(c(rr = NA, lo = NA, hi = NA, p = NA))
    rr <- exp(coef(model)[term])
    ci <- tryCatch(exp(suppressMessages(confint(model, term))), error = function(e) c(NA, NA))
    p  <- tryCatch(summary(model)$coefficients[term, "Pr(>|t|)"], error = function(e) NA)
    c(rr = unname(rr), lo = unname(ci[1]), hi = unname(ci[2]), p = unname(p))
  }
  effects <- list(
    schock_heiss = eff("schocktag_heiss"),
    welle_heiss  = eff("wellentag_heiss"),
    schock_kalt  = eff("schocktag_kalt"),
    welle_kalt   = eff("wellentag_kalt")
  )

  res <- list(
    model = model, mrt = mrt, curve = curve,
    rr_cold = rr_cold, rr_hot = rr_hot,
    lag_hot = lag_hot, lag_cold = lag_cold,
    contour = contour, effects = effects,
    thresholds = c(kalt = unname(SCHWELLE_KALT), heiss = unname(SCHWELLE_HEISS))
  )
  .fit_cache[[cache_key]] <- res
  res
}

# -----------------------------------------------------------------------------
# 6) Descriptive data for the explorer (computed once)
# -----------------------------------------------------------------------------
extreme_days_per_year <- df %>%
  group_by(year) %>%
  summarise(
    `Heat days`       = sum(extremtag_heiss, na.rm = TRUE),
    `Heat shock days` = sum(schocktag_heiss, na.rm = TRUE),
    `Heat wave days`  = sum(wellentag_heiss, na.rm = TRUE),
    `Cold days`       = sum(extremtag_kalt, na.rm = TRUE),
    `Cold shock days` = sum(schocktag_kalt, na.rm = TRUE),
    `Cold wave days`  = sum(wellentag_kalt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = year)

var_labels <- c(
  lufttemp_avg = "Air temperature (°C)", tautemp_avg = "Dew point (°C)",
  gesamt = "All cases", herz_gesamt = "Cardiovascular",
  pulmonal_gesamt = "Pulmonary", volumenmangel_gesamt = "Volume depletion"
)

descriptives <- function(cols = names(var_labels)) {
  do.call(rbind, lapply(cols, function(col) {
    v <- na.omit(as.numeric(df[[col]]))
    data.frame(
      Variable = unname(var_labels[col]), n = length(v),
      Min = round(min(v), 2), P25 = round(quantile(v, .25), 2),
      Median = round(median(v), 2), P75 = round(quantile(v, .75), 2),
      Max = round(max(v), 2), Mean = round(mean(v), 2),
      SD = round(sd(v), 2), row.names = NULL
    )
  }))
}

# -----------------------------------------------------------------------------
# 7) Helpers for result tables
# -----------------------------------------------------------------------------
fmt_ci <- function(rr, lo, hi) {
  if (is.na(rr)) return(NA_character_)
  if (is.na(lo) || is.na(hi) || is.infinite(lo) || is.infinite(hi))
    return(sprintf("%.3f (CI n/a)", rr))
  sprintf("%.3f (%.3f – %.3f)", rr, lo, hi)
}

# Two-sided p-value derived from an RR and its 95% CI (as in the thesis script)
p_from_ci <- function(rr, lo, hi) {
  if (any(is.na(c(rr, lo, hi))) || lo <= 0 || hi <= 0) return(NA_real_)
  se <- (log(hi) - log(lo)) / (2 * 1.96)
  2 * pnorm(-abs(log(rr) / se))
}

# -----------------------------------------------------------------------------
# 8) Precomputed result tables (loaded from RDS if present; see precompute.R).
#    Keeps the deployed app fast -- no need to fit all 31 models at startup.
# -----------------------------------------------------------------------------
RESULTS_PATH <- file.path("data", "results.rds")
results_tables <- if (file.exists(RESULTS_PATH)) readRDS(RESULTS_PATH) else NULL

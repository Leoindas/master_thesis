# =============================================================================
# global.R  --  Laufzeit der App. Laedt ausschliesslich data/app_data.rds.
#
# Die App rechnet nichts mehr selbst und kennt den Originaldatensatz nicht:
# alle Kurven, Flaechen und Tabellen sind offline von build.R vorberechnet
# (siehe model.R fuer die eigentliche DLNM-Logik). Damit enthaelt weder das
# Repo noch der Server die zugrunde liegenden Tagesdaten.
# =============================================================================

DATA_PATH <- file.path("data", "app_data.rds")
if (!file.exists(DATA_PATH))
  stop("data/app_data.rds fehlt. Einmal `Rscript build.R` laufen lassen ",
       "(braucht den Originaldatensatz, siehe model.R).")

app_data <- readRDS(DATA_PATH)

START_JAHR     <- app_data$meta$start_year
END_JAHR       <- app_data$meta$end_year
N_DAYS         <- app_data$meta$n_days
SCHWELLEN      <- app_data$meta$thresholds
LAG_MAX        <- app_data$meta$lag_max
DATA_GENERATED <- app_data$meta$generated

outcome_meta   <- app_data$outcome_meta
monthly        <- app_data$monthly
extreme_days_per_year <- app_data$extreme_days
descriptives   <- app_data$descriptives
results_tables <- app_data$results

outcome_label <- function(key) {
  m <- outcome_meta$label[match(key, outcome_meta$key)]
  ifelse(is.na(m), key, m)
}

# Named list fuer selectInput, gruppiert nach Dimension
outcome_choices <- lapply(split(outcome_meta, outcome_meta$dimension), function(d) {
  setNames(d$key, d$label)
})

#' Vorberechnete Plot-Daten eines Outcomes.
#' Liefert dieselbe Struktur wie frueher fit_outcome(), nur ohne `model`:
#' list(mrt, rr_cold, rr_hot, curve, lag_hot, lag_cold, contour, effects)
get_outcome <- function(key) {
  res <- app_data$outcomes[[key]]
  if (is.null(res)) stop("Unbekanntes Outcome: ", key)
  res$thresholds <- SCHWELLEN
  res
}

# -----------------------------------------------------------------------------
# Helpers fuer die Ergebnistabellen
# -----------------------------------------------------------------------------
fmt_ci <- function(rr, lo, hi) {
  if (is.na(rr)) return(NA_character_)
  if (is.na(lo) || is.na(hi) || is.infinite(lo) || is.infinite(hi))
    return(sprintf("%.3f (CI n/a)", rr))
  sprintf("%.3f (%.3f – %.3f)", rr, lo, hi)
}

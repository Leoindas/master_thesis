# =============================================================================
# precompute.R  --  Fit all 31 outcomes once and build the result tables that
# the "Results" tab shows. Saves data/results.rds so the deployed app stays
# fast (only single-outcome models are fitted live).
#
# Run once (and whenever the data/model changes):
#   Rscript precompute.R
# =============================================================================
if (!file.exists("global.R")) setwd("C:/Users/chris/OneDrive/Masterarbeit/ShinyApp")
suppressPackageStartupMessages({ library(car); library(dplyr) })
source("global.R")

message("Fitting all outcomes ...")
models <- list()
for (oc in outcome_meta$key) {
  message("  - ", oc)
  models[[oc]] <- fit_outcome(oc)
}

# -----------------------------------------------------------------------------
# (1) RR summary table (one row per outcome)
# -----------------------------------------------------------------------------
rr_summary <- do.call(rbind, lapply(outcome_meta$key, function(oc) {
  f <- models[[oc]]; e <- f$effects
  data.frame(
    Outcome = outcome_label(oc),
    `MRT (°C)` = round(f$mrt, 1),
    `RR heat P95` = fmt_ci(f$rr_hot["rr"], f$rr_hot["lo"], f$rr_hot["hi"]),
    `p heat` = round(p_from_ci(f$rr_hot["rr"], f$rr_hot["lo"], f$rr_hot["hi"]), 4),
    `RR cold P5` = fmt_ci(f$rr_cold["rr"], f$rr_cold["lo"], f$rr_cold["hi"]),
    `p cold` = round(p_from_ci(f$rr_cold["rr"], f$rr_cold["lo"], f$rr_cold["hi"]), 4),
    `RR heat shock` = fmt_ci(e$schock_heiss["rr"], e$schock_heiss["lo"], e$schock_heiss["hi"]),
    `RR heat wave`  = fmt_ci(e$welle_heiss["rr"],  e$welle_heiss["lo"],  e$welle_heiss["hi"]),
    `RR cold shock` = fmt_ci(e$schock_kalt["rr"],  e$schock_kalt["lo"],  e$schock_kalt["hi"]),
    `RR cold wave`  = fmt_ci(e$welle_kalt["rr"],   e$welle_kalt["lo"],   e$welle_kalt["hi"]),
    check.names = FALSE, row.names = NULL
  )
}))

# -----------------------------------------------------------------------------
# (2) Detailed RR at extreme percentiles (heat & cold)
# -----------------------------------------------------------------------------
extreme_rr <- function(percentiles) {
  do.call(rbind, lapply(outcome_meta$key, function(oc) {
    f <- models[[oc]]; cu <- f$curve
    do.call(rbind, lapply(percentiles, function(p) {
      tq <- quantile(df$lufttemp_avg, p, na.rm = TRUE)
      i <- which.min(abs(cu$Temperature - tq))
      data.frame(
        Outcome = outcome_label(oc), Percentile = p * 100,
        `Temp (°C)` = round(cu$Temperature[i], 1),
        RR = fmt_ci(cu$RR[i], cu$CI_low[i], cu$CI_high[i]),
        p = round(p_from_ci(cu$RR[i], cu$CI_low[i], cu$CI_high[i]), 4),
        check.names = FALSE, row.names = NULL
      )
    }))
  }))
}
extreme_heat <- extreme_rr(c(0.95, 0.96, 0.97, 0.98, 0.99, 1.00))
extreme_cold <- extreme_rr(c(0.05, 0.04, 0.03, 0.02, 0.01, 0.00))

# -----------------------------------------------------------------------------
# (3) Wald tests: subgroup comparisons of single coefficients between models
# -----------------------------------------------------------------------------
coef_terms <- c("schocktag_heiss", "wellentag_heiss", "schocktag_kalt", "wellentag_kalt")
term_label <- c(schocktag_heiss = "Heat shock", wellentag_heiss = "Heat wave",
                schocktag_kalt = "Cold shock", wellentag_kalt = "Cold wave",
                cb_temp = "Temperature (cb)")

wald <- function(oc1, oc2, term, comparison) {
  m1 <- models[[oc1]]$model; m2 <- models[[oc2]]$model
  if (is.null(m1) || is.null(m2)) return(NULL)
  if (!(term %in% names(coef(m1)) && term %in% names(coef(m2)))) return(NULL)
  b1 <- coef(m1)[term]; b2 <- coef(m2)[term]
  v1 <- vcov(m1)[term, term]; v2 <- vcov(m2)[term, term]
  z <- (b1 - b2) / sqrt(v1 + v2)
  data.frame(Comparison = comparison, Coefficient = unname(term_label[term]),
             `z value` = round(unname(z), 3),
             `p value` = round(2 * pnorm(-abs(z)), 4),
             check.names = FALSE, row.names = NULL)
}

wald_rows <- list()
add_wald <- function(...) wald_rows[[length(wald_rows) + 1]] <<- wald(...)

# H1 disease groups
grp <- c("herz_gesamt", "pulmonal_gesamt", "volumenmangel_gesamt")
for (i in 1:2) for (j in (i + 1):3) for (t in c("cb_temp", coef_terms))
  add_wald(grp[i], grp[j], t, paste(outcome_label(grp[i]), "vs.", outcome_label(grp[j])))
# H2 age within sex/group
for (g in c("herz_m", "herz_f", "pul_m", "pul_f", "vol_m", "vol_f"))
  for (t in coef_terms)
    add_wald(paste0(g, "_15_64"), paste0(g, "_64_plus"), t,
             paste(outcome_label(paste0(g, "_15_64")), "vs. 65+"))
# H3 sex within group (overall)
for (g in c("herz", "pul", "vol")) for (t in coef_terms)
  add_wald(paste0(g, "_m_gesamt"), paste0(g, "_f_gesamt"), t,
           paste(outcome_label(paste0(g, "_m_gesamt")), "vs. Women"))
# H4 regions within group
for (g in c("herz", "pul", "vol")) for (i in 1:2) for (j in (i + 1):3)
  for (t in c("cb_temp", coef_terms))
    add_wald(paste0("reg_", g, "_0", i), paste0("reg_", g, "_0", j), t,
             paste(outcome_label(paste0("reg_", g, "_0", i)), "vs. Region", j))

wald_tests <- do.call(rbind, wald_rows)

# -----------------------------------------------------------------------------
# (4) Shock vs. wave (F-test of coefficient equality per outcome)
# -----------------------------------------------------------------------------
sw_rows <- list()
for (oc in outcome_meta$key) {
  m <- models[[oc]]$model; if (is.null(m)) next
  do_test <- function(hyp, label) {
    tryCatch({
      r <- linearHypothesis(m, hyp, test = "F")
      data.frame(Outcome = outcome_label(oc), Comparison = label,
                 `F value` = round(r$`F`[2], 3), `p value` = round(r$`Pr(>F)`[2], 4),
                 check.names = FALSE, row.names = NULL)
    }, error = function(e) NULL)
  }
  sw_rows[[length(sw_rows) + 1]] <- do_test("schocktag_heiss - wellentag_heiss = 0",
                                            "Heat: shock vs. wave")
  sw_rows[[length(sw_rows) + 1]] <- do_test("schocktag_kalt - wellentag_kalt = 0",
                                            "Cold: shock vs. wave")
}
shock_wave <- do.call(rbind, sw_rows)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------
results_tables <- list(
  rr_summary   = rr_summary,
  extreme_heat = extreme_heat,
  extreme_cold = extreme_cold,
  wald_tests   = wald_tests,
  shock_wave   = shock_wave,
  descriptives = descriptives(),
  extreme_days = extreme_days_per_year,
  generated    = format(Sys.time(), "%Y-%m-%d %H:%M")
)
saveRDS(results_tables, file.path("data", "results.rds"))
message("Saved data/results.rds  (", nrow(rr_summary), " outcomes, ",
        nrow(wald_tests), " Wald tests, ", nrow(shock_wave), " F-tests)")

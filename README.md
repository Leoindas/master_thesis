# Master's thesis – Interactive DLNM app

Interactive Shiny presentation of the master's thesis *"Extreme temperatures and
hospital admissions"* (DLNM analysis, 2014–2023).

[[![Live App – Posit Connect Cloud](https://img.shields.io/badge/Live%20App-Posit%20Connect%20Cloud-447099?logo=posit)](https://019f27d4-52e6-6163-f91a-c83bd809dbcd.share.connect.posit.cloud/)

**▶️ [Try the live app in your browser](https://019f27d4-52e6-6163-f91a-c83bd809dbcd.share.connect.posit.cloud/)** – hosted on Posit Connect Cloud, no installation needed.](https://chalf-master-thesis.share.connect.posit.cloud/)

## Run locally

In R / RStudio:

```r
setwd("C:/Users/chris/OneDrive/Masterarbeit/ShinyApp")
shiny::runApp(launch.browser = TRUE)
```

Or from the command line:

```powershell
& "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" -e "shiny::runApp('C:/Users/chris/OneDrive/Masterarbeit/ShinyApp', launch.browser=TRUE)"
```

## Files

| File | Purpose |
|------|---------|
| `app.R`        | UI (bslib `page_navbar`) + server |
| `global.R`     | Data prep + DLNM engine `fit_outcome()`, loaded once at start |
| `precompute.R` | Fits all 31 outcomes once and writes `data/results.rds` (the Results tab) |
| `deploy.R`     | Publishes the app to shinyapps.io |
| `data/`        | Dataset copy + `results.rds` |

## Tabs

- **Overview** – intro, English abstract, key figures, and a "Request the full
  thesis" button (opens a pre-filled email to `CONTACT_EMAIL` in `app.R`)
- **Methods** – detailed, reproducible methodology in a 10-section accordion
  (design & hypotheses, data + ICD-10 codes, exposure/shock/wave, outcomes,
  confounders, DLNM model + formula + exact R code, RR & MRT, inference,
  sensitivity analyses, software)
- **Data** – time series, extreme days per year, descriptive statistics, raw data
- **Dose-response** – U-curve (RR vs. temperature) with MRT / P5 / P95; model fitted live
- **Lag profile** – RR over 0–21 days for heat (P95) and cold (P5)
- **Contour** – RR surface over temperature × lag (thesis colours: red = RR>1)
- **Shock vs. wave** – discrete effects: first extreme day vs. sustained spell
- **Results** – all thesis result tables (RR summary, extreme-percentile RRs,
  subgroup Wald tests, shock-vs-wave F-tests), searchable + CSV export

The model matches `DLNM_Masterarbeit_Alfter.R`: quasi-Poisson GLM with a
cross-basis (lag 21, ns df=4/4), adjusted for time trend, day of week, holidays
and dew point. Single outcomes are fitted on the fly and cached; the Results tab
is precomputed for speed.

## Rebuilding the result tables

If the data or model changes, regenerate the Results tab:

```powershell
& "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" precompute.R
```

## Deploy a shareable link (shinyapps.io, free)

1. Create a free account at <https://www.shinyapps.io>.
2. Dashboard → avatar (top right) → **Tokens** → **Add Token** → **Show** →
   **Copy to clipboard**. You get `name`, `token`, `secret`.
3. Paste them into `deploy.R` (or run the `setAccountInfo(...)` line once in the
   R console so the secret isn't stored in the file), then:

   ```powershell
   & "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" deploy.R
   ```

The live URL will be `https://<account>.shinyapps.io/temperature-health`.

> **Note:** shinyapps.io is being migrated to **Posit Connect Cloud** by the end
> of 2026. The GitHub route below is the future-proof successor.

## Deploy via GitHub + Posit Connect Cloud (free, future-proof)

Connect Cloud deploys straight from a **public** GitHub repo and needs a
`manifest.json` (already generated here; regenerate with
`rsconnect::writeManifest(appFiles = c("app.R","global.R","data/Datensatz_Masterarbeit_Alfter.xlsx","data/results.rds"))`
whenever the app or its dependencies change).

1. Create a new **public** repo on GitHub (e.g. `temperature-health`).
2. Push this folder:

   ```bash
   git remote add origin https://github.com/<user>/temperature-health.git
   git branch -M main
   git push -u origin main
   ```

3. Go to <https://connect.posit.cloud>, sign in with GitHub, click
   **Publish → Shiny (R)**, pick the repo/branch, confirm `app.R` +
   `manifest.json`, and publish.

Redeploys happen by pushing new commits (Connect Cloud re-reads the repo).

### Data exposure note

The public app bundles the aggregated dataset (needed to fit models live). The
raw row-level CSV download is **disabled** by default (`ALLOW_RAW_DOWNLOAD <-
FALSE` in `app.R`). Set it to `TRUE` only if the data may be published. Result
tables (aggregate) remain downloadable.

## Packages

`shiny`, `bslib`, `plotly`, `DT`, `thematic`, `dlnm`, `splines`, `lubridate`,
`dplyr`, `ggplot2`, `scales`, `readxl`, `tibble` (app) · `car` (precompute only)
· `rsconnect` (deploy only).

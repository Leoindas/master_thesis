# Master's thesis – Interactive DLNM app

Interactive Shiny presentation of the master's thesis *"Extreme temperatures and
hospital admissions"* (DLNM analysis, 2014–2023).

[![Live App – Posit Connect Cloud](https://img.shields.io/badge/Live%20App-Posit%20Connect%20Cloud-447099?logo=posit)](https://chalf-master-thesis.share.connect.posit.cloud/)

**▶️ [Try the live app in your browser](https://chalf-master-thesis.share.connect.posit.cloud/)** – hosted on Posit Connect Cloud, no installation needed.

## Data policy

**This repository does not contain the underlying dataset, and neither does the
deployed app.** The app shows *results only*: every curve, surface and table is
precomputed offline by `build.R` and stored in `data/app_data.rds`. That file
holds model output (relative risks, confidence intervals, test statistics) plus
monthly aggregates and summary statistics — no daily values, and no fitted model
objects (a `glm` would carry its model frame, i.e. the full daily data, with it).

The original Excel file stays outside the repository and is git-ignored. `build.R`
reads it from the path in `model.R`, overridable via the `THESIS_DATA`
environment variable.

## Architecture

The analysis code and the app runtime are deliberately separate:

```
model.R  ──(needs the dataset)──►  build.R  ──►  data/app_data.rds  ──►  global.R ──► app.R
        offline, on your machine                    committed              deployed
```

| File | Runs where | Purpose |
|------|-----------|---------|
| `model.R`  | offline only | Data prep + DLNM engine `fit_outcome()`. Needs the dataset. **Not deployed.** |
| `build.R`  | offline only | Fits all 31 outcomes once, writes `data/app_data.rds`. **Not deployed.** |
| `global.R` | app runtime  | Loads `data/app_data.rds`, exposes `get_outcome()`. No data, no model fitting. |
| `app.R`    | app runtime  | UI (bslib `page_navbar`) + server |
| `deploy.R` | offline only | Publishes to shinyapps.io |

## Run locally

Needs `data/app_data.rds` (committed). In R / RStudio:

```r
setwd("C:/Users/chris/OneDrive/Masterarbeit/ShinyApp")
shiny::runApp(launch.browser = TRUE)
```

Or from the command line:

```powershell
& "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" -e "shiny::runApp('C:/Users/chris/OneDrive/Masterarbeit/ShinyApp', launch.browser=TRUE)"
```

## Rebuilding after a data or model change

Requires the original dataset:

```powershell
& "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" build.R
```

This refits all 31 outcomes and rewrites `data/app_data.rds` (~220 KB). Then
regenerate the deployment manifest:

```powershell
& "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" -e "rsconnect::writeManifest(appFiles = c('app.R','global.R','data/app_data.rds'))"
```

## Tabs

- **Overview** – intro, English abstract, key figures
- **Methods** – reproducible methodology in an 11-section accordion (design &
  hypotheses, data + ICD-10 codes, exposure/shock/wave, outcomes, confounders,
  DLNM model + formula + exact R code, RR & MRT, inference, sensitivity
  analyses, software, data availability)
- **Data** – monthly time series, extreme days per year, descriptive statistics
- **Dose-response** – U-curve (RR vs. temperature) with MRT / P5 / P95
- **Lag profile** – RR over 0–21 days for heat (P95) and cold (P5)
- **Contour** – RR surface over temperature × lag (thesis colours: red = RR>1)
- **Shock vs. wave** – discrete effects: first extreme day vs. sustained spell
- **Results** – all thesis result tables (RR summary, extreme-percentile RRs,
  subgroup Wald tests, shock-vs-wave F-tests), searchable + CSV export

The three model charts each carry a collapsible legend explaining RR, MRT,
P5/P95, lag and the confidence bands.

The model matches `DLNM_Masterarbeit_Alfter.R`: quasi-Poisson GLM with a
cross-basis (lag 21, ns df=4/4), adjusted for time trend, day of week, holidays
and dew point.

## Deploy via GitHub + Posit Connect Cloud

Connect Cloud deploys straight from a **public** GitHub repo and needs a
`manifest.json` (regenerate it whenever the app or its dependencies change, see
above). Publishing is a plain `git push` — Connect Cloud re-reads the repo and
redeploys within a minute.

Because only `app.R`, `global.R` and `data/app_data.rds` are listed in the
manifest, the public repo being a requirement of the free tier no longer means
publishing the data.

## Deploy to shinyapps.io (alternative)

1. Create a free account at <https://www.shinyapps.io>.
2. Dashboard → avatar (top right) → **Tokens** → **Add Token** → **Show** →
   **Copy to clipboard**. You get `name`, `token`, `secret`.
3. Paste them into `deploy.R` (or run the `setAccountInfo(...)` line once in the
   R console so the secret isn't stored in the file), then:

   ```powershell
   & "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" deploy.R
   ```

## Packages

Runtime: `shiny`, `bslib`, `plotly`, `DT`, `thematic`, `ggplot2`.
Build only: `dlnm`, `splines`, `lubridate`, `dplyr`, `readxl`, `tibble`, `car`.
Deploy only: `rsconnect`.

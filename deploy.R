# =============================================================================
# deploy.R  --  Publish the app to shinyapps.io (free tier)
#
# ONE-TIME SETUP
# --------------
# 1. Create a free account at https://www.shinyapps.io  (Google/GitHub login works).
# 2. In the shinyapps.io dashboard: top-right avatar -> "Tokens" -> "Add Token"
#    -> "Show" -> "Copy to clipboard".  You get three values:
#       name (your account), token, secret.
# 3. Paste them below (or, safer, run the setAccountInfo() line once in the
#    R console so the secret never lives in this file).
#
# Then run:   Rscript deploy.R
# =============================================================================

if (!file.exists("global.R")) setwd("C:/Users/chris/OneDrive/Masterarbeit/ShinyApp")
library(rsconnect)

# ---- 1) Account credentials (fill in once) ----------------------------------
ACCOUNT <- "YOUR_ACCOUNT_NAME"
TOKEN   <- "YOUR_TOKEN"
SECRET  <- "YOUR_SECRET"

if (ACCOUNT == "YOUR_ACCOUNT_NAME")
  stop("Fill in ACCOUNT / TOKEN / SECRET from your shinyapps.io token first.")

setAccountInfo(name = ACCOUNT, token = TOKEN, secret = SECRET)

# ---- 2) Only upload what the app actually needs ------------------------------
# (precompute.R / deploy.R / README are dev files -> excluded, so heavy build
#  deps like 'car' are not pulled onto the server.)
app_files <- c(
  "app.R",
  "global.R",
  "data/Datensatz_Masterarbeit_Alfter.xlsx",
  "data/results.rds"
)

# ---- 3) Deploy ---------------------------------------------------------------
deployApp(
  appDir    = ".",
  appFiles  = app_files,
  appName   = "temperature-health",          # -> https://ACCOUNT.shinyapps.io/temperature-health
  appTitle  = "Temperature & Health – DLNM thesis",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

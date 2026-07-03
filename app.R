# =============================================================================
# app.R  --  Interactive presentation of the master's thesis (DLNM)
# "Extreme temperatures and hospital admissions" -- Christian Alfter
#
# Run:  open the folder in R/RStudio and   shiny::runApp()
#       or:   Rscript -e "shiny::runApp('.', launch.browser=TRUE)"
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(ggplot2)

source("global.R")

thematic::thematic_shiny(font = "auto")

# If TRUE, visitors can download the raw daily dataset as CSV. Off by default
# because the app is deployed publicly (data = aggregated hospital admissions).
ALLOW_RAW_DOWNLOAD <- FALSE

# -----------------------------------------------------------------------------
# Theme
# -----------------------------------------------------------------------------
theme_app <- bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#0f1420", fg = "#e8ecf3",
  primary = "#e5484d", secondary = "#3b82f6",
  base_font = font_google("Inter"),
  heading_font = font_google("Space Grotesk"),
  "navbar-bg" = "#151b2b"
)

COL_HOT  <- "#e5484d"
COL_COLD <- "#3b82f6"

# Pretty-print an RR effect vector c(rr, lo, hi, ...)
fmt_rr <- function(v) {
  if (is.na(v["rr"])) return("–")
  sprintf("%.3f  (%.3f – %.3f)", v["rr"], v["lo"], v["hi"])
}
sig_badge <- function(p) {
  if (is.na(p)) return("")
  lab <- if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
  cls <- if (p < 0.05) "text-danger fw-bold" else "text-secondary"
  sprintf("<span class='%s'>%s</span>", cls, lab)
}

# DataTable helper: highlight significant p-value columns, offer CSV export
result_dt <- function(data, p_cols = NULL, round_cols = NULL) {
  dt <- datatable(
    data, rownames = FALSE, filter = "top", extensions = "Buttons",
    options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip",
                   buttons = list(list(extend = "csv", text = "Download CSV"))),
    class = "compact stripe hover"
  )
  if (!is.null(round_cols)) dt <- formatRound(dt, round_cols, 3)
  for (pc in p_cols) {
    dt <- formatStyle(dt, pc,
                      color = styleInterval(0.05, c("#ff6b6b", "#9aa4b2")),
                      fontWeight = styleInterval(0.05, c("bold", "normal")))
  }
  dt
}

# Contact / Impressum details (shown on the Contact tab). Fill in the address.
CONTACT_NAME    <- "Christian Alfter"
CONTACT_EMAIL   <- "christianheinrichalfter@gmail.com"
CONTACT_ADDRESS <- c("Im Ohndorf 12", "56204 Hillscheid")
CONTACT_PHONE   <- ""   # optional, e.g. "+49 ..." (leave "" to hide)
.mail_subject <- "Request: Master's thesis - Temperature & hospital admissions"
.mail_body <- paste0(
  "Hi Christian,\n\nI would be interested in reading your full master's thesis ",
  "on temperature and hospital admissions. Could you please send me the PDF?\n\nThanks!"
)
THESIS_MAILTO <- sprintf("mailto:%s?subject=%s&body=%s", CONTACT_EMAIL,
                         utils::URLencode(.mail_subject, reserved = TRUE),
                         utils::URLencode(.mail_body, reserved = TRUE))
request_button <- function(label = "Request the full thesis", size = "") {
  tags$a(href = THESIS_MAILTO,
         class = paste("btn btn-primary", size),
         icon("envelope"), " ", label)
}

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = "Temperature & Health",
  theme = theme_app,
  # Only the plot/table tabs fill the viewport height; the text-heavy Overview
  # and Methods tabs scroll naturally (otherwise each card gets its own scrollbar).
  fillable = c("Data", "Dose-response", "Lag profile", "Contour",
               "Shock vs. wave", "Results"),
  id = "nav",

  # ---- Overview -------------------------------------------------------------
  nav_panel(
    "Overview", icon = icon("book-open"),
    layout_column_wrap(
      width = 1, heights_equal = "row",
      card(card_body(
        class = "p-4",
        h2("Extreme temperatures and hospital admissions", class = "fw-bold"),
        p(class = "fs-5 text-secondary",
          "How do heat and cold affect the number of hospital admissions? ",
          "This app makes the Distributed-Lag Non-Linear Model (DLNM) analysis ",
          "of my master's thesis interactive."),
        p("Daily data ", strong("2014–2023"), " (", strong("3,651 days"),
          ") on cardiovascular, pulmonary and volume-depletion admissions, ",
          "linked to air temperature and dew point. The model separates three ",
          "effects: the ", strong("continuous"), " temperature effect (U-shape), ",
          strong("shock days"), " (first extreme day) and ", strong("wave days"),
          " (sustained extreme spell) – each for heat and cold."),
        div(class = "mt-3", request_button())
      )),
      card(
        card_header("Abstract"),
        card_body(
          p("The aim of this study is to examine the effects of heat waves and cold ",
            "spells in Germany between 2014 and 2023 on the rate of hospital admissions. ",
            "Stratification was performed according to disease group, age, gender, and ",
            "region type in order to identify vulnerable population groups. The data is ",
            "based on all hospital admissions documented by Destatis in Germany during ",
            "this period in the disease groups cardiovascular diseases, pulmonary diseases, ",
            "and volume-deficiency diseases. The temperature data was obtained from the ",
            "online portal of the German Weather Service."),
          p("A distributed lag non-linear model was used to model temperature-dependent ",
            "risks, taking into account both nonlinear relationships between temperature ",
            "and hospital admissions and delayed effects. Temperature extremes were defined ",
            "using percentile thresholds for the daily average temperature. Below the 5th ",
            "percentile, a day was considered an extremely cold day; above the 95th ",
            "percentile, an extremely hot day. Shock days were defined as the first day of ",
            "an extreme period or as an isolated extreme day. If there were two or more ",
            "consecutive extreme days, the day was defined as a wave day."),
          p("The results show differences between disease groups on shock and wave days. ",
            "Heat was associated with an increase in hospital admissions due to ",
            "volume-deficiency diseases, while cardiovascular diseases showed higher risks ",
            "in cold weather. When comparing age groups, younger people were more affected ",
            "by volume-deficiency diseases on extremely hot days than older people. The ",
            "gender-specific analysis showed an increased risk for men only in cases of ",
            "volume-deficiency disorders on cold shock days. When comparing region types, ",
            "the highest risks were found in urban regions on heat shock and heat wave days. ",
            "The analysis of the effects of shock days and wave days showed no significant ",
            "differences. This suggests that the duration of the extreme event plays a ",
            "lesser role than the temperature stress itself.")
        )
      ),
      layout_column_wrap(
        width = 1/3,
        value_box("Study period", "2014 – 2023", showcase = icon("calendar"),
                  theme = "secondary", p("10 years · 3,651 days")),
        value_box("Disease groups", "3 + Total", showcase = icon("heart-pulse"),
                  theme = "primary", p("Cardiovascular · Pulmonary · Volume depletion")),
        value_box("Outcomes analysed", "31", showcase = icon("layer-group"),
                  theme = "secondary", p("Region · Sex · Age"))
      ),
      card(card_body(
        class = "p-3",
        p(class = "mb-0",
          icon("flask"), " See the ", strong("Methods"),
          " tab for the full, reproducible methodology (model, data, ICD-10 codes, ",
          "confounders, formula).")
      ))
    )
  ),

  # ---- Methods (detailed, reproducible) -------------------------------------
  nav_panel(
    "Methods", icon = icon("flask"),
    div(
      class = "mx-auto", style = "max-width: 900px;",
      card(card_body(class = "p-4",
        h3("Methodology", class = "fw-bold"),
        p(class = "text-secondary",
          "A full, reproducible description of the data and the Distributed-Lag ",
          "Non-Linear Model (DLNM). Expand a section to read the details."),
        accordion(
          open = "Study design & hypotheses",

          accordion_panel(
            "Study design & hypotheses", icon = icon("clipboard-list"),
            p("Time-series study on ecological (population-level) data for Germany, ",
              "2014–2023. The association between extreme temperatures and hospital ",
              "admissions is modelled with a DLNM in R, stratified by disease group, ",
              "sex, age group and region type to identify vulnerable populations."),
            p(strong("Research question:"), " How do heat waves and cold spells in ",
              "Germany affect hospital admissions, and do these effects differ by ",
              "disease group, age, sex and region type?"),
            tags$ul(
              tags$li(strong("H1"), " – effects differ between disease groups."),
              tags$li(strong("H2"), " – people aged 65+ show stronger increases than 15–64."),
              tags$li(strong("H3"), " – there are sex-specific differences."),
              tags$li(strong("H4"), " – effects vary by region type."),
              tags$li(strong("H5"), " – sustained waves are associated with stronger ",
                      "increases than isolated shock days.")
            )
          ),

          accordion_panel(
            "Data sources", icon = icon("database"),
            p(strong("Health data."), " Daily aggregated inpatient admissions from the ",
              "DRG statistics of the German Federal Statistical Office (Destatis), ",
              "2014–2023, by admission date, all of Germany, stratified by age, sex and ",
              "region type. Only selected temperature-sensitive main diagnoses (ICD-10) ",
              "were included:"),
            tags$table(
              class = "table table-sm table-dark table-bordered small",
              tags$thead(tags$tr(tags$th("Group"), tags$th("ICD-10 codes"))),
              tags$tbody(
                tags$tr(tags$td(strong("Cardiovascular")),
                        tags$td("I10, I11, I12, I13, I15, I21, I22, I25, I46, I48, I50, ",
                                "I62, I63, I64, I67, I95")),
                tags$tr(tags$td(strong("Pulmonary")),
                        tags$td("J00–J06, J09–J18, J20, J43, J44, J45, J96")),
                tags$tr(tags$td(strong("Volume depletion")),
                        tags$td("E86, E87"))
              )
            ),
            p("Analysis restricted to ages ≥ 15 (children < 15, ~15 % of pulmonary ",
              "cases, excluded by recomputing group totals from the 15–64 and 65+ ",
              "subgroups; regional series could not be age-filtered). The final day ",
              "(31 Dec 2023) was dropped as a documentation artefact."),
            p(strong("Environmental data."), " Daily station-level data from the German ",
              "Weather Service (DWD). Exposure = daily mean 2 m air temperature; dew ",
              "point (from 24 hourly values) as humidity control. Both aggregated to ",
              "federal-state level and then to a population-weighted national mean.")
          ),

          accordion_panel(
            "Exposure: extreme, shock & wave days", icon = icon("temperature-half"),
            p("Extreme days are defined relative to the temperature distribution ",
              "(a broad WHO-recommended band): a day is an ", strong("extreme cold day"),
              " below the 5th percentile and an ", strong("extreme heat day"),
              " above the 95th percentile of daily mean temperature."),
            tags$ul(
              tags$li(strong("Shock day"), " – an isolated extreme day, or the first ",
                      "day of an extreme spell."),
              tags$li(strong("Wave day"), " – any extreme day from the second ",
                      "consecutive day onward (sustained thermal load).")
            ),
            p("This separates the effect of the sudden onset from that of cumulative ",
              "exposure, for heat and cold independently.")
          ),

          accordion_panel(
            "Outcomes & stratification", icon = icon("layer-group"),
            p("Outcome = daily count of admissions per group (cardiovascular, ",
              "pulmonary, volume depletion), plus an overall total. Each group is ",
              "analysed as a national total and stratified by:"),
            tags$ul(
              tags$li(strong("Sex"), " (men / women) × ", strong("age"),
                      " (15–64 / 65+) → subgroups per disease group."),
              tags$li(strong("Region type"), " by urbanisation of the admitting ",
                      "hospital: urban (01), suburban (02), rural (03) – no further ",
                      "sex/age split available regionally.")
            ),
            p("In total ", strong("31 outcomes"), " are modelled (each selectable in ",
              "the interactive tabs).")
          ),

          accordion_panel(
            "Confounders", icon = icon("sliders"),
            tags$ul(
              tags$li(strong("Dew point"), " – continuous natural spline (3 df), ",
                      "controls absolute humidity."),
              tags$li(strong("Day of week"), " – categorical (Mon–Sun), captures ",
                      "weekly admission patterns."),
              tags$li(strong("Public holidays"), " – binary indicator (fixed + movable ",
                      "holidays via the Easter/Computus algorithm)."),
              tags$li(strong("Long-term trend & season"), " – natural spline over ",
                      "calendar time with 7 degrees of freedom per year.")
            )
          ),

          accordion_panel(
            "DLNM model & formula", icon = icon("square-root-variable"),
            p("A Distributed Lag Non-Linear Model is a regression framework developed ",
              "for environmental time series (Gasparrini et al., 2010). It captures two ",
              "things within a single model: the non-linear relationship between ",
              "temperature and health, and the delayed effects that unfold over the days ",
              "after exposure."),
            p("Extreme temperatures rarely act only on the same day. Cold effects in ",
              "particular build up with a delay and persist for several days, while heat ",
              "tends to act almost immediately. The relationship between temperature and ",
              "risk is also non-linear, because both very high and very low temperatures ",
              "raise the risk of admission while moderate temperatures show almost none. ",
              "This produces the characteristic U-shaped or J-shaped risk curve shown in ",
              "the Dose-response tab."),
            p("The model represents both dimensions in a cross-basis, a two-dimensional ",
              "predictor that combines a spline over temperature with a spline over the ",
              "lag. Because the cross-basis enters an ordinary GLM, the model stays linear ",
              "in its parameters even though the temperature and health surface it ",
              "describes is non-linear."),
            tags$hr(),
            p("Here the cross-basis enters a ", strong("quasi-Poisson"), " GLM (log ",
              "link) to allow for overdispersion in the daily counts. Both the ",
              "exposure-response and the lag-response use natural cubic splines with ",
              "4 df; the maximum lag is 21 days."),
            p(class = "mb-1 small text-secondary", "Model equation:"),
            tags$div(class = "p-2 mb-3 rounded",
              style = "background:#0b0f1a; font-style:italic;",
              HTML("log(E[Y<sub>t</sub>]) = &alpha; + cb<sub>temp,t</sub> ",
                   "+ &beta;<sub>1</sub>·Shock<sub>heat,t</sub> ",
                   "+ &beta;<sub>2</sub>·Wave<sub>heat,t</sub> ",
                   "+ &beta;<sub>3</sub>·Shock<sub>cold,t</sub> ",
                   "+ &beta;<sub>4</sub>·Wave<sub>cold,t</sub><br>",
                   "&emsp;&emsp;+ f(t) + Weekday<sub>t</sub> + Holiday<sub>t</sub> ",
                   "+ s(DewPoint<sub>t</sub>)")),
            p(class = "mb-1 small text-secondary",
              "Exact R specification (as run in the analysis):"),
            tags$pre(class = "p-2 rounded", style = "background:#0b0f1a; white-space:pre-wrap;",
              tags$code(
"cb_temp <- crossbasis(lufttemp_avg, lag = 21,
                      argvar = list(fun = \"ns\", df = 4),
                      arglag = list(fun = \"ns\", df = 4))

glm(outcome ~ cb_temp + schocktag_heiss + wellentag_heiss +
              schocktag_kalt + wellentag_kalt +
              ns(datum, df = round(years) * 7) + wochentag + holiday +
              ns(tautemp_avg, knots = c(P10, P75, P90)),
    family = quasipoisson())"))
          ),

          accordion_panel(
            "Relative risks & MRT", icon = icon("wave-square"),
            p("The reference is the ", strong("minimum-risk temperature (MRT)"),
              " – the temperature with the lowest risk, found empirically within the ",
              "50th–94th percentile range. All relative risks (RR) are centred on it, ",
              "so the MRT adapts to each subgroup's vulnerability."),
            tags$ul(
              tags$li(strong("Continuous RR"), " – at P5 (cold) and P95 (heat); the ",
                      "cumulative risk over the whole 21-day lag window vs. the MRT."),
              tags$li(strong("Discrete RR"), " – from the shock/wave indicators; the ",
                      "extra risk from the event itself.")
            ),
            p("Because of the log link, the total RR of an extreme day is multiplicative: ",
              "RR", tags$sub("total"), " = RR", tags$sub("continuous"),
              " × RR", tags$sub("shock/wave"), ".")
          ),

          accordion_panel(
            "Statistical inference", icon = icon("vials"),
            tags$ul(
              tags$li(strong("H1–H4"), " (disease group, age, sex, region) are fitted as ",
                      "separate, independent models and compared with ", strong("Wald tests"),
                      " on the relevant coefficients."),
              tags$li(strong("H5"), " (shock vs. wave) uses an ", strong("F-test"),
                      " (linear hypothesis) because both coefficients come from the same ",
                      "model and are statistically dependent.")
            ),
            p("A p-value < 0.05 is treated as statistically significant. All test ",
              "results are in the ", strong("Results"), " tab.")
          ),

          accordion_panel(
            "Sensitivity analyses", icon = icon("shield-halved"),
            p("Robustness was checked by varying the DLNM parameters: temperature and ",
              "lag degrees of freedom (3–5) and maximum lag (14 / 21 / 28 days) – ",
              "81 alternative scenarios per main outcome. Each scenario's MRT, heat/cold ",
              "RRs and shock/wave RRs were compared with the main model by effect ",
              "direction, CI overlap and significance:"),
            tags$ul(
              tags$li("≥ 90 % agreement → highly robust"),
              tags$li("75–89 % → robust"),
              tags$li("< 75 % → not robust")
            )
          ),

          accordion_panel(
            "Software & reproducibility", icon = icon("code"),
            p("Analysis in ", strong("R"), " with the ", tags$code("dlnm"), " package ",
              "(cross-basis / crosspred), ", tags$code("splines"), " (natural splines), ",
              tags$code("mgcv"), ", ", tags$code("car"), " (F-tests) and ",
              tags$code("dplyr"), "/", tags$code("lubridate"), " for data handling."),
            p("This app re-implements the exact model pipeline: single outcomes are ",
              "fitted live from the same data and formula; the Results tables are ",
              "precomputed from all 31 fitted models."),
            div(class = "mt-3", request_button("Request the full thesis (PDF)"))
          )
        )
      ))
    )
  ),

  # ---- Data explorer --------------------------------------------------------
  nav_panel(
    "Data", icon = icon("chart-area"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Display",
        selectInput("explore_outcome", "Admission series",
                    choices = outcome_choices, selected = "gesamt"),
        checkboxInput("show_temp", "Overlay temperature", TRUE),
        sliderInput("explore_years", "Period", min = START_JAHR, max = END_JAHR,
                    value = c(START_JAHR, END_JAHR), step = 1, sep = "")
      ),
      navset_card_tab(
        nav_panel("Time series", plotlyOutput("ts_plot", height = "440px")),
        nav_panel("Extreme days per year", plotlyOutput("extreme_plot", height = "440px")),
        nav_panel("Descriptive statistics", DTOutput("desc_table")),
        nav_panel("Raw data",
                  if (ALLOW_RAW_DOWNLOAD)
                    div(class = "p-2", downloadButton("dl_data", "Download CSV",
                                                      class = "btn-sm btn-secondary mb-2")),
                  DTOutput("raw_table"))
      )
    )
  ),

  # ---- Dose-response --------------------------------------------------------
  nav_panel(
    "Dose-response", icon = icon("wave-square"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Model",
        selectInput("dr_outcome", "Outcome", choices = outcome_choices,
                    selected = "herz_gesamt"),
        sliderInput("dr_lag", "Maximum lag (days)", min = 7, max = 28,
                    value = 21, step = 1),
        helpText("The U-curve shows the relative risk across temperature, ",
                 "centred on the minimum-risk temperature (MRT).")
      ),
      layout_column_wrap(
        width = 1/3,
        value_box("MRT", textOutput("vb_mrt"), showcase = icon("temperature-half"),
                  theme = "secondary"),
        value_box("RR at heat (P95)", textOutput("vb_hot"),
                  showcase = icon("sun"), theme = "primary"),
        value_box("RR at cold (P5)", textOutput("vb_cold"),
                  showcase = icon("snowflake"), theme = value_box_theme(bg = COL_COLD))
      ),
      card(full_screen = TRUE,
           card_header(textOutput("dr_title", inline = TRUE)),
           plotlyOutput("dr_plot", height = "460px"))
    )
  ),

  # ---- Lag profile ----------------------------------------------------------
  nav_panel(
    "Lag profile", icon = icon("clock"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Model",
        selectInput("lag_outcome", "Outcome", choices = outcome_choices,
                    selected = "herz_gesamt"),
        helpText("How the risk from a single extreme day spreads over the ",
                 "following 0–21 days (the lag) – a horizontal slice through the ",
                 "contour surface at the cold (P5) and heat (P95) thresholds. ",
                 "Cold effects typically build up with a delay and persist; ",
                 "heat effects hit almost immediately.")
      ),
      card(full_screen = TRUE,
           card_header("Relative risk over lag time"),
           plotlyOutput("lag_plot", height = "500px"))
    )
  ),

  # ---- Contour --------------------------------------------------------------
  nav_panel(
    "Contour", icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Model",
        selectInput("ct_outcome", "Outcome", choices = outcome_choices,
                    selected = "herz_gesamt"),
        helpText("The surface shows the relative risk for each combination of ",
                 "temperature (x) and lag (y).")
      ),
      card(full_screen = TRUE,
           card_header("RR surface: temperature × lag"),
           plotlyOutput("ct_plot", height = "520px"))
    )
  ),

  # ---- Shock vs. wave -------------------------------------------------------
  nav_panel(
    "Shock vs. wave", icon = icon("bolt"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Model",
        selectInput("ef_outcome", "Outcome", choices = outcome_choices,
                    selected = "herz_gesamt"),
        helpText("Discrete effects: first extreme day (shock) vs. sustained ",
                 "spell (wave).")
      ),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Heat"), htmlOutput("ef_hot")),
        card(card_header("Cold"), htmlOutput("ef_cold"))
      ),
      card(card_header("Relative risk of shock / wave days"),
           plotlyOutput("ef_plot", height = "360px"))
    )
  ),

  # ---- Results tables -------------------------------------------------------
  nav_panel(
    "Results", icon = icon("table"),
    navset_card_tab(
      nav_panel("RR summary", DTOutput("tbl_summary")),
      nav_panel("Extreme heat (RR)", DTOutput("tbl_heat")),
      nav_panel("Extreme cold (RR)", DTOutput("tbl_cold")),
      nav_panel("Subgroup Wald tests", DTOutput("tbl_wald")),
      nav_panel("Shock vs. wave (F-test)", DTOutput("tbl_sw"))
    )
  ),

  # ---- Contact / Impressum --------------------------------------------------
  nav_panel(
    "Contact", icon = icon("address-card"),
    div(
      class = "mx-auto", style = "max-width: 780px;",
      layout_column_wrap(
        width = 1, heights_equal = "row",
        card(
          card_header("Contact"),
          card_body(
            p("This app presents the results of the master's thesis ",
              tags$em("“Extreme temperatures and hospital admissions”"), " by ",
              strong(CONTACT_NAME), "."),
            tags$ul(
              class = "list-unstyled",
              tags$li(icon("envelope"), " ",
                      tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL)),
              if (nzchar(CONTACT_PHONE))
                tags$li(icon("phone"), " ", CONTACT_PHONE)
            ),
            div(class = "mt-2", request_button("Request the full thesis (PDF)"))
          )
        ),
        card(
          card_header("Impressum"),
          card_body(
            p(class = "text-secondary small", "Angaben gemäß § 5 DDG (ehem. TMG)"),
            p(strong(CONTACT_NAME), tags$br(),
              HTML(paste(CONTACT_ADDRESS, collapse = "<br>"))),
            p(strong("Kontakt"), tags$br(),
              "E-Mail: ", tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL),
              if (nzchar(CONTACT_PHONE)) tagList(tags$br(), "Telefon: ", CONTACT_PHONE)),
            p(strong("Verantwortlich für den Inhalt nach § 18 Abs. 2 MStV"), tags$br(),
              CONTACT_NAME, " (Anschrift wie oben)"),
            tags$hr(),
            p(class = "small text-secondary mb-1", strong("Haftungsausschluss")),
            p(class = "small text-secondary",
              "Diese Anwendung stellt die Ergebnisse einer Masterarbeit zu ",
              "Informations- und Bildungszwecken dar. Sie ersetzt keine medizinische ",
              "oder politische Beratung. Für die Richtigkeit, Vollständigkeit und ",
              "Aktualität der Inhalte wird keine Haftung übernommen."),
            p(class = "small text-secondary mb-1", strong("Datenquellen")),
            p(class = "small text-secondary mb-0",
              "Krankenhausfälle: Statistisches Bundesamt (Destatis), DRG-Statistik. ",
              "Temperatur/Taupunkt: Deutscher Wetterdienst (DWD). ",
              "Die aggregierten Daten dienen ausschließlich wissenschaftlichen Zwecken.")
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Master's thesis · Christian Alfter"))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  fit_dr  <- reactive({ req(input$dr_outcome);  fit_outcome(input$dr_outcome, lag = input$dr_lag) })
  fit_lag <- reactive({ req(input$lag_outcome); fit_outcome(input$lag_outcome) })
  fit_ct  <- reactive({ req(input$ct_outcome);  fit_outcome(input$ct_outcome) })
  fit_ef  <- reactive({ req(input$ef_outcome);  fit_outcome(input$ef_outcome) })

  # ---- Data explorer -------------------------------------------------------
  explore_df <- reactive({
    df[df$year >= input$explore_years[1] & df$year <= input$explore_years[2], ]
  })

  output$ts_plot <- renderPlotly({
    d <- explore_df(); oc <- input$explore_outcome
    p <- plot_ly(d, x = ~datum)
    p <- add_lines(p, y = d[[oc]], name = outcome_label(oc),
                   line = list(color = COL_HOT, width = 1.2),
                   hovertemplate = "%{x|%d %b %Y}<br>%{y} cases<extra></extra>")
    if (isTRUE(input$show_temp)) {
      p <- add_lines(p, y = ~lufttemp_avg, name = "Temperature (°C)", yaxis = "y2",
                     line = list(color = COL_COLD, width = 0.8), opacity = 0.55,
                     hovertemplate = "%{y:.1f} °C<extra></extra>")
    }
    p %>% layout(
      hovermode = "x unified",
      yaxis  = list(title = paste0(outcome_label(oc), " (cases)")),
      yaxis2 = list(title = "Temperature (°C)", overlaying = "y", side = "right",
                    showgrid = FALSE),
      xaxis  = list(title = ""),
      legend = list(orientation = "h", y = 1.08), margin = list(r = 60)
    )
  })

  output$extreme_plot <- renderPlotly({
    d <- extreme_days_per_year
    plot_ly(d, x = ~Year) %>%
      add_bars(y = ~`Heat days`, name = "Heat days", marker = list(color = COL_HOT)) %>%
      add_bars(y = ~`Cold days`, name = "Cold days", marker = list(color = COL_COLD)) %>%
      layout(barmode = "group", xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Number of days"),
             legend = list(orientation = "h", y = 1.1))
  })

  output$desc_table <- renderDT(
    datatable(descriptives(), rownames = FALSE,
              options = list(dom = "t", pageLength = 20),
              class = "compact stripe hover")
  )

  output$raw_table <- renderDT({
    d <- explore_df()[, c("datum", "lufttemp_avg", "tautemp_avg", "gesamt",
                          "herz_gesamt", "pulmonal_gesamt", "volumenmangel_gesamt")]
    names(d) <- c("Date", "Air temp (°C)", "Dew point (°C)", "All cases",
                  "Cardiovascular", "Pulmonary", "Volume depletion")
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 12, scrollX = TRUE),
              class = "compact stripe hover") |>
      formatRound(c("Air temp (°C)", "Dew point (°C)"), 1)
  })

  if (ALLOW_RAW_DOWNLOAD) {
    output$dl_data <- downloadHandler(
      filename = function() "thesis_dataset.csv",
      content = function(file) write.csv(explore_df(), file, row.names = FALSE)
    )
  }

  # ---- Dose-response -------------------------------------------------------
  output$dr_title <- renderText(paste0("Dose-response – ", outcome_label(input$dr_outcome)))
  output$vb_mrt  <- renderText(sprintf("%.1f °C", fit_dr()$mrt))
  output$vb_hot  <- renderText(sprintf("%.3f", fit_dr()$rr_hot["rr"]))
  output$vb_cold <- renderText(sprintf("%.3f", fit_dr()$rr_cold["rr"]))

  output$dr_plot <- renderPlotly({
    f <- fit_dr(); c <- f$curve
    c$tt <- sprintf("%.1f °C (P%.0f)<br>RR %.3f (%.3f–%.3f)",
                    c$Temperature, c$Percentile, c$RR, c$CI_low, c$CI_high)
    p <- plot_ly(c, x = ~Temperature)
    p <- add_ribbons(p, ymin = ~CI_low, ymax = ~CI_high, line = list(width = 0),
                     fillcolor = "rgba(229,72,77,0.18)", name = "95% CI",
                     hoverinfo = "skip")
    p <- add_lines(p, y = ~RR, name = "RR", line = list(color = COL_HOT, width = 2.5),
                   text = ~tt, hovertemplate = "%{text}<extra></extra>")
    p %>% layout(
      shapes = list(
        list(type = "line", x0 = min(c$Temperature), x1 = max(c$Temperature),
             y0 = 1, y1 = 1, line = list(dash = "dash", color = "#9aa4b2", width = 1)),
        vline(f$mrt, "#22c55e"),
        vline(f$thresholds["kalt"], COL_COLD),
        vline(f$thresholds["heiss"], COL_HOT)
      ),
      annotations = list(
        anno(f$mrt, sprintf("MRT %.1f°C", f$mrt), "#22c55e"),
        anno(f$thresholds["kalt"], "P5", COL_COLD),
        anno(f$thresholds["heiss"], "P95", COL_HOT)
      ),
      xaxis = list(title = "Mean air temperature (°C)"),
      yaxis = list(title = "Relative risk (RR)"), showlegend = FALSE
    )
  })

  # ---- Lag profile ---------------------------------------------------------
  output$lag_plot <- renderPlotly({
    f <- fit_lag(); dat <- rbind(f$lag_hot, f$lag_cold)
    validate(need(!is.null(dat) && nrow(dat) > 0, "No lag profile available."))
    p <- plot_ly()
    for (lv in unique(dat$Schwelle)) {
      s <- dat[dat$Schwelle == lv, ]
      col  <- if (grepl("Heat", lv)) COL_HOT else COL_COLD
      rgba <- if (grepl("Heat", lv)) "rgba(229,72,77,0.18)" else "rgba(59,130,246,0.18)"
      p <- add_ribbons(p, data = s, x = ~Lag, ymin = ~CI_low, ymax = ~CI_high,
                       line = list(width = 0), fillcolor = rgba,
                       showlegend = FALSE, hoverinfo = "skip")
      p <- add_lines(p, data = s, x = ~Lag, y = ~RR, name = lv,
                     line = list(color = col, width = 2.5),
                     hovertemplate = paste0(lv, "<br>Lag %{x}: RR %{y:.3f}<extra></extra>"))
    }
    p %>% layout(
      shapes = list(list(type = "line", x0 = 0, x1 = max(dat$Lag), y0 = 1, y1 = 1,
                         line = list(dash = "dash", color = "#9aa4b2", width = 1))),
      xaxis = list(title = "Lag (days after exposure)"),
      yaxis = list(title = "Relative risk (RR vs. MRT)"),
      legend = list(orientation = "h", y = 1.1)
    )
  })

  # ---- Contour -------------------------------------------------------------
  output$ct_plot <- renderPlotly({
    f <- fit_ct(); d <- f$contour
    temps <- sort(unique(d$Temperature)); lags <- sort(unique(d$Lag))
    z <- matrix(d$RR, nrow = length(temps),
                dimnames = list(as.character(temps), as.character(lags)))
    z <- z[order(as.numeric(rownames(z))), order(as.numeric(colnames(z)))]
    # temperature on x, lag on y (transpose z: rows become lag, cols become temp).
    # Colours match the thesis: red = RR>1 (higher risk), blue = RR<1, white at
    # RR=1. Symmetric zmin/zmax around 1 keeps white exactly on the neutral value;
    # explicit contour start/end/size avoids plotly's autocontour hang on the
    # tiny RR range.
    rng  <- max(abs(range(z, na.rm = TRUE) - 1))
    size <- signif(rng / 7, 1)
    div_scale <- list(c(0, "#2166ac"), c(0.5, "#ffffff"), c(1, "#b2182b"))
    plot_ly(x = temps, y = lags, z = t(z), type = "contour",
            colorscale = div_scale, zmin = 1 - rng, zmax = 1 + rng,
            contours = list(start = 1 - rng, end = 1 + rng, size = size,
                            showlabels = TRUE),
            colorbar = list(title = "RR"),
            hovertemplate = "%{x:.1f} °C<br>Lag %{y}<br>RR %{z:.3f}<extra></extra>") %>%
      layout(xaxis = list(title = "Temperature (°C)"),
             yaxis = list(title = "Lag (days)"))
  })

  # ---- Shock vs. wave ------------------------------------------------------
  eff_card <- function(f, kind) {
    e <- f$effects
    sc <- e[[paste0("schock_", kind)]]; wv <- e[[paste0("welle_", kind)]]
    HTML(sprintf(
      "<div class='p-2'>
         <div class='mb-2'><span class='badge bg-secondary'>Shock day</span>
           <div class='fs-5'>%s %s</div></div>
         <div><span class='badge bg-secondary'>Wave day</span>
           <div class='fs-5'>%s %s</div></div>
       </div>",
      fmt_rr(sc), sig_badge(sc["p"]), fmt_rr(wv), sig_badge(wv["p"])))
  }
  output$ef_hot  <- renderUI(eff_card(fit_ef(), "heiss"))
  output$ef_cold <- renderUI(eff_card(fit_ef(), "kalt"))

  output$ef_plot <- renderPlotly({
    e <- fit_ef()$effects
    rows <- list(
      c("Heat · shock", e$schock_heiss, COL_HOT),
      c("Heat · wave",  e$welle_heiss,  COL_HOT),
      c("Cold · shock", e$schock_kalt,  COL_COLD),
      c("Cold · wave",  e$welle_kalt,   COL_COLD))
    d <- do.call(rbind, lapply(rows, function(r) data.frame(
      label = r[[1]], rr = as.numeric(r[2]), lo = as.numeric(r[3]),
      hi = as.numeric(r[4]), col = r[[5]], stringsAsFactors = FALSE)))
    d <- d[!is.na(d$rr), ]; d$label <- factor(d$label, levels = rev(d$label))
    plot_ly(d, y = ~label, x = ~rr, color = ~I(col), type = "scatter",
            mode = "markers", marker = list(size = 11),
            error_x = list(type = "data", symmetric = FALSE,
                           array = ~(hi - rr), arrayminus = ~(rr - lo),
                           color = "#9aa4b2"),
            hovertemplate = "%{y}<br>RR %{x:.3f}<extra></extra>") %>%
      layout(shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = -0.5,
                                y1 = nrow(d) - 0.5,
                                line = list(dash = "dash", color = "#9aa4b2"))),
             xaxis = list(title = "Relative risk (RR)"),
             yaxis = list(title = ""), showlegend = FALSE)
  })

  # ---- Results tables ------------------------------------------------------
  rt <- results_tables
  need_rt <- function() validate(need(!is.null(rt), "Run precompute.R to build the result tables."))

  output$tbl_summary <- renderDT({ need_rt(); result_dt(rt$rr_summary, p_cols = c("p heat", "p cold")) })
  output$tbl_heat    <- renderDT({ need_rt(); result_dt(rt$extreme_heat, p_cols = "p") })
  output$tbl_cold    <- renderDT({ need_rt(); result_dt(rt$extreme_cold, p_cols = "p") })
  output$tbl_wald    <- renderDT({ need_rt(); result_dt(rt$wald_tests, p_cols = "p value") })
  output$tbl_sw      <- renderDT({ need_rt(); result_dt(rt$shock_wave, p_cols = "p value") })
}

# -- plotly helpers (vertical line + annotation) ------------------------------
vline <- function(x, col) list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1,
                               yref = "paper", line = list(color = col, width = 1.5, dash = "dot"))
anno  <- function(x, txt, col) list(x = x, y = 1, yref = "paper", text = txt,
                                    showarrow = FALSE, yanchor = "bottom",
                                    font = list(color = col, size = 11))

shinyApp(ui, server)

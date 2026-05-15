################################################################################
#              CRICHTON GROUP ANALYSIS SUITE  --  app.R  (modular)
################################################################################
#
#  The orchestrator. ~140 lines instead of 7,452. Each tool lives in its own
#  R/mod_*.R file with its own namespace; this file's job is to declare
#  the navbar, source the modules' UIs into nav panels, and wire their
#  servers up.
#
#  Run locally:    shiny::runApp(".")
#  Deploy:         rsconnect::deployApp(".")  (same as before)
#
#  All packages, helper sources, and module sources live in global.R.
#  Shiny picks that up automatically at startup.
#
################################################################################

# -- UI -----------------------------------------------------------------------
ui <- bslib::page_navbar(
  title  = shiny::tags$span(
    shiny::tags$span("Crichton Group", style = "color: #00C2FF"),
    " Analysis Suite"),
  id     = "nav",
  theme  = bslib::bs_theme(
    version    = 5,
    bg         = "#080C14",
    fg         = "#E8F0FE",
    primary    = "#00C2FF",
    secondary  = "#1E2D45",
    success    = "#00E5A0",
    font_scale = 0.92
  ),
  header = shiny::tags$head(
    # CSS and JS now live as static files in www/
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    shiny::tags$script(src = "custom.js"),
    shinyjs::useShinyjs()
  ),
  collapsible = TRUE,

  # -- Tool tabs ------------------------------------------------------------
  # Each tab pulls its UI from the relevant module. Nothing else lives here.

  bslib::nav_panel("Home", icon = shiny::icon("house"),
                   home_ui("home")),

  bslib::nav_panel("\u00c4KTA Chromatography",
                   icon  = shiny::icon("chart-line"),
                   value = "AKTA",
                   akta_ui("akta")),

  bslib::nav_panel("BCA",
                   icon  = shiny::icon("flask"),
                   value = "BCA",
                   bca_ui("bca")),

  bslib::nav_panel("CPM Peak Picker",
                   icon  = shiny::icon("temperature-half"),
                   value = "CPM",
                   cpm_ui("cpm")),

  bslib::nav_panel("CPM Contour Plotting",
                   icon  = shiny::icon("border-all"),
                   value = "CPMCONTOUR",
                   cpm_contour_ui("cpm_contour")),

  bslib::nav_panel("CPM QC",
                   icon  = shiny::icon("magnifying-glass-chart"),
                   value = "CPMQC",
                   cpm_qc_ui("cpm_qc")),

  bslib::nav_panel("Gel Annotator",
                   icon  = shiny::icon("image"),
                   value = "GEL",
                   gel_ui("gel")),

  bslib::nav_panel("UCP1 Proton Conductance",
                   icon  = shiny::icon("vial"),
                   value = "UCP1",
                   ucp1_ui("ucp1")),

  # -- Cross-tool export item in the navbar ---------------------------------
  bslib::nav_spacer(),
  bslib::nav_item(export_ui("export"))
)


# -- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Wire up home-card navigation. Each card on the landing page writes its
  # target value (e.g. "BCA") to the `tool_select` input via JS; we react
  # here by calling nav_select() on the page_navbar. This is needed because
  # bslib's nav input is read-only - writing to it from the client does
  # not trigger a tab change.
  shiny::observeEvent(input$tool_select, {
    bslib::nav_select(id = "nav", selected = input$tool_select)
  }, ignoreInit = TRUE)

  # Each module returns a reactive() exposing its current results.
  # Capture those so the export module can collect them.
  bca_results         <- bca_server("bca")
  cpm_results         <- cpm_server("cpm")
  akta_results        <- akta_server("akta")
  gel_results         <- gel_server("gel")
  cpm_qc_results      <- cpm_qc_server("cpm_qc")
  cpm_contour_results <- cpm_contour_server("cpm_contour")
  ucp1_results        <- ucp1_server("ucp1")

  export_server("export", tools = list(
    bca         = bca_results,
    cpm         = cpm_results,
    akta        = akta_results,
    gel         = gel_results,
    cpm_qc      = cpm_qc_results,
    cpm_contour = cpm_contour_results,
    ucp1        = ucp1_results
  ))
}


# -- Run ----------------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)

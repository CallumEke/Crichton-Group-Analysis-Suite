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

  bslib::nav_panel("AKTA",
                   icon  = shiny::icon("chart-line"),
                   value = "AKTA",
                   akta_ui("akta")),

  bslib::nav_panel("BCA",
                   icon  = shiny::icon("flask"),
                   value = "BCA",
                   bca_ui("bca")),

  bslib::nav_panel("CPM Peak",
                   icon  = shiny::icon("temperature-half"),
                   value = "CPM",
                   cpm_ui("cpm")),

  bslib::nav_panel("CPM Contour",
                   icon  = shiny::icon("border-all"),
                   value = "CPMCONTOUR",
                   cpm_contour_ui("cpm_contour")),

  bslib::nav_panel("CPM QC",
                   icon  = shiny::icon("magnifying-glass-chart"),
                   value = "CPMQC",
                   cpm_qc_ui("cpm_qc")),

  bslib::nav_panel("Gel: Annotate",
                   icon  = shiny::icon("image"),
                   value = "GEL",
                   gel_ui("gel")),

  bslib::nav_panel("Proton Conductance",
                   icon  = shiny::icon("vial"),
                   value = "UCP1",
                   ucp1_ui("ucp1")),

  # Right-aligned Clear button. Each tool's server still owns the click;
  # we just provide a single navbar-level affordance that fires the
  # currently visible tool's clear via JS. See the observer in server().
  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::tags$button(
      id = "global_clear",
      class = "btn-clear-nav",
      onclick = "Shiny.setInputValue('nav_clear_click', Math.random(), {priority:'event'});",
      shiny::HTML("\U0001f504  Clear")
    )
  )
)


# -- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Defensive: re-set the upload size limit at server-function time as
  # well as in global.R. Some Shiny hosting environments evaluate global.R
  # in a way that doesn't propagate options() to the per-session R process,
  # so setting it again here ensures every connection uses the higher limit.
  options(shiny.maxRequestSize = 20 * 1024^2)

  # Wire up home-card navigation. Each card on the landing page writes its
  # target value (e.g. "BCA") to the `tool_select` input via JS; we react
  # here by calling nav_select() on the page_navbar. This is needed because
  # bslib's nav input is read-only - writing to it from the client does
  # not trigger a tab change.
  shiny::observeEvent(input$tool_select, {
    bslib::nav_select(id = "nav", selected = input$tool_select)
  }, ignoreInit = TRUE)

  # Global Clear button in the navbar. Each tool's actionButton(ns("clear"))
  # renders as <button id="<module_id>-clear">. We just dispatch a click on
  # the right one based on which tab is currently active. This lets us have
  # a single navbar-level Clear button instead of a per-page bar without
  # needing each module to be aware of the global button.
  shiny::observeEvent(input$nav_clear_click, {
    tab <- input$nav %||% ""
    target <- switch(tab,
      AKTA       = "akta-clear",
      BCA        = "bca-clear",
      CPM        = "cpm-clear",
      CPMCONTOUR = "cpm_contour-clear",
      CPMQC      = "cpm_qc-clear",
      GEL        = "gel-clear",
      UCP1       = "ucp1-clear",
      NULL
    )
    if (!is.null(target)) {
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.click();", target))
    }
  }, ignoreInit = TRUE)

  # Initialise each tool module. Each returns a reactive() exposing its
  # current results, but the cross-tool report bundler was removed in
  # favour of per-tool exports, so we don't capture those reactives here.
  bca_server("bca")
  cpm_server("cpm")
  akta_server("akta")
  gel_server("gel")
  cpm_qc_server("cpm_qc")
  cpm_contour_server("cpm_contour")
  ucp1_server("ucp1")
}


# -- Run ----------------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)

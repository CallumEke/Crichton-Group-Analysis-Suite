################################################################################
#  mod_home.R  --  Landing page (tool gallery)
################################################################################
#
#  The home tab is the only "UI-only" module - no server logic. It still
#  follows the module pattern so it can live in its own file. The card
#  onclick handlers use the (non-namespaced) top-level "nav" input which
#  is set on the page_navbar() at the app root.
#
################################################################################

home_ui <- function(id) {

  tool_card <- function(target, title, desc, icon_name, tags, accent = NULL) {
    style <- if (!is.null(accent)) paste0("--accent-col: ", accent, ";") else NULL
    # Use a dedicated input ("tool_select") rather than the navbar's "nav"
    # input, because bslib::page_navbar's nav input is read-only - writing
    # to it from JS doesn't trigger a tab change. The server picks up
    # tool_select via an observer and calls nav_select().
    # priority:"event" forces the input to fire even if the same card is
    # clicked twice in a row.
    shiny::div(
      class = "tool-card",
      style = style,
      onclick = sprintf(
        "Shiny.setInputValue('tool_select', '%s', {priority: 'event'})",
        target),
      shiny::div(class = "tool-icon", shiny::icon(icon_name)),
      shiny::div(class = "tool-title", title),
      shiny::div(class = "tool-desc", desc),
      lapply(tags, function(t) shiny::tags$span(t, class = "tool-tag"))
    )
  }

  shiny::tagList(
    shiny::div(class = "home-hero",
      shiny::h1("Crichton Group Analysis Suite"),
      shiny::p(paste("Automated analysis tools for the Crichton Group.",
                     "Upload your data, adjust parameters, and get",
                     "publication-ready results - no coding required."))
    ),

    shiny::fluidRow(
      shiny::column(3, tool_card("AKTA", "\u00c4KTA Chromatography",
        "Plot UNICORN 7 SEC/IEX/HIC exports. Overlay multiple runs, highlight fractions, and integrate peaks.",
        "chart-line", c(".csv", "UNICORN 7", "Peak Integration"),
        accent = "#FF7B47")),
      shiny::column(3, tool_card("BCA", "BCA Protein Quantification",
        "Analyse SoftMax Pro BCA assay exports. Fits a standard curve, calculates concentration, and reports total protein yield.",
        "flask", c(".xls", "SoftMax Pro", "Standard Curve"))),
      shiny::column(3, tool_card("CPMCONTOUR", "CPM Contour Plotting",
        "Average replicate dF/dT traces across conditions and visualise as a heatmap. Plot Tm vs. concentration on a log scale.",
        "border-all", c(".csv", "Prism Export", "Titre"),
        accent = "#00E5A0")),
      shiny::column(3, tool_card("CPM", "CPM Peak Picker",
        "Calculate Tm from RotorGene Q CPM exports via manual range or automatic peak detection.",
        "temperature-half", c(".csv", "RotorGene Q", "Tm")))
    ),
    shiny::fluidRow(
      shiny::column(3, tool_card("CPMQC", "CPM QC",
        "Compare two samples: raw fluorescence, dF/dT traces and Tm bar chart. Designed for -GDP vs +GDP QC runs.",
        "magnifying-glass-chart", c(".csv", "RotorGene Q", "QC"),
        accent = "#A78BFA")),
      shiny::column(3, tool_card("GEL", "Gel Annotator",
        "Label SDS-PAGE and Western blot gels with publication-ready formatting. Supports TIFF, PNG, and JPEG files.",
        "image", c(".tif", "Western Blot", "Publication"),
        accent = "#8B5CF6")),
      shiny::column(3, tool_card("UCP1", "UCP1 Proton Conductance",
        "Automated pre-processing of fluorimetric proton conductance assay data. Upload raw intensity traces for calibration and analysis.",
        "vial", c(".csv", "Fluorimetry", "UCP1"),
        accent = "#22C55E"))
    )
  )
}

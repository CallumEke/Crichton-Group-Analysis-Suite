################################################################################
#  mod_cpm_qc.R  --  CPM Quality Control (Shiny module)
################################################################################
#
#  Migrated from app_v10l.R:
#    UI:     lines 1218 - 1406
#    Server: lines 5226 - 5610  (simple mode)
#            lines 5614 - 5923  (multi-sample mode)
#
#  Two sub-modes selected by a JS-driven toggle:
#    - Simple: two samples (e.g. -GDP vs +GDP), custom labels & colours,
#              manual or auto Tm override, optional dTm bracket annotation
#    - Multi:  3-10 samples, automatic palette, automatic Tm
#
#  Three plots per mode: raw fluorescence, dF/dT, Tm bar chart.
#  Plot ggplots are built with a Prism-style white theme so they look right
#  when exported, then a `dark_overlay()` theme is layered on top for the
#  on-screen rendering.
#
################################################################################

# Palette for multi-sample mode (ColorBrewer Set1 + extras), up to 10
.MQC_PALETTE <- c("#E41A1C","#377EB8","#4DAF4A","#FF7F00","#984EA3",
                  "#A65628","#F781BF","#00CED1","#FFDB58","#555555")


# -- UI -----------------------------------------------------------------------
cpm_qc_ui <- function(id) {
  ns <- shiny::NS(id)

  .cpm_expected <- c("read_rotorgene_csv_full")
  if (!ensure_helper_loaded("tm_analysis_functions.R", .cpm_expected)) {
    return(missing_helper_warning("tm_analysis_functions.R", .cpm_expected))
  }

  # Build mode-toggle JS by hand. The two buttons each set the value and
  # swap the 'active' CSS class, mirroring the original. Note the namespaced
  # input name passed to Shiny.setInputValue.
  mode_input <- ns("mode")
  btn_simple <- ns("btn_simple")
  btn_multi  <- ns("btn_multi")

  js_simple <- sprintf(
    "Shiny.setInputValue('%s', 'simple', {priority: 'event'});
     document.getElementById('%s').classList.add('active');
     document.getElementById('%s').classList.remove('active');",
    mode_input, btn_simple, btn_multi)

  js_multi <- sprintf(
    "Shiny.setInputValue('%s', 'multi', {priority: 'event'});
     document.getElementById('%s').classList.add('active');
     document.getElementById('%s').classList.remove('active');",
    mode_input, btn_multi, btn_simple)

  cond_simple <- sprintf("input['%s'] == 'simple' || input['%s'] == null || input['%s'] == undefined",
                         mode_input, mode_input, mode_input)
  cond_multi  <- sprintf("input['%s'] == 'multi'", mode_input)

  shiny::tagList(
    shiny::div(class = "clear-button-container",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    # ---- Mode toggle ----------------------------------------------------
    shiny::div(style = "padding: 0 1rem 0.5rem;",
      shiny::div(class = "qc-mode-toggle",
        shiny::tags$button("Simple (2 samples)",
          id = btn_simple, class = "qc-mode-btn active", onclick = js_simple),
        shiny::tags$button("Multi-Sample (3\u201310)",
          id = btn_multi, class = "qc-mode-btn", onclick = js_multi)
      )
    ),

    # ---- SIMPLE MODE ----------------------------------------------------
    shiny::conditionalPanel(condition = cond_simple,
      shiny::fluidRow(
        shiny::column(4,
          lab_card(
            step_title(1, "Upload Data File"),
            shiny::fileInput(ns("file"), NULL, accept = ".csv",
                             buttonLabel = "Browse\u2026",
                             placeholder = "RotorGene Q CSV export"),
            shiny::uiOutput(ns("file_status"))
          ),
          lab_card(
            step_title(2, "Select Samples"),
            info_box("Select two samples to compare (e.g. -GDP and +GDP)."),
            shiny::uiOutput(ns("sample_a_ui")),
            shiny::uiOutput(ns("sample_b_ui"))
          ),
          lab_card(
            step_title(3, "Sample Labels & Colours"),
            shiny::fluidRow(
              shiny::column(8, shiny::textInput(ns("label_a"), "Label A", value = "-GDP")),
              shiny::column(4, shiny::div(class = "lbl", "Colour A"),
                .qc_colour_picker(ns("col_a"), "#E41A1C"))
            ),
            shiny::fluidRow(
              shiny::column(8, shiny::textInput(ns("label_b"), "Label B", value = "+GDP")),
              shiny::column(4, shiny::div(class = "lbl", "Colour B"),
                .qc_colour_picker(ns("col_b"), "#377EB8"))
            )
          ),
          lab_card(
            step_title(4, "Tm Values"),
            info_box("Auto-detected as the maximum of the dF/dT peak. Override manually if needed."),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("tm_a"), "Tm A (\u00b0C)",
                                                   value = NULL, step = 0.1)),
              shiny::column(6, shiny::numericInput(ns("tm_b"), "Tm B (\u00b0C)",
                                                   value = NULL, step = 0.1))
            ),
            shiny::uiOutput(ns("tm_auto_status")),
            shiny::br(),
            shiny::checkboxInput(ns("show_tm_labels"),
                                 "Show Tm values above bars", value = FALSE),
            shiny::checkboxInput(ns("show_dtm"),
                                 "Show \u0394T\u2098 annotation", value = FALSE)
          ),
          lab_card(
            step_title(5, "Plot Title"),
            shiny::textInput(ns("title"), NULL, placeholder = "e.g. NaOaUCP1 CPM QC"),
            shiny::div(class = "lab-card-title", "Plot Style"),
            shiny::numericInput(ns("linewidth"), "Line width",
                                value = 1.5, min = 0.5, max = 4, step = 0.25),
            shiny::br(),
            shiny::tags$button("\u2699  Advanced Plot Settings", class = "adv-toggle",
              onclick = sprintf("$('#%s').slideToggle(200)", ns("adv_panel"))),
            shiny::div(id = ns("adv_panel"), style = "display:none;",
              shiny::div(class = "settings-group",
                shiny::div(class = "settings-group-title", "Legend Position"),
                shiny::selectInput(ns("legend_pos"), NULL,
                  choices = c("Top right"="topright","Top left"="topleft",
                              "Bottom right"="bottomright","Bottom left"="bottomleft"),
                  selected = "topright", width = "100%")))
          ),
          lab_card(
            step_title(6, "Generate"),
            shiny::actionButton(ns("run"), "\u25b6  Generate QC Plots", class = "btn-run"),
            shiny::br(), shiny::br(),
            shiny::uiOutput(ns("download_buttons"))
          )
        ),
        shiny::column(8,
          shiny::uiOutput(ns("badges")),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f321\ufe0f  Raw Fluorescence"),
            shiny::uiOutput(ns("raw_placeholder")),
            shiny::plotOutput(ns("raw_plot"), height = "300px")
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4c8  dF/dT (unnormalised)"),
            shiny::plotOutput(ns("dfdt_plot"), height = "300px")
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4ca  Tm Comparison"),
            shiny::plotOutput(ns("tm_plot"), height = "320px")
          )
        )
      )
    ),

    # ---- MULTI MODE ----------------------------------------------------
    shiny::conditionalPanel(condition = cond_multi,
      shiny::fluidRow(
        shiny::column(4,
          lab_card(
            step_title(1, "Upload Data File"),
            info_box("Same file format as Simple mode. Re-use an already uploaded file by switching modes without re-uploading."),
            shiny::fileInput(ns("mqc_file"), NULL, accept = ".csv",
                             buttonLabel = "Browse\u2026",
                             placeholder = "RotorGene Q CSV export"),
            shiny::uiOutput(ns("mqc_file_status"))
          ),
          lab_card(
            step_title(2, "Select Samples"),
            info_box("Select 3\u201310 samples. Colours are assigned automatically from a qualitative palette."),
            shiny::uiOutput(ns("mqc_sample_select_ui"))
          ),
          lab_card(
            step_title(3, "Sample Labels"),
            info_box("Optionally override the default sample names for cleaner plots."),
            shiny::uiOutput(ns("mqc_labels_ui"))
          ),
          lab_card(
            step_title(4, "Plot Settings"),
            shiny::textInput(ns("mqc_title"), "Plot title",
                             placeholder = "e.g. HsUCP1 Multi-Ligand QC"),
            shiny::numericInput(ns("mqc_linewidth"), "Line width",
                                value = 1.5, min = 0.5, max = 4, step = 0.25)
          ),
          lab_card(
            step_title(5, "Generate"),
            shiny::actionButton(ns("mqc_run"), "\u25b6  Generate QC Plots", class = "btn-run"),
            shiny::br(), shiny::br(),
            shiny::uiOutput(ns("mqc_download_buttons"))
          )
        ),
        shiny::column(8,
          shiny::uiOutput(ns("mqc_badges")),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f321\ufe0f  Raw Fluorescence"),
            shiny::uiOutput(ns("mqc_raw_placeholder")),
            shiny::plotOutput(ns("mqc_raw_plot"), height = "300px")
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4c8  dF/dT (unnormalised)"),
            shiny::plotOutput(ns("mqc_dfdt_plot"), height = "300px")
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4ca  Tm Comparison"),
            shiny::plotOutput(ns("mqc_tm_plot"), height = "320px")
          )
        )
      )
    )
  )
}


# -- Server -------------------------------------------------------------------
cpm_qc_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State for both modes
    qc_data     <- shiny::reactiveVal(NULL)
    qc_results  <- shiny::reactiveVal(NULL)
    mqc_data    <- shiny::reactiveVal(NULL)
    mqc_results <- shiny::reactiveVal(NULL)

    # ---- Clear all (both modes) -----------------------------------------
    shiny::observeEvent(input$clear, {
      qc_data(NULL);  qc_results(NULL)
      mqc_data(NULL); mqc_results(NULL)
      for (i in c("file","mqc_file","label_a","label_b","title","mqc_title"))
        shinyjs::reset(i)
      shiny::updateNumericInput(session, "tm_a", value = NA)
      shiny::updateNumericInput(session, "tm_b", value = NA)
      shiny::showNotification("CPM QC data cleared", type = "message", duration = 2)
    })


    # =====================================================================
    # SIMPLE MODE
    # =====================================================================

    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      qc_results(NULL)
      tryCatch({
        qc_data(read_rotorgene_csv_full(input$file$datapath))
      }, error = function(e) qc_data(list(error = conditionMessage(e))))
    })

    output$file_status <- shiny::renderUI({
      shiny::req(qc_data())
      d <- qc_data()
      if (!is.null(d$error)) {
        status_pill("error", "Error loading file")
      } else {
        n <- length(d$sample_ids)
        status_pill("ready",
                    sprintf("%d sample%s loaded", n, if (n != 1) "s" else ""))
      }
    })

    sample_choices <- shiny::reactive({
      shiny::req(qc_data())
      d <- qc_data()
      if (!is.null(d$error)) return(NULL)
      stats::setNames(d$sample_ids,
                      paste0("[", d$sample_ids, "] ", d$sample_names))
    })

    output$sample_a_ui <- shiny::renderUI({
      shiny::req(sample_choices())
      shiny::selectInput(ns("sample_a"), "Sample A", choices = sample_choices())
    })

    output$sample_b_ui <- shiny::renderUI({
      shiny::req(sample_choices())
      ch  <- sample_choices()
      sel <- if (length(ch) >= 2) ch[2] else ch[1]
      shiny::selectInput(ns("sample_b"), "Sample B",
                         choices = ch, selected = sel)
    })

    # Auto-detect Tm from dF/dT max - updates the numeric inputs.
    output$tm_auto_status <- shiny::renderUI({
      shiny::req(qc_data(), input$sample_a, input$sample_b)
      d <- qc_data()
      get_auto_tm <- function(sid) {
        idx <- which(d$sample_ids == sid)
        if (!length(idx)) return(NA)
        dfdt  <- d$data[, idx[1]]
        if (!any(!is.na(dfdt))) return(NA)
        round(d$temperature[which.max(dfdt)], 2)
      }
      tm_a <- get_auto_tm(input$sample_a)
      tm_b <- get_auto_tm(input$sample_b)

      if (!is.na(tm_a)) shiny::updateNumericInput(session, "tm_a", value = tm_a)
      if (!is.na(tm_b)) shiny::updateNumericInput(session, "tm_b", value = tm_b)

      shiny::div(class = "status-pill ready", style = "margin-top:0.3rem;",
        shiny::div(class = "dot"),
        sprintf("Auto: A = %.1f\u00b0C  |  B = %.1f\u00b0C",
                if (is.na(tm_a)) 0 else tm_a,
                if (is.na(tm_b)) 0 else tm_b))
    })

    # ---- Generate plots --------------------------------------------------
    shiny::observeEvent(input$run, {
      shiny::req(qc_data(), input$sample_a, input$sample_b)
      d <- qc_data()
      if (!is.null(d$error)) {
        shiny::showNotification(d$error, type = "error"); return()
      }
      idx_a <- which(d$sample_ids == input$sample_a)[1]
      idx_b <- which(d$sample_ids == input$sample_b)[1]
      if (is.na(idx_a) || is.na(idx_b)) {
        shiny::showNotification("Could not find selected samples in file.",
                                type = "error"); return()
      }
      if (is.null(d$data) || ncol(d$data) < max(idx_a, idx_b)) {
        shiny::showNotification("dF/dT data matrix is missing or too narrow. Check file format.",
                                type = "error"); return()
      }
      if (is.null(d$raw_data) || nrow(d$raw_data) == 0 ||
          ncol(d$raw_data) < max(idx_a, idx_b)) {
        shiny::showNotification(paste0(
          "Raw fluorescence block could not be parsed. ",
          "Ensure this is a full RotorGene Q export containing both the raw ",
          "fluorescence section (~row 23) and the Melt analysis dF/dT section (~row 97)."),
          type = "error", duration = 15)
        return()
      }

      lbl_a <- if (nchar(trimws(input$label_a)) > 0) trimws(input$label_a)
               else d$sample_names[idx_a]
      lbl_b <- if (nchar(trimws(input$label_b)) > 0) trimws(input$label_b)
               else d$sample_names[idx_b]
      col_a <- if (!is.null(input$col_a) && nchar(input$col_a) > 0) input$col_a
               else "#E41A1C"
      col_b <- if (!is.null(input$col_b) && nchar(input$col_b) > 0) input$col_b
               else "#377EB8"
      lw    <- input$linewidth
      title <- trimws(input$title)

      auto_tm <- function(idx) round(d$temperature[which.max(d$data[, idx])], 2)
      tm_a <- if (!is.na(input$tm_a) && input$tm_a > 0) input$tm_a else auto_tm(idx_a)
      tm_b <- if (!is.na(input$tm_b) && input$tm_b > 0) input$tm_b else auto_tm(idx_b)

      raw_df  <- rbind(
        stats::na.omit(data.frame(Temperature = d$raw_temperature,
                                  Value       = d$raw_data[, idx_a],
                                  Sample      = lbl_a)),
        stats::na.omit(data.frame(Temperature = d$raw_temperature,
                                  Value       = d$raw_data[, idx_b],
                                  Sample      = lbl_b))
      )
      dfdt_df <- rbind(
        stats::na.omit(data.frame(Temperature = d$temperature,
                                  Value       = d$data[, idx_a],
                                  Sample      = lbl_a)),
        stats::na.omit(data.frame(Temperature = d$temperature,
                                  Value       = d$data[, idx_b],
                                  Sample      = lbl_b))
      )
      pal <- stats::setNames(c(col_a, col_b), c(lbl_a, lbl_b))

      # Resolve legend position from select input
      leg_choice <- input$legend_pos %||% "topright"
      leg_pos  <- switch(leg_choice,
        topright = c(0.98, 0.98), topleft = c(0.02, 0.98),
        bottomright = c(0.98, 0.02), bottomleft = c(0.02, 0.02),
        c(0.98, 0.98))
      leg_just <- switch(leg_choice,
        topright = c(1, 1), topleft = c(0, 1),
        bottomright = c(1, 0), bottomleft = c(0, 0), c(1, 1))

      p_raw  <- .qc_raw_plot(raw_df, pal, lw, title)  + .qc_prism_theme(leg_pos, leg_just)
      p_dfdt <- .qc_dfdt_plot(dfdt_df, pal, lw, title) + .qc_prism_theme(leg_pos, leg_just)

      # ---- Tm bar plot --------------------------------------------------
      show_tm_labels <- isTRUE(input$show_tm_labels)
      show_dtm       <- isTRUE(input$show_dtm)

      tm_df <- data.frame(
        Sample = factor(c(lbl_a, lbl_b), levels = c(lbl_a, lbl_b)),
        Tm     = c(tm_a, tm_b),
        x_num  = c(1, 2),
        label  = c(lbl_a, lbl_b),
        stringsAsFactors = FALSE
      )
      bar_width <- 0.4
      x_hi      <- if (show_dtm) 3.4 else 2.6

      p_tm <- ggplot2::ggplot(tm_df) +
        ggplot2::geom_rect(ggplot2::aes(xmin = x_num - bar_width/2,
                                        xmax = x_num + bar_width/2,
                                        ymin = 25, ymax = Tm, fill = label),
                           colour = "black", linewidth = 0.6, show.legend = FALSE) +
        ggplot2::geom_point(ggplot2::aes(x = x_num, y = Tm),
                            size = 2.5, colour = "black", shape = 16) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_x_continuous(breaks = c(1, 2),
                                    labels = c(lbl_a, lbl_b),
                                    limits = c(0.4, x_hi), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(25, 75),
                                    breaks = seq(25, 75, 10),
                                    expand = ggplot2::expansion(mult = c(0, 0.02))) +
        ggplot2::labs(title = if (nchar(title) > 0) paste(title, "Tm") else NULL,
                      x = NULL, y = "T\u2098 (\u00b0C)") +
        .qc_prism_theme(legend_pos = "none", legend_just = c(0.5, 0.5))

      if (show_tm_labels) {
        p_tm <- p_tm +
          ggplot2::geom_text(data = tm_df,
            ggplot2::aes(x = x_num, y = Tm + 0.8,
                         label = sprintf("%.1f\u00b0C", Tm),
                         colour = label),
            vjust = 0, size = 3.5, fontface = "bold",
            inherit.aes = FALSE) +
          ggplot2::scale_colour_manual(values = pal, guide = "none")
      }
      if (show_dtm) {
        dtm     <- tm_b - tm_a
        mid_y   <- (tm_a + tm_b) / 2
        x_line  <- 2.38
        x_label <- 2.46
        p_tm <- p_tm +
          ggplot2::annotate("segment", x = x_line, xend = x_line,
                            y = tm_a, yend = tm_b,
                            colour = "black", linewidth = 0.6) +
          ggplot2::annotate("segment", x = x_line - 0.05, xend = x_line,
                            y = tm_a, yend = tm_a,
                            colour = "black", linewidth = 0.6) +
          ggplot2::annotate("segment", x = x_line - 0.05, xend = x_line,
                            y = tm_b, yend = tm_b,
                            colour = "black", linewidth = 0.6) +
          ggplot2::annotate("text", x = x_label, y = mid_y,
                            label = sprintf("\u0394T\u2098 = %.1f\u00b0C", dtm),
                            hjust = 0, vjust = 0.5,
                            size = 3.5, fontface = "bold", colour = "black")
      }

      qc_results(list(p_raw  = p_raw,  p_dfdt = p_dfdt,  p_tm = p_tm,
                      tm_a   = tm_a,   tm_b   = tm_b,
                      lbl_a  = lbl_a,  lbl_b  = lbl_b,   title = title))
    })

    # ---- Simple mode outputs --------------------------------------------
    output$badges <- shiny::renderUI({
      shiny::req(qc_results()); r <- qc_results()
      shiny::fluidRow(
        shiny::column(6, result_badge(paste0(r$lbl_a, " Tm"),
                                      sprintf("%.2f \u00b0C", r$tm_a))),
        shiny::column(6, result_badge(paste0(r$lbl_b, " Tm"),
                                      sprintf("%.2f \u00b0C", r$tm_b), tone = "green"))
      )
    })

    output$raw_placeholder <- shiny::renderUI({
      if (is.null(qc_results()))
        plot_placeholder("\U0001f321\ufe0f",
          "Upload a file, select two samples and click Generate QC Plots")
    })

    output$raw_plot  <- shiny::renderPlot({ shiny::req(qc_results())
      .qc_dark_overlay(qc_results()$p_raw) },  bg = CG_PALETTE$bg_card)
    output$dfdt_plot <- shiny::renderPlot({ shiny::req(qc_results())
      .qc_dark_overlay(qc_results()$p_dfdt) }, bg = CG_PALETTE$bg_card)
    output$tm_plot   <- shiny::renderPlot({ shiny::req(qc_results())
      .qc_dark_overlay(qc_results()$p_tm) },   bg = CG_PALETTE$bg_card)

    output$download_buttons <- shiny::renderUI({
      shiny::req(qc_results())
      shiny::tagList(
        shiny::downloadButton(ns("dl_raw"),  "\u2193 PNG Fluorescence", class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_dfdt"), "\u2193 PNG dF/dT",        class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_tm"),   "\u2193 PNG Tm Chart",     class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_csv"),  "\u2193 CSV Summary",      class = "btn-download")
      )
    })

    .save_qc <- function(p, f, w = 5.5, h = 4.5)
      ggplot2::ggsave(f, p, width = w, height = h, dpi = 300, bg = "white")

    output$dl_raw  <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_QC_fluorescence", "png"),
      content  = function(f) .save_qc(qc_results()$p_raw,  f))
    output$dl_dfdt <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_QC_dFdT",      "png"),
      content  = function(f) .save_qc(qc_results()$p_dfdt, f))
    output$dl_tm   <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_QC_Tm",        "png"),
      content  = function(f) {
        w <- if (isTRUE(shiny::isolate(input$show_dtm))) 6 else 4
        .save_qc(qc_results()$p_tm, f, w = w, h = 5)
      })
    output$dl_csv  <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_QC", "csv"),
      content  = function(file) {
        r <- qc_results()
        utils::write.csv(data.frame(
          Label   = c(r$lbl_a, r$lbl_b),
          Tm_degC = c(r$tm_a, r$tm_b),
          dTm_degC = c(NA, r$tm_b - r$tm_a),
          Date    = Sys.time()
        ), file, row.names = FALSE)
      })


    # =====================================================================
    # MULTI-SAMPLE MODE
    # =====================================================================

    shiny::observeEvent(input$mqc_file, {
      shiny::req(input$mqc_file)
      mqc_results(NULL)
      tryCatch({
        mqc_data(read_rotorgene_csv_full(input$mqc_file$datapath))
      }, error = function(e) mqc_data(list(error = conditionMessage(e))))
    })

    output$mqc_file_status <- shiny::renderUI({
      shiny::req(mqc_data())
      d <- mqc_data()
      if (!is.null(d$error)) {
        status_pill("error", "Error loading file")
      } else {
        n <- length(d$sample_ids)
        status_pill("ready",
                    sprintf("%d sample%s loaded", n, if (n != 1) "s" else ""))
      }
    })

    mqc_sample_choices <- shiny::reactive({
      shiny::req(mqc_data())
      d <- mqc_data()
      if (!is.null(d$error)) return(NULL)
      stats::setNames(d$sample_ids,
                      paste0("[", d$sample_ids, "] ", d$sample_names))
    })

    output$mqc_sample_select_ui <- shiny::renderUI({
      shiny::req(mqc_sample_choices())
      ch    <- mqc_sample_choices()
      sel   <- as.character(ch[seq_len(min(length(ch), 10))])
      n_vis <- min(length(ch), 8)
      shiny::tagList(
        # Force the multi-select to autosize so users can see all options at once
        shiny::tags$style(shiny::HTML(sprintf(
          "#%s { height: auto !important; }", ns("samples")))),
        shiny::selectInput(ns("samples"), NULL, choices = ch, selected = sel,
                           multiple = TRUE, width = "100%",
                           selectize = FALSE, size = n_vis)
      )
    })

    output$mqc_labels_ui <- shiny::renderUI({
      shiny::req(mqc_data())
      d <- mqc_data()
      if (!is.null(d$error)) return(NULL)
      sids <- if (!is.null(input$samples) && length(input$samples) > 0)
                input$samples
              else
                as.character(d$sample_ids[seq_len(min(length(d$sample_ids), 10))])
      n    <- length(sids)
      cols <- .MQC_PALETTE[seq_len(n)]

      rows <- lapply(seq_len(n), function(i) {
        sid   <- sids[i]
        idx   <- which(d$sample_ids == sid)[1]
        dname <- if (!is.na(idx)) d$sample_names[idx] else sid
        shiny::fluidRow(style = "margin-bottom:4px;",
          shiny::column(2, shiny::div(style = sprintf(
            "width:1.1rem;height:1.1rem;border-radius:50%%;background:%s;margin-top:0.5rem;",
            cols[i]))),
          shiny::column(10,
            shiny::textInput(ns(paste0("mqc_lbl_", i)), NULL,
                             placeholder = dname, width = "100%"))
        )
      })
      shiny::tagList(rows)
    })

    # ---- Generate multi-sample plots ------------------------------------
    shiny::observeEvent(input$mqc_run, {
      shiny::req(mqc_data(), input$samples)
      d    <- mqc_data()
      sids <- input$samples
      n    <- length(sids)

      if (!is.null(d$error)) {
        shiny::showNotification(d$error, type = "error"); return()
      }
      if (n < 2)  { shiny::showNotification("Select at least 2 samples.", type = "warning"); return() }
      if (n > 10) { shiny::showNotification("Maximum 10 samples.",        type = "warning"); return() }

      cols  <- .MQC_PALETTE[seq_len(n)]
      title <- trimws(input$mqc_title)
      lw    <- input$mqc_linewidth

      labels <- sapply(seq_len(n), function(i) {
        ov  <- trimws(input[[paste0("mqc_lbl_", i)]] %||% "")
        idx <- which(d$sample_ids == sids[i])[1]
        if (nchar(ov) > 0) ov else d$sample_names[idx]
      })

      idxs <- sapply(sids, function(s) which(d$sample_ids == s)[1])
      if (any(is.na(idxs))) {
        shiny::showNotification("Could not locate all selected samples.",
                                type = "error"); return()
      }
      if (is.null(d$data) || ncol(d$data) < max(idxs)) {
        shiny::showNotification("dF/dT data matrix too narrow. Check file format.",
                                type = "error"); return()
      }
      if (is.null(d$raw_data) || ncol(d$raw_data) < max(idxs)) {
        shiny::showNotification("Raw fluorescence block missing. Ensure this is a full RotorGene Q export.",
                                type = "error", duration = 10); return()
      }

      pal <- stats::setNames(cols, labels)

      raw_df <- do.call(rbind, lapply(seq_len(n), function(i)
        stats::na.omit(data.frame(Temperature = d$raw_temperature,
                                  Value       = d$raw_data[, idxs[i]],
                                  Sample      = labels[i]))))
      raw_df$Sample <- factor(raw_df$Sample, levels = labels)

      dfdt_df <- do.call(rbind, lapply(seq_len(n), function(i)
        stats::na.omit(data.frame(Temperature = d$temperature,
                                  Value       = d$data[, idxs[i]],
                                  Sample      = labels[i]))))
      dfdt_df$Sample <- factor(dfdt_df$Sample, levels = labels)

      tms <- sapply(idxs, function(idx)
        round(d$temperature[which.max(d$data[, idx])], 2))
      names(tms) <- labels

      p_raw  <- .qc_raw_plot(raw_df, pal, lw, title)   + .qc_prism_theme()
      p_dfdt <- .qc_dfdt_plot(dfdt_df, pal, lw, title) + .qc_prism_theme()

      bar_width <- max(0.25, min(0.5, 2.5 / n))
      tm_df <- data.frame(Sample = factor(labels, levels = labels),
                          Tm = tms, x_num = seq_len(n),
                          stringsAsFactors = FALSE)

      p_tm <- ggplot2::ggplot(tm_df) +
        ggplot2::geom_rect(ggplot2::aes(xmin = x_num - bar_width/2,
                                        xmax = x_num + bar_width/2,
                                        ymin = 25, ymax = Tm, fill = Sample),
                           colour = "black", linewidth = 0.5, show.legend = FALSE) +
        ggplot2::geom_point(ggplot2::aes(x = x_num, y = Tm),
                            size = 2, colour = "black", shape = 16) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_x_continuous(breaks = seq_len(n), labels = labels,
                                    limits = c(0.4, n + 0.6), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(25, 75), breaks = seq(25, 75, 10),
                                    expand = ggplot2::expansion(mult = c(0, 0.02))) +
        ggplot2::labs(title = if (nchar(title) > 0) paste(title, "Tm") else NULL,
                      x = NULL, y = "T\u2098 (\u00b0C)") +
        .qc_prism_theme(legend_pos = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = if (n > 4) 35 else 0,
          hjust = if (n > 4) 1 else 0.5))

      mqc_results(list(p_raw = p_raw, p_dfdt = p_dfdt, p_tm = p_tm,
                       tms = tms, labels = labels, title = title, n = n))
    })

    output$mqc_badges <- shiny::renderUI({
      shiny::req(mqc_results())
      r    <- mqc_results()
      cols <- .MQC_PALETTE[seq_len(r$n)]
      badge_cols <- lapply(seq_len(r$n), function(i) {
        shiny::column(max(2, floor(12 / r$n)),
          shiny::div(class = "result-badge",
            shiny::div(class = "result-label",
              shiny::div(style = sprintf(
                "display:inline-block;width:0.6rem;height:0.6rem;border-radius:50%%;background:%s;margin-right:0.3rem;vertical-align:middle;",
                cols[i])),
              r$labels[i]),
            shiny::div(class = "result-value", style = "font-size:1.1rem;",
              sprintf("%.2f\u00b0C", r$tms[i]))))
      })
      do.call(shiny::fluidRow, badge_cols)
    })

    output$mqc_raw_placeholder <- shiny::renderUI({
      if (is.null(mqc_results()))
        plot_placeholder("\U0001f321\ufe0f",
          "Upload a file, select samples and click Generate QC Plots")
    })

    output$mqc_raw_plot  <- shiny::renderPlot({ shiny::req(mqc_results())
      .qc_dark_overlay(mqc_results()$p_raw) },  bg = CG_PALETTE$bg_card)
    output$mqc_dfdt_plot <- shiny::renderPlot({ shiny::req(mqc_results())
      .qc_dark_overlay(mqc_results()$p_dfdt) }, bg = CG_PALETTE$bg_card)
    output$mqc_tm_plot   <- shiny::renderPlot({ shiny::req(mqc_results())
      .qc_dark_overlay(mqc_results()$p_tm) },   bg = CG_PALETTE$bg_card)

    output$mqc_download_buttons <- shiny::renderUI({
      shiny::req(mqc_results())
      shiny::tagList(
        shiny::downloadButton(ns("mqc_dl_raw"),  "\u2193 PNG Fluorescence", class = "btn-download"), " ",
        shiny::downloadButton(ns("mqc_dl_dfdt"), "\u2193 PNG dF/dT",        class = "btn-download"), " ",
        shiny::downloadButton(ns("mqc_dl_tm"),   "\u2193 PNG Tm Chart",     class = "btn-download"), " ",
        shiny::downloadButton(ns("mqc_dl_csv"),  "\u2193 CSV Summary",      class = "btn-download")
      )
    })

    .save_mqc <- function(p, f, w = 6.5, h = 4.5)
      ggplot2::ggsave(f, p, width = w, height = h, dpi = 300, bg = "white")

    output$mqc_dl_raw  <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_multiQC_fluorescence", "png"),
      content  = function(f) .save_mqc(mqc_results()$p_raw,  f))
    output$mqc_dl_dfdt <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_multiQC_dFdT", "png"),
      content  = function(f) .save_mqc(mqc_results()$p_dfdt, f))
    output$mqc_dl_tm   <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_multiQC_Tm", "png"),
      content  = function(f) {
        r <- mqc_results()
        .save_mqc(r$p_tm, f, w = max(4, 1.2 * r$n), h = 5)
      })
    output$mqc_dl_csv  <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_multiQC", "csv"),
      content  = function(file) {
        r   <- mqc_results()
        tms <- r$tms
        utils::write.csv(data.frame(
          Label    = names(tms),
          Tm_degC  = as.numeric(tms),
          dTm_degC = as.numeric(tms) - as.numeric(tms[1]),
          Date     = Sys.time()
        ), file, row.names = FALSE)
      })

    # ---- Public reactive ------------------------------------------------
    shiny::reactive({
      simple <- qc_results()
      multi  <- mqc_results()
      if (is.null(simple) && is.null(multi)) return(NULL)
      list(simple = simple, multi = multi)
    })
  })
}


# -- Internal helpers --------------------------------------------------------

# Inline colour picker (HTML5 <input type="color"> wired up via JS to a
# normal text input that R can read).
.qc_colour_picker <- function(text_id, default_hex) {
  picker_id <- paste0(text_id, "_picker")
  shiny::div(style = "display:flex;align-items:center;gap:0.4rem;",
    shiny::tags$input(
      id      = picker_id,
      type    = "color",
      value   = default_hex,
      style   = paste("width:2.2rem;height:2.2rem;border:1px solid #1E2D45;",
                      "border-radius:6px;background:transparent;cursor:pointer;",
                      "padding:0.1rem;flex-shrink:0;"),
      onchange = sprintf("Shiny.setInputValue('%s', this.value);", text_id),
      oninput  = sprintf("Shiny.setInputValue('%s', this.value);", text_id)),
    shiny::textInput(text_id, NULL, value = default_hex, width = "90px")
  )
}

# Shared white/Prism theme used for plots before they're dark-overlay'd
# for on-screen rendering. Centralising this avoids redefining it in both
# modes.
.qc_prism_theme <- function(legend_pos  = c(0.98, 0.98),
                            legend_just = c(1, 1),
                            base = 12) {
  ggplot2::theme_classic(base_size = base) +
    ggplot2::theme(
      plot.title           = ggplot2::element_text(face = "bold",
                                                   size = base + 2, hjust = 0),
      axis.title           = ggplot2::element_text(face = "bold", size = base),
      axis.text            = ggplot2::element_text(size = base - 1, colour = "black"),
      axis.line            = ggplot2::element_line(colour = "black", linewidth = 0.7),
      axis.ticks           = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.ticks.length    = ggplot2::unit(0.18, "cm"),
      legend.position      = legend_pos,
      legend.justification = legend_just,
      legend.background    = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key           = ggplot2::element_rect(fill = NA, colour = NA),
      legend.key.size      = ggplot2::unit(0.9, "lines"),
      legend.text          = ggplot2::element_text(size = base - 1),
      legend.title         = ggplot2::element_blank(),
      panel.grid           = ggplot2::element_blank(),
      plot.background      = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background     = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin          = ggplot2::margin(12, 16, 10, 10)
    )
}

# Dark theme overlay - applied for on-screen rendering only. Exported plots
# keep the white prism theme.
.qc_dark_overlay <- function(p) {
  p + ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
    panel.background = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
    axis.line        = ggplot2::element_line(colour = CG_PALETTE$muted),
    axis.ticks       = ggplot2::element_line(colour = CG_PALETTE$muted),
    axis.text        = ggplot2::element_text(colour = CG_PALETTE$muted),
    axis.title       = ggplot2::element_text(colour = CG_PALETTE$txt),
    plot.title       = ggplot2::element_text(colour = CG_PALETTE$txt),
    legend.text      = ggplot2::element_text(colour = CG_PALETTE$txt),
    panel.grid       = ggplot2::element_blank()
  )
}

# Shared raw fluorescence plot builder
.qc_raw_plot <- function(raw_df, pal, lw, title) {
  ggplot2::ggplot(raw_df, ggplot2::aes(x = Temperature, y = Value, colour = Sample)) +
    ggplot2::geom_line(linewidth = lw) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_x_continuous(limits = c(25, 90), breaks = seq(25, 90, 5),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                                expand = c(0, 0)) +
    ggplot2::labs(title = if (nchar(title) > 0) paste(title, "FU") else NULL,
                  x = "Temperature (\u00b0C)", y = "Fluorescence (a.u.)")
}

# Shared dF/dT plot builder
.qc_dfdt_plot <- function(dfdt_df, pal, lw, title) {
  ggplot2::ggplot(dfdt_df, ggplot2::aes(x = Temperature, y = Value, colour = Sample)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = lw) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_x_continuous(limits = c(25.5, 89.5),
                                breaks = seq(30, 85, 10), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.12))) +
    ggplot2::labs(title = if (nchar(title) > 0) paste(title, "dF/dT") else NULL,
                  x = "Temperature (\u00b0C)", y = "dF/dT")
}

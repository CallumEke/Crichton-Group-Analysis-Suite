################################################################################
#  mod_akta.R  --  AKTA Chromatography (Shiny module)
################################################################################
#
#  Migrated from app_v10l.R:
#    UI:     lines  786 -  895
#    Server: lines 6893 - 7340
#
#  The plot_akta_improved() function is provided by inst/analytics/plot_akta_improved.R.
#  parse_akta_uv_trace() is internal to this module - it's only used by the
#  integration reactive and the CSV exporter.
#
################################################################################

# -- UI -----------------------------------------------------------------------
akta_ui <- function(id) {
  ns <- shiny::NS(id)

  .akta_expected <- c("plot_akta_improved")
  if (!ensure_helper_loaded("plot_akta_improved.R", .akta_expected)) {
    return(missing_helper_warning("plot_akta_improved.R", .akta_expected))
  }

  shiny::tagList(
    shiny::div(class = "clear-button-container",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    shiny::fluidRow(
      # ----- Left column: controls (1-7) ----------------------------------
      shiny::column(4,
        lab_card(
          step_title(1, "Upload Data File(s)"),
          info_box("Upload one file for a single run, or multiple to overlay traces."),
          shiny::fileInput(ns("files"), NULL, accept = ".csv", multiple = TRUE,
                           buttonLabel = "Browse\u2026",
                           placeholder = "UNICORN 7 CSV export(s)"),
          shiny::uiOutput(ns("file_status"))
        ),

        lab_card(
          step_title(2, "Custom Sample Names"),
          info_box("Override filenames in the legend. One per line, same order as files."),
          shiny::textAreaInput(ns("custom_names"), NULL, rows = 3,
                               placeholder = "e.g.\nWT protein\nMutant R151A")
        ),

        lab_card(
          step_title(3, "Volume Range"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("vol_min"), "From (mL)",
                                                 value = NA, min = 0, step = 1)),
            shiny::column(6, shiny::numericInput(ns("vol_max"), "To (mL)",
                                                 value = NA, min = 0, step = 1))
          )
        ),

        lab_card(
          step_title(4, "Display Options"),
          shiny::checkboxInput(ns("show_fractions"), "Show fraction markers",  value = TRUE),
          shiny::checkboxInput(ns("show_cond"),      "Show conductance trace", value = FALSE),
          shiny::checkboxInput(ns("show_uv260"),     "Show UV 260 nm trace",   value = FALSE),
          shiny::checkboxInput(ns("show_pctb"),      "Show % Buffer B trace",  value = FALSE),
          shiny::br(),
          shiny::div(class = "lbl", "Highlight fractions"),
          shiny::div(style = "font-size:0.72rem;color:var(--muted);margin-bottom:0.4rem;",
            "Single (B8), list (B8,B9,B10), or range (A4-A8)."),
          # Main highlight row: textbox + colour picker. This is what most
          # users will use - it covers the "one colour, some fractions" case.
          shiny::fluidRow(
            shiny::column(7, shiny::textInput(ns("highlight"), NULL,
                                              placeholder = "e.g. B8,B9,B10")),
            shiny::column(5, colour_picker_inline(ns("highlight_colour"),
                                                  "#FFD93D", text_width = "70px"))
          ),
          # Collapsible panel for additional groups, each with its own colour.
          # Hidden by default; opening it doesn't grow the page much.
          shiny::tags$button("\u2699  Multiple highlight colours", class = "adv-toggle",
            onclick = sprintf("$('#%s').slideToggle(200)", ns("hl_adv_panel"))),
          shiny::div(id = ns("hl_adv_panel"), style = "display:none;",
            shiny::div(class = "settings-group",
              shiny::div(class = "settings-group-title", "Additional highlight groups"),
              shiny::div(style = "font-size:0.72rem;color:var(--muted);margin-bottom:0.5rem;",
                "Each row uses its own colour. Combines with the main highlight above."),
              .akta_hl_group_row(ns, 2, "#FF5C5C"),
              .akta_hl_group_row(ns, 3, "#00C2FF"),
              .akta_hl_group_row(ns, 4, "#00E5A0")
            )
          )
        ),

        lab_card(
          step_title(5, "Peak Integration"),
          info_box(paste("Enter volume boundaries to integrate the UV trace.",
                         "Reports area, purity, centroid elution volume, and estimated MW.")),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("int_start"), "From (mL)",
                                                 value = NA, min = 0, step = 0.1)),
            shiny::column(6, shiny::numericInput(ns("int_end"),   "To (mL)",
                                                 value = NA, min = 0, step = 0.1))
          ),
          shiny::tags$button("\u2699  Column Calibration", class = "adv-toggle",
            onclick = sprintf("$('#%s').slideToggle(200)", ns("calib_panel"))),
          shiny::div(id = ns("calib_panel"), style = "display:none;",
            shiny::div(class = "settings-group",
              shiny::div(class = "settings-group-title", "SEC Column Calibration"),
              shiny::div(style = "font-size:0.75rem;color:var(--muted);margin-bottom:0.5rem;",
                "Defaults calibrated for your column. Update if using a different column."),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("void_vol"),  "Void vol. (mL)",
                                                     value = 8.23,  min = 0, step = 0.01)),
                shiny::column(6, shiny::numericInput(ns("total_vol"), "Total vol. (mL)",
                                                     value = 24.00, min = 0, step = 0.01))
              ),
              shiny::div(style = "font-size:0.72rem;color:var(--muted);",
                "MW = 10^(\u22123.2245 \u00d7 (Ve\u2212Void)/(Total\u2212Void) + 5.9275)")
            )
          ),
          shiny::br(),
          shiny::uiOutput(ns("integration_result")),
          shiny::br(),
          shiny::uiOutput(ns("integration_actions"))
        ),

        lab_card(
          step_title(6, "Plot Style"),
          shiny::selectInput(ns("theme"), "Theme",
            choices = c("Publication" = "publication",
                        "Presentation" = "presentation",
                        "Minimal" = "minimal"),
            selected = "publication"),
          shiny::numericInput(ns("linewidth"), "Line width",
                              value = 1.2, min = 0.5, max = 4, step = 0.2)
        ),

        lab_card(
          step_title(7, "Plot"),
          shiny::actionButton(ns("run"), "\u25b6  Generate Plot", class = "btn-run"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("download_buttons"))
        )
      ),

      # ----- Right column: plot + file list + history ---------------------
      shiny::column(8,
        lab_card(
          shiny::div(class = "lab-card-title", "\U0001f4c8  Chromatography Profile"),
          shiny::uiOutput(ns("plot_placeholder")),
          shiny::plotOutput(ns("plot"), height = "500px")
        ),
        shiny::uiOutput(ns("file_list_ui")),
        lab_card(
          shiny::div(class = "lab-card-title", "\U0001f551  Session History"),
          shiny::uiOutput(ns("history_ui"))
        )
      )
    )
  )
}


# -- Server -------------------------------------------------------------------
akta_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    akta_results    <- shiny::reactiveVal(NULL)
    akta_history    <- shiny::reactiveVal(list())
    akta_annotation <- shiny::reactiveVal(NULL)

    # ---- Clear -----------------------------------------------------------
    shiny::observeEvent(input$clear, {
      akta_results(NULL)
      akta_history(list())
      akta_annotation(NULL)
      for (i in c("files", "custom_names", "vol_min", "vol_max",
                  "highlight", "int_start", "int_end")) shinyjs::reset(i)
      shiny::showNotification("\u00c4KTA data cleared", type = "message", duration = 2)
    })

    # ---- File-status pill ------------------------------------------------
    output$file_status <- shiny::renderUI({
      shiny::req(input$files)
      n <- nrow(input$files)
      status_pill("ready",
                  sprintf("%d file%s loaded", n, if (n > 1) "s" else ""))
    })

    output$file_list_ui <- shiny::renderUI({
      shiny::req(input$files)
      if (nrow(input$files) <= 1) return(NULL)
      lab_card(
        shiny::div(class = "lab-card-title", "\U0001f4c1  Loaded files (overlay order)"),
        lapply(seq_len(nrow(input$files)), function(i) {
          shiny::div(style = "display:flex;align-items:center;gap:.6rem;padding:.3rem 0;",
            shiny::tags$span(
              style = paste("font-family:'JetBrains Mono',monospace;",
                            "font-size:.72rem;background:#161E2E;color:#00C2FF;",
                            "padding:.1rem .4rem;border-radius:4px;"),
              as.character(i)),
            shiny::tags$span(input$files$name[i],
                             style = "font-size:.83rem;color:#E8F0FE;")
          )
        })
      )
    })

    output$plot_placeholder <- shiny::renderUI({
      if (is.null(akta_results()))
        plot_placeholder("\U0001f4ca", "Upload file(s) and click Generate Plot")
    })

    # ---- Generate plot ---------------------------------------------------
    shiny::observeEvent(input$run, {
      shiny::req(input$files)
      akta_results(NULL)

      # Capture the raw user inputs now; resolve them against the file's
      # fraction table later (after we've actually read it). Resolution
      # is file-aware so range endpoints can land on any dispenser pattern.
      highlight_inputs <- list(
        list(spec = input$highlight,
             colour = input$highlight_colour %||% "#FFD93D"),
        list(spec = input$hl_g2,
             colour = input$hl_g2_colour     %||% "#FF5C5C"),
        list(spec = input$hl_g3,
             colour = input$hl_g3_colour     %||% "#00C2FF"),
        list(spec = input$hl_g4,
             colour = input$hl_g4_colour     %||% "#00E5A0")
      )

      vol_range <- NULL
      if (!is.na(input$vol_min) && !is.na(input$vol_max))
        vol_range <- c(input$vol_min, input$vol_max)

      # Rename temp files to original names so the plot legend reads
      # "WT_protein" not "0a4f3b.csv" - plot_akta_improved() uses filenames.
      renamed <- file.path(
        dirname(input$files$datapath),
        tools::file_path_sans_ext(input$files$name)
      )
      for (i in seq_along(renamed))
        if (!file.exists(renamed[i]))
          file.copy(input$files$datapath[i], renamed[i])

      # Custom sample-name overrides (one per line)
      cnames_raw <- trimws(input$custom_names)
      if (nchar(cnames_raw) > 0) {
        cnames <- trimws(strsplit(cnames_raw, "\n")[[1]])
        cnames <- cnames[nchar(cnames) > 0]
        for (i in seq_along(cnames)) {
          if (i <= length(renamed)) {
            new_path <- file.path(dirname(renamed[i]), cnames[i])
            if (!file.exists(new_path)) file.copy(renamed[i], new_path)
            renamed[i] <- new_path
          }
        }
      }

      shiny::withProgress(message = "Generating plot\u2026", value = 0, {
        tryCatch({
          shiny::incProgress(0.4, detail = "Reading files\u2026")
          p <- plot_akta_improved(
            files               = renamed,
            volume_range        = vol_range,
            show_fractions      = input$show_fractions,
            show_conductance    = input$show_cond,
            show_uv260          = input$show_uv260,
            show_percent_b      = input$show_pctb,
            highlight_fractions = NULL,   # we draw highlights ourselves
            save_plot           = FALSE,
            theme               = input$theme,
            line_width          = input$linewidth
          )

          # Layer the user's highlight groups on top of the plot. Doing
          # this here (rather than in the helper) lets us paint multiple
          # groups in distinct colours, and resolve range endpoints
          # against the file's actual fraction order - which matters for
          # serpentine and other non-monotonic dispenser modes. Rectangles
          # are drawn on top of the fraction tick-marks but kept at
          # alpha = 0.30 so labels remain readable.
          fractions <- .parse_akta_fractions(input$files$datapath[1])
          highlight_groups <- .compile_highlight_groups(highlight_inputs, fractions)
          if (length(highlight_groups) > 0) {
            for (grp in highlight_groups) {
              rects <- .fraction_rects(fractions, grp$labels)
              if (!is.null(rects) && nrow(rects) > 0) {
                p <- p + ggplot2::annotate("rect",
                  xmin = rects$xmin, xmax = rects$xmax,
                  ymin = -Inf, ymax = Inf,
                  fill = grp$colour, alpha = 0.30)
              }
            }
          }

          shiny::incProgress(0.5, detail = "Rendering\u2026")
          akta_results(list(plot = p,
                            file_names = input$files$name,
                            renamed = renamed))

          akta_history(c(akta_history(), list(list(
            time  = format(Sys.time(), "%H:%M:%S"),
            files = paste(input$files$name, collapse = ", "),
            n     = nrow(input$files),
            vol   = if (!is.null(vol_range))
                      sprintf("%.0f-%.0f mL", vol_range[1], vol_range[2])
                    else "full"
          ))))
        }, error = function(e) {
          shiny::showNotification(paste("\u00c4KTA error:", conditionMessage(e)),
                                  type = "error", duration = 12)
        })
      })
    })

    # ---- Plot ------------------------------------------------------------
    output$plot <- shiny::renderPlot({
      shiny::req(akta_results())
      p   <- akta_results()$plot
      ann <- akta_annotation()

      # Annotation layer applied if user clicked "Add to Plot"
      if (!is.null(ann)) {
        lbl <- sprintf("Ve = %.2f mL\n%.1f kDa", ann$centroid, ann$mw_kda)
        p <- p +
          ggplot2::geom_vline(xintercept = ann$centroid,
                              colour = CG_PALETTE$accent_warm,
                              linewidth = 0.7, linetype = "dashed") +
          ggplot2::annotate("label",
                            x = ann$centroid, y = Inf,
                            label = lbl, vjust = 1.3, size = 3.5,
                            colour = CG_PALETTE$accent_warm,
                            fill = CG_PALETTE$bg_card,
                            label.size = 0.3,
                            label.padding = ggplot2::unit(0.25, "lines"))
      }

      # Layer the dark theme on top - plot_akta_improved() returns a plot
      # with its own theme baked in, so we just override colours.
      p + ggplot2::theme(
        plot.background   = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
        panel.background  = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
        panel.grid.major  = ggplot2::element_line(colour = CG_PALETTE$border,       linewidth = 0.4),
        panel.grid.minor  = ggplot2::element_line(colour = "#161E2E",               linewidth = 0.2),
        text              = ggplot2::element_text(colour = CG_PALETTE$txt),
        axis.text         = ggplot2::element_text(colour = CG_PALETTE$muted),
        axis.line         = ggplot2::element_line(colour = CG_PALETTE$border_light),
        axis.ticks        = ggplot2::element_line(colour = CG_PALETTE$border_light),
        plot.title        = ggplot2::element_text(face = "bold", colour = CG_PALETTE$txt),
        plot.subtitle     = ggplot2::element_text(colour = CG_PALETTE$muted),
        legend.background = ggplot2::element_rect(fill = CG_PALETTE$bg_card,
                                                  colour = CG_PALETTE$border),
        legend.text       = ggplot2::element_text(colour = CG_PALETTE$txt),
        legend.title      = ggplot2::element_text(colour = CG_PALETTE$muted)
      )
    }, bg = CG_PALETTE$bg_card)

    # ---- Peak integration -----------------------------------------------
    # Single source of truth: parses UV trace from first uploaded file,
    # integrates between v_start/v_end, computes centroid + MW.
    akta_integration <- shiny::reactive({
      shiny::req(input$files)
      v_start <- input$int_start
      v_end   <- input$int_end
      if (is.na(v_start) || is.na(v_end) || v_start >= v_end) return(NULL)

      void_vol  <- if (is.na(input$void_vol))  8.23  else input$void_vol
      total_vol <- if (is.na(input$total_vol)) 24.00 else input$total_vol

      tryCatch({
        uv_df <- .parse_akta_uv_trace(input$files$datapath[1])
        if (is.null(uv_df))
          return(list(error = "Could not read UV trace for integration."))

        roi   <- uv_df[uv_df$vol >= v_start & uv_df$vol <= v_end, ]
        total <- uv_df[uv_df$uv >= 0, ]
        if (nrow(roi) < 2)
          return(list(error = "Not enough data points in integration range."))

        # Trapezoidal area + purity
        area_roi   <- sum(diff(roi$vol)   * (head(roi$uv,   -1) + tail(roi$uv,   -1))) / 2
        area_total <- sum(diff(total$vol) * (head(total$uv, -1) + tail(total$uv, -1))) / 2
        purity     <- if (area_total > 0) 100 * area_roi / area_total else NA

        # UV-weighted centroid elution volume
        uv_pos   <- pmax(roi$uv, 0)
        centroid <- if (sum(uv_pos) > 0) sum(roi$vol * uv_pos) / sum(uv_pos)
                    else mean(c(v_start, v_end))

        # MW from SEC calibration
        norm_ve <- (centroid - void_vol) / (total_vol - void_vol)
        mw_da   <- 10 ^ (-3.22448969353114 * norm_ve + 5.92750021160459)

        list(error = NULL, area = area_roi, purity = purity,
             centroid = centroid, mw_da = mw_da, mw_kda = mw_da / 1000,
             v_start = v_start, v_end = v_end, n_points = nrow(roi),
             void_vol = void_vol, total_vol = total_vol)
      }, error = function(e) list(error = conditionMessage(e)))
    })

    output$integration_result <- shiny::renderUI({
      res <- akta_integration()
      if (is.null(res)) return(NULL)
      if (!is.null(res$error))
        return(shiny::div(class = "warn-box", res$error))

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, result_badge("Peak Area",      sprintf("%.1f", res$area))),
          shiny::column(6, result_badge("Purity",         sprintf("%.1f%%", res$purity),
                                        tone = "green"))
        ),
        shiny::fluidRow(
          shiny::column(6, result_badge("Elution Volume", sprintf("%.2f mL", res$centroid),
                                        tone = "orange")),
          shiny::column(6, shiny::div(class = "result-badge",
            shiny::div(class = "result-label", "Est. MW"),
            shiny::div(class = "result-value purple",
                       sprintf("%.1f kDa", res$mw_kda))))
        ),
        shiny::div(style = "font-size:0.72rem;color:var(--muted);margin-top:0.3rem;",
          sprintf("Range: %.1f\u2013%.1f mL  (%d points)",
                  res$v_start, res$v_end, res$n_points))
      )
    })

    output$integration_actions <- shiny::renderUI({
      res <- akta_integration()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      shiny::tagList(
        shiny::actionButton(ns("annotate_plot"), "\U0001f4cc  Add to Plot",
                            class = "btn-run",
                            style = "font-size:0.78rem;padding:0.45rem 0.9rem;width:auto;"),
        " ",
        shiny::downloadButton(ns("int_csv"), "\u2193 Export Table",
                              class = "btn-download")
      )
    })

    shiny::observeEvent(input$annotate_plot, {
      res <- akta_integration()
      if (!is.null(res) && is.null(res$error)) {
        akta_annotation(res)
        shiny::showNotification(
          "\u2713 Annotation set \u2014 regenerate the plot to see it on the chromatogram.",
          type = "message", duration = 4)
      }
    })

    output$int_csv <- shiny::downloadHandler(
      filename = function() ts_filename("AKTA_integration", "csv"),
      content  = function(file) {
        res <- akta_integration()
        shiny::req(!is.null(res) && is.null(res$error))
        run_name <- if (!is.null(input$files)) input$files$name[1] else "unknown"
        utils::write.csv(data.frame(
          Run            = run_name,
          Range_start_mL = res$v_start,
          Range_end_mL   = res$v_end,
          Peak_area      = round(res$area,     3),
          Purity_pct     = round(res$purity,   2),
          Elution_vol_mL = round(res$centroid, 3),
          MW_kDa         = round(res$mw_kda,   2),
          MW_Da          = round(res$mw_da,    0),
          Void_vol_mL    = res$void_vol,
          Total_vol_mL   = res$total_vol,
          Date           = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ), file, row.names = FALSE)
        shiny::showNotification("\u2713 Integration table exported!",
                                type = "message", duration = 3)
      }
    )

    # ---- Downloads -------------------------------------------------------
    output$download_buttons <- shiny::renderUI({
      shiny::req(akta_results())
      shiny::tagList(
        shiny::downloadButton(ns("dl_png"), "\u2193 PNG Plot",       class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_pdf"), "\u2193 PDF Plot",       class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_csv"), "\u2193 CSV Trace Data", class = "btn-download")
      )
    })

    dl_plot <- function(file, device, bg) {
      shiny::req(akta_results())
      p   <- akta_results()$plot
      ann <- akta_annotation()

      if (!is.null(ann)) {
        lbl <- sprintf("Ve = %.2f mL\n%.1f kDa", ann$centroid, ann$mw_kda)
        p <- p +
          ggplot2::geom_vline(xintercept = ann$centroid,
                              colour = "#E07030", linewidth = 0.7,
                              linetype = "dashed") +
          ggplot2::annotate("label",
                            x = ann$centroid, y = Inf, label = lbl,
                            vjust = 1.3, size = 3.5,
                            colour = "#E07030", fill = "white",
                            label.size = 0.3,
                            label.padding = ggplot2::unit(0.25, "lines"))
      }
      p <- p + theme_cg_publication()
      ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 300,
                      device = device, bg = bg)
    }

    output$dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("AKTA", "png"),
      content  = function(f) dl_plot(f, "png", "white"))
    output$dl_pdf <- shiny::downloadHandler(
      filename = function() ts_filename("AKTA", "pdf"),
      content  = function(f) dl_plot(f, "pdf", NULL))

    output$dl_csv <- shiny::downloadHandler(
      filename = function() ts_filename("AKTA_traces", "csv"),
      content  = function(file) {
        shiny::req(akta_results(), input$files)
        files     <- input$files$datapath
        run_names <- tools::file_path_sans_ext(input$files$name)

        # Parse UV 280 trace for every uploaded file
        traces <- lapply(files, .parse_akta_uv_trace)
        valid  <- Filter(Negate(is.null), traces)
        vnames <- run_names[!sapply(traces, is.null)]

        if (length(valid) == 0) {
          utils::write.csv(data.frame(Error = "No UV traces could be parsed"),
                           file, row.names = FALSE)
          return()
        }

        # Wide table: use first run's volume axis as reference, interpolate others
        ref_vol <- valid[[1]]$vol
        out     <- data.frame(Volume_mL = ref_vol)
        for (i in seq_along(valid)) {
          uv_interp <- stats::approx(valid[[i]]$vol, valid[[i]]$uv,
                                     xout = ref_vol, rule = 2)$y
          out[[paste0("UV280_", vnames[i])]] <- round(uv_interp, 4)
        }
        utils::write.csv(out, file, row.names = FALSE)
        shiny::showNotification("\u2713 Trace data exported!",
                                type = "message", duration = 3)
      }
    )

    # ---- History --------------------------------------------------------
    output$history_ui <- shiny::renderUI({
      h <- akta_history()
      if (length(h) == 0)
        return(shiny::p("No plots generated yet.",
                        style = "color:var(--muted);font-size:0.8rem;"))
      rows <- lapply(rev(h), function(e) {
        shiny::div(class = "history-row",
          style = "grid-template-columns: 70px 1fr 80px 80px;",
          shiny::div(class = "history-time", e$time),
          shiny::div(style = "font-size:0.75rem;color:var(--txt);overflow:hidden;
                              text-overflow:ellipsis;white-space:nowrap;", e$files),
          shiny::div(style = "font-size:0.75rem;color:var(--muted);",
                     paste0(e$n, " file(s)")),
          shiny::div(style = "font-size:0.75rem;color:var(--muted);", e$vol)
        )
      })
      do.call(shiny::div, rows)
    })

    # ---- Public reactive ------------------------------------------------
    # Pass current results + annotation for the cross-tool export
    shiny::reactive({
      r <- akta_results()
      if (is.null(r)) return(NULL)
      list(plot = r$plot, annotation = akta_annotation())
    })
  })
}


# -- Internal helpers ---------------------------------------------------------
#
# Format-aware UV-trace parser. Handles BOTH:
#   1. UTF-16LE tab-separated matrix format (older UNICORN exports)
#   2. UTF-8 columnar format with "UV"/"Fraction"/"Cond" headers on row 2
# Returns data.frame(vol, uv) or NULL.
.parse_akta_uv_trace <- function(fp) {
  tryCatch({
    first_lines <- readLines(fp, n = 3, encoding = "UTF-8", warn = FALSE)
    is_columnar <- length(first_lines) >= 2 &&
                   grepl("UV|Fraction|Cond", first_lines[2], ignore.case = FALSE)

    if (is_columnar) {
      raw         <- utils::read.csv(fp, header = FALSE, stringsAsFactors = FALSE,
                                     check.names = FALSE,
                                     fileEncoding = "UTF-8-BOM", na.strings = "")
      trace_types <- trimws(as.character(raw[2, ]))
      data_rows   <- raw[4:nrow(raw), ]
      uv_col      <- which(trace_types == "UV")[1]
      if (is.na(uv_col)) return(NULL)
      vols <- suppressWarnings(as.numeric(as.character(data_rows[[uv_col]])))
      uvs  <- suppressWarnings(as.numeric(as.character(data_rows[[uv_col + 1]])))
      ok   <- !is.na(vols) & !is.na(uvs)
      if (sum(ok) == 0) return(NULL)
      return(data.frame(vol = vols[ok], uv = uvs[ok]))
    }

    # Matrix format
    raw    <- utils::read.delim(fp, header = FALSE, sep = "\t",
                                fileEncoding = "UTF-16LE",
                                stringsAsFactors = FALSE, check.names = FALSE)
    aktlst <- t(as.matrix(raw))
    n_rows <- nrow(aktlst); n_cols <- ncol(aktlst)
    for (r in seq_len(n_rows)) {
      lbl <- trimws(as.character(aktlst[r, 2]))
      if (is.na(lbl) || lbl == "") next
      if (lbl %in% c("UV 1_280", "UV")) {
        x_v <- suppressWarnings(as.numeric(aktlst[r,     4:n_cols]))
        y_v <- suppressWarnings(as.numeric(aktlst[r + 1, 4:n_cols]))
        x_v <- x_v[!is.na(x_v)]; y_v <- y_v[!is.na(y_v)]
        m   <- min(length(x_v), length(y_v))
        if (m > 0) return(data.frame(vol = x_v[1:m], uv = y_v[1:m]))
      }
    }
    NULL
  }, error = function(e) NULL)
}

# ---- Fraction parser --------------------------------------------------------
#
# Mirrors .parse_akta_uv_trace() but pulls out the fraction list instead of
# the UV trace. Returns data.frame(volume, label) sorted by volume, or NULL
# if the file has no Fraction column.
.parse_akta_fractions <- function(fp) {
  tryCatch({
    first_lines <- readLines(fp, n = 3, encoding = "UTF-8", warn = FALSE)
    is_columnar <- length(first_lines) >= 2 &&
                   grepl("UV|Fraction|Cond", first_lines[2], ignore.case = FALSE)

    if (is_columnar) {
      raw         <- utils::read.csv(fp, header = FALSE, stringsAsFactors = FALSE,
                                     check.names = FALSE,
                                     fileEncoding = "UTF-8-BOM", na.strings = "")
      trace_types <- trimws(as.character(raw[2, ]))
      data_rows   <- raw[4:nrow(raw), ]
      frac_col    <- which(trace_types == "Fraction")[1]
      if (is.na(frac_col)) return(NULL)
      vols <- suppressWarnings(as.numeric(as.character(data_rows[[frac_col]])))
      lbls <- trimws(gsub('"', '', as.character(data_rows[[frac_col + 1]])))
      ok   <- !is.na(vols) & nchar(lbls) > 0
      if (sum(ok) == 0) return(NULL)
      df <- data.frame(volume = vols[ok], label = lbls[ok],
                       stringsAsFactors = FALSE)
      return(df[order(df$volume), ])
    }

    # Matrix format
    raw    <- utils::read.delim(fp, header = FALSE, sep = "\t",
                                fileEncoding = "UTF-16LE",
                                stringsAsFactors = FALSE, check.names = FALSE)
    aktlst <- t(as.matrix(raw))
    n_rows <- nrow(aktlst); n_cols <- ncol(aktlst)
    for (r in seq_len(n_rows)) {
      lbl <- trimws(as.character(aktlst[r, 2]))
      if (is.na(lbl) || lbl == "") next
      if (lbl == "Fraction") {
        if (r + 1 > n_rows) return(NULL)
        x_vals     <- suppressWarnings(as.numeric(aktlst[r,     4:n_cols]))
        frac_lbls  <- trimws(as.character(aktlst[r + 1, 4:n_cols]))
        ok <- !is.na(x_vals) & nchar(frac_lbls) > 0
        if (sum(ok) > 0) {
          df <- data.frame(volume = x_vals[ok], label = frac_lbls[ok],
                           stringsAsFactors = FALSE)
          return(df[order(df$volume), ])
        }
      }
    }
    NULL
  }, error = function(e) NULL)
}

# ---- Highlight-group compilation -------------------------------------------
#
# The user types fraction specs into the textboxes; each spec can be any
# mix of:
#
#   - "B8"                   single fraction
#   - "B8,B9 C12"            list (commas, semicolons, or whitespace)
#   - "A4-A8"                range, inclusive
#   - "1.B.8"                literal multi-run label
#
# Range semantics: the range covers everything between the two endpoints
# *in the order the fractions appear in the file* (sorted by volume). This
# matters because AKTA dispenser modes can interleave rows (serpentine:
# A1..A12, B12..B1, C1..C12, ...). Treating ranges as letter+number
# arithmetic gives wrong results for any range that crosses a row in
# serpentine mode. Using the file's own order is robust against every
# dispensing pattern.
#
# Multi-run files: a plain "B8" matches any of "1.B.8".."5.B.8" present
# in the file; a range endpoint matches the first occurrence of any form.
# Users who care about a specific run can use the literal "1.B.8".

# Split a spec string into a list of typed tokens. File-independent:
# doesn't look up anything yet, just classifies the user's input.
.tokenize_highlight_spec <- function(spec) {
  if (is.null(spec) || !nzchar(trimws(spec))) return(list())
  spec  <- gsub("\\s*-\\s*", "-", trimws(spec))   # tighten whitespace around dashes
  terms <- trimws(strsplit(spec, "[,;\\s]+", perl = TRUE)[[1]])
  terms <- terms[nzchar(terms)]
  lapply(terms, function(t) {
    if (grepl("-", t, fixed = TRUE)) {
      parts <- trimws(strsplit(t, "-", fixed = TRUE)[[1]])
      if (length(parts) == 2 && all(nzchar(parts)))
        return(list(kind = "range", from = parts[1], to = parts[2]))
    }
    list(kind = "single", term = t)
  })
}

# Find the position of a label in the chronologically-sorted fractions
# table. Tries exact match first, then multi-run expansions ("B8" matches
# any of "1.B.8".."5.B.8"). Returns the FIRST matching row position, or
# NA if nothing matches.
.find_fraction_position <- function(label, fractions) {
  exact <- which(fractions$label == label)
  if (length(exact) > 0) return(exact[1])

  # If the user gave a literal multi-run label (contains a dot), exact
  # match is all we try - we don't second-guess their run number.
  if (grepl(".", label, fixed = TRUE)) return(NA_integer_)

  m <- regmatches(label, regexec("^([A-Za-z]+)([0-9]+)$", label))[[1]]
  if (length(m) == 3) {
    candidates <- paste0(seq_len(5), ".", m[2], ".", m[3])
    hit <- which(fractions$label %in% candidates)
    if (length(hit) > 0) return(hit[1])
  }
  NA_integer_
}

# Resolve a token into a vector of fraction labels actually present in
# the file. Singles match exact + all multi-run forms; ranges return the
# chronological block between the two endpoints (inclusive, in file order).
.resolve_token <- function(token, fractions) {
  if (token$kind == "single") {
    if (token$term %in% fractions$label) return(token$term)
    if (grepl(".", token$term, fixed = TRUE)) return(character(0))
    m <- regmatches(token$term, regexec("^([A-Za-z]+)([0-9]+)$", token$term))[[1]]
    if (length(m) == 3) {
      candidates <- paste0(seq_len(5), ".", m[2], ".", m[3])
      return(intersect(candidates, fractions$label))
    }
    return(character(0))
  }
  # range
  pos_from <- .find_fraction_position(token$from, fractions)
  pos_to   <- .find_fraction_position(token$to,   fractions)
  if (is.na(pos_from) || is.na(pos_to)) return(character(0))
  span <- min(pos_from, pos_to):max(pos_from, pos_to)
  fractions$label[span]
}

# Take a list of group specs (from the UI) plus the file's fractions
# table, return a tidy list of {labels, colour} pairs ready for rectangle
# drawing. Empty groups dropped; if there are no fractions to match
# against, returns an empty list.
.compile_highlight_groups <- function(groups, fractions) {
  if (is.null(fractions) || nrow(fractions) == 0) return(list())
  out <- list()
  for (g in groups) {
    tokens <- .tokenize_highlight_spec(g$spec)
    if (!length(tokens)) next
    labels <- unique(unlist(lapply(tokens, .resolve_token, fractions = fractions)))
    if (!length(labels)) next
    colour <- if (!is.null(g$colour) && nzchar(g$colour)) g$colour else "#FFD93D"
    out[[length(out) + 1]] <- list(labels = labels, colour = colour)
  }
  out
}

# Given a fraction-volume table and a vector of labels, return
# data.frame(xmin, xmax) for matching fraction boundaries. Each fraction
# extends from its start volume to the next fraction's start volume; the
# last fraction uses the mean fraction width as a fallback. Works for any
# dispensing pattern because the table is volume-sorted.
.fraction_rects <- function(fractions, labels_to_highlight) {
  if (is.null(fractions) || nrow(fractions) == 0) return(NULL)
  idx <- which(fractions$label %in% labels_to_highlight)
  if (!length(idx)) return(NULL)
  avg_width <- if (nrow(fractions) > 1) mean(diff(fractions$volume)) else 1
  rows <- lapply(idx, function(i) {
    xmin <- fractions$volume[i]
    xmax <- if (i < nrow(fractions)) fractions$volume[i + 1]
            else xmin + avg_width
    data.frame(xmin = xmin, xmax = xmax)
  })
  do.call(rbind, rows)
}

# Render one (colour picker + textbox) row for the advanced highlight panel.
# Used for groups 2..4; group 1 is the main textbox above.
.akta_hl_group_row <- function(ns, idx, default_colour) {
  shiny::fluidRow(style = "margin-bottom:0.5rem;",
    shiny::column(5, colour_picker_inline(
      ns(paste0("hl_g", idx, "_colour")), default_colour, text_width = "70px")),
    shiny::column(7, shiny::textInput(
      ns(paste0("hl_g", idx)), NULL,
      placeholder = sprintf("Group %d fractions", idx)))
  )
}

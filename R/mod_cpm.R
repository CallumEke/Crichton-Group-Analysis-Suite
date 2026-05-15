################################################################################
#  mod_cpm.R  --  CPM Thermostability Peak Picker (Shiny module)
################################################################################
#
#  Second worked example. Identical structural pattern to mod_bca.R:
#     cpm_ui(id) + cpm_server(id), returning a reactive() of current results.
#
#  Note the conditionalPanel() calls use sprintf("input['%s'] == ...", ns(...))
#  - because mode lives in this module's namespace, the JS condition must use
#  the namespaced id, not the bare one.
#
################################################################################

# -- UI -----------------------------------------------------------------------
cpm_ui <- function(id) {
  ns <- shiny::NS(id)

  # See note in mod_bca.R
  .cpm_expected <- c("read_rotorgene_csv",
                     "calculate_tm",
                     "calculate_tm_automatic")
  if (!ensure_helper_loaded("tm_analysis_functions.R", .cpm_expected)) {
    return(missing_helper_warning("tm_analysis_functions.R", .cpm_expected))
  }

  shiny::tagList(
    shiny::div(class = "clear-button-container",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

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
          step_title(2, "Select Sample"),
          shiny::uiOutput(ns("sample_ui"))
        ),

        lab_card(
          step_title(3, "Analysis Mode"),
          shiny::div(class = "mode-pills",
            shiny::radioButtons(ns("mode"), NULL,
              choices = c("Manual range" = "manual", "Auto peak detect" = "auto"),
              selected = "manual", inline = TRUE)
          ),
          shiny::br(),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("mode")),
            info_box("Check the Preview tab, then enter the temperature range around your peak."),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("tlow"),  "Lower T (\u00b0C)", value = 45, step = 0.5)),
              shiny::column(6, shiny::numericInput(ns("thigh"), "Upper T (\u00b0C)", value = 65, step = 0.5))
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'auto'", ns("mode")),
            info_box("Peaks outside the region of interest are ignored for analysis but still plotted."),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("tmin"), "Ignore below (\u00b0C)", value = 30, step = 1)),
              shiny::column(6, shiny::numericInput(ns("tmax"), "Ignore above (\u00b0C)", value = 80, step = 1))
            ),
            shiny::numericInput(ns("prominence"), "Min peak prominence (0-1)",
              value = 0.10, min = 0.01, max = 0.9, step = 0.01)
          )
        ),

        lab_card(
          step_title(4, "Custom Sample Name"),
          info_box("Override the auto-generated name for cleaner plots and exports."),
          shiny::textInput(ns("custom_name"), NULL, placeholder = "Leave blank to use original name")
        ),

        lab_card(
          step_title(5, "Analyse"),
          shiny::actionButton(ns("run"), "\u25b6  Run Analysis", class = "btn-run"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("download_buttons")),
          shiny::br(),
          shiny::tags$button("\u2699  Advanced Settings", class = "adv-toggle",
            onclick = sprintf("$('#%s').slideToggle(200)", ns("adv_panel"))),
          shiny::div(id = ns("adv_panel"), style = "display:none;",
            shiny::div(class = "settings-group",
              shiny::div(class = "settings-group-title", "Peak Detection"),
              shiny::numericInput(ns("smooth_sigma"), "Smoothing sigma (\u00b0C)",
                value = 3, min = 0.5, max = 10, step = 0.5),
              shiny::numericInput(ns("min_sep"), "Min peak separation (\u00b0C)",
                value = 8, min = 1, max = 20, step = 1),
              shiny::numericInput(ns("boundary_thresh"), "Boundary threshold (0-1)",
                value = 0.10, min = 0.01, max = 0.5, step = 0.01)
            )
          )
        )
      ),

      shiny::column(8,
        bslib::navset_card_underline(
          id = ns("tabs"),
          bslib::nav_panel("\U0001f4e1  Preview",
            shiny::div(style = "padding:1rem 0;",
              info_box("Full dF/dT trace. Use to choose your integration range."),
              shiny::plotOutput(ns("preview_plot"), height = "380px")
            )
          ),
          bslib::nav_panel("\U0001f4c8  Results",
            shiny::div(style = "padding:1rem 0;",
              shiny::uiOutput(ns("result_badges")),
              shiny::plotOutput(ns("result_plot"), height = "400px"),
              shiny::br(),
              shiny::uiOutput(ns("results_table"))
            )
          ),
          bslib::nav_panel("\U0001f551  History",
            shiny::div(style = "padding:1rem 0;",
              shiny::uiOutput(ns("history_ui")),
              shiny::uiOutput(ns("history_dl"))
            )
          )
        )
      )
    )
  )
}


# -- Server -------------------------------------------------------------------
cpm_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cpm_data    <- shiny::reactiveVal(NULL)
    cpm_results <- shiny::reactiveVal(NULL)
    cpm_history <- shiny::reactiveVal(list())

    # ---- Clear -----------------------------------------------------------
    shiny::observeEvent(input$clear, {
      cpm_data(NULL); cpm_results(NULL); cpm_history(list())
      for (id in c("file", "custom_name", "mode", "tlow", "thigh",
                   "tmin", "tmax", "prominence")) shinyjs::reset(id)
      shiny::showNotification("CPM data cleared", type = "message", duration = 2)
    })

    # ---- Sample name resolution -----------------------------------------
    cpm_resolved_name <- shiny::reactive({
      d <- cpm_data()
      shiny::req(d, !is.null(d$sample_names))
      custom <- trimws(input$custom_name)
      sid    <- input$sample_id
      if (nchar(custom) > 0) custom
      else {
        idx <- which(d$sample_ids == sid)
        if (length(idx)) d$sample_names[idx[1]] else sid
      }
    })

    # ---- File upload -----------------------------------------------------
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      cpm_results(NULL)
      tryCatch({
        cpm_data(read_rotorgene_csv(input$file$datapath))
      }, error = function(e) cpm_data(list(error = conditionMessage(e))))
    })

    output$file_status <- shiny::renderUI({
      shiny::req(cpm_data())
      d <- cpm_data()
      if (!is.null(d$error)) {
        status_pill("error", "Error loading file")
      } else {
        n <- length(d$sample_ids)
        status_pill("ready",
                    sprintf("%d sample%s loaded", n, if (n != 1) "s" else ""))
      }
    })

    output$sample_ui <- shiny::renderUI({
      shiny::req(cpm_data())
      d <- cpm_data()
      if (!is.null(d$error)) return(NULL)
      choices <- setNames(d$sample_ids,
                          paste0("[", d$sample_ids, "] ", d$sample_names))
      shiny::tagList(
        shiny::selectInput(ns("sample_id"), "Sample", choices = choices),
        shiny::uiOutput(ns("sample_confirm"))
      )
    })

    output$sample_confirm <- shiny::renderUI({
      shiny::req(input$sample_id, cpm_data())
      d   <- cpm_data()
      idx <- which(d$sample_ids == input$sample_id)
      if (!length(idx)) return(NULL)
      status_pill("ready",
                  paste0("ID ", d$sample_ids[idx], ": ", d$sample_names[idx]))
    })

    # ---- Sample data extraction ------------------------------------------
    cpm_sample_data <- shiny::reactive({
      shiny::req(cpm_data(), input$sample_id)
      d   <- cpm_data()
      idx <- which(d$sample_ids == input$sample_id)
      shiny::req(length(idx) > 0)
      df  <- data.frame(Temperature = d$temperature, dFdT = d$data[, idx[1]])
      stats::na.omit(df)
    })

    # ---- Preview plot ----------------------------------------------------
    output$preview_plot <- shiny::renderPlot({
      shiny::req(cpm_data(), input$sample_id)
      df    <- cpm_sample_data()
      sname <- cpm_resolved_name()
      ggplot2::ggplot(df, ggplot2::aes(x = Temperature, y = dFdT)) +
        ggplot2::geom_line(color = CG_PALETTE$accent, linewidth = 1.0) +
        ggplot2::labs(title = paste0("Preview: ", sname),
                      x = "Temperature (\u00b0C)", y = "dF/dT (raw)") +
        theme_cg_dark()
    }, bg = CG_PALETTE$bg_card)

    # ---- Run analysis ----------------------------------------------------
    shiny::observeEvent(input$run, {
      shiny::req(cpm_data(), input$sample_id)
      d <- cpm_data()
      if (!is.null(d$error)) {
        shiny::showNotification(d$error, type = "error"); return()
      }
      sname <- cpm_resolved_name()

      shiny::withProgress(message = "Analysing CPM\u2026", value = 0, {
        tryCatch({
          shiny::incProgress(0.4, detail = "Calculating Tm\u2026")
          res <- if (input$mode == "manual") {
            calculate_tm(
              data        = cpm_sample_data(),
              T_lower     = input$tlow,
              T_upper     = input$thigh,
              sample_name = sname,
              sample_id   = input$sample_id
            )
          } else {
            calculate_tm_automatic(
              data             = cpm_sample_data(),
              T_min            = input$tmin,
              T_max            = input$tmax,
              min_prominence   = input$prominence,
              smooth_sigma_deg = input$smooth_sigma,
              min_peak_sep_deg = input$min_sep,
              boundary_thresh  = input$boundary_thresh,
              sample_name      = sname,
              sample_id        = input$sample_id
            )
          }
          shiny::incProgress(0.5, detail = "Rendering\u2026")
          cpm_results(list(res = res, mode = input$mode, sample_name = sname))
          bslib::nav_select(ns("tabs"), "\U0001f4c8  Results")

          # History
          if (input$mode == "manual") {
            entry <- list(time = format(Sys.time(), "%H:%M:%S"), sample = sname,
              tm = res$tm,
              range = sprintf("%.1f-%.1f", res$T_lower, res$T_upper),
              area = res$area, fwhm = res$fwhm, mode = "manual")
          } else {
            pk1 <- res$peak_results[[1]]
            entry <- list(time = format(Sys.time(), "%H:%M:%S"), sample = sname,
              tm = pk1$tm,
              range = sprintf("%.1f-%.1f", pk1$T_start, pk1$T_end),
              area = pk1$area, fwhm = NA, mode = "auto",
              n_peaks = res$n_peaks)
          }
          cpm_history(c(cpm_history(), list(entry)))

        }, error = function(e) {
          shiny::showNotification(paste("Analysis error:", conditionMessage(e)),
                                  type = "error", duration = 12)
        })
      })
    })

    # ---- Result badges --------------------------------------------------
    output$result_badges <- shiny::renderUI({
      shiny::req(cpm_results())
      r <- cpm_results()
      if (r$mode == "manual") {
        shiny::fluidRow(
          shiny::column(4, result_badge("Tm",
            sprintf("%.2f \u00b0C", r$res$tm))),
          shiny::column(4, result_badge("Integration Range",
            sprintf("%.1f \u2013 %.1f \u00b0C", r$res$T_lower, r$res$T_upper),
            tone = "green")),
          shiny::column(4, result_badge("Peak Area",
            sprintf("%.4f", r$res$area), tone = "orange"))
        )
      } else {
        n    <- r$res$n_peaks
        rows <- lapply(seq_len(n), function(i) {
          pk  <- r$res$peak_results[[i]]
          lbl <- if (n == 1) "" else paste0("Peak ", i, " \u2014 ")
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(4, result_badge(paste0(lbl, "Tm"),
                sprintf("%.2f \u00b0C", pk$tm))),
              shiny::column(4, result_badge(paste0(lbl, "Integration Range"),
                sprintf("%.1f \u2013 %.1f \u00b0C", pk$T_start, pk$T_end), tone = "green")),
              shiny::column(4, result_badge(paste0(lbl, "Peak Area"),
                sprintf("%.4f", pk$area), tone = "orange"))
            ),
            if (i < n) shiny::tags$hr(style = "border-color:#1E2D45; margin:0.5rem 0;") else NULL
          )
        })
        do.call(shiny::tagList, rows)
      }
    })

    # ---- Result plot ----------------------------------------------------
    output$result_plot <- shiny::renderPlot({
      shiny::req(cpm_results())
      cpm_results()$res$plot + theme_cg_dark() +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(colour = CG_PALETTE$muted))
    }, bg = CG_PALETTE$bg_card)

    # ---- Results table --------------------------------------------------
    output$results_table <- shiny::renderUI({
      shiny::req(cpm_results())
      .cpm_results_table_ui(cpm_results())
    })

    # ---- Downloads ------------------------------------------------------
    output$download_buttons <- shiny::renderUI({
      shiny::req(cpm_results())
      shiny::tagList(
        shiny::downloadButton(ns("dl_png"), "\u2193 PNG Plot",    class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_pdf"), "\u2193 PDF Plot",    class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_csv"), "\u2193 CSV Results", class = "btn-download")
      )
    })

    dl_plot <- function(file, device, bg) {
      shiny::req(cpm_results())
      p <- cpm_results()$res$plot + theme_cg_publication()
      ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 300,
                      device = device, bg = bg)
    }

    output$dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_Tm", "png"),
      content  = function(f) dl_plot(f, "png", "white"))
    output$dl_pdf <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_Tm", "pdf"),
      content  = function(f) dl_plot(f, "pdf", NULL))
    output$dl_csv <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_results", "csv"),
      content  = function(file) {
        r <- cpm_results()
        df <- if (r$mode == "manual") {
          cbind(Sample = r$sample_name, Mode = "Manual",
                Tm_degC = r$res$tm, T_lower = r$res$T_lower, T_upper = r$res$T_upper,
                Area = r$res$area, FWHM = r$res$fwhm, N_points = r$res$n_points,
                Date = Sys.time())
        } else {
          cbind(Sample = r$sample_name, Mode = "Automatic",
                r$res$summary, Date = Sys.time())
        }
        utils::write.csv(df, file, row.names = FALSE)
      })

    # ---- History --------------------------------------------------------
    output$history_ui <- shiny::renderUI({
      h <- cpm_history()
      if (length(h) == 0) {
        return(shiny::p("No analyses run yet.",
                        style = "color:var(--muted);font-size:0.8rem;"))
      }
      rows <- lapply(rev(h), function(e) {
        shiny::div(class = "history-row",
          style = "grid-template-columns: 70px 1fr 80px 120px 80px;",
          shiny::div(class = "history-time", e$time),
          shiny::div(style = "font-size:0.78rem;color:var(--txt);overflow:hidden;
                              text-overflow:ellipsis;white-space:nowrap;", e$sample),
          shiny::div(class = "history-val", sprintf("%.2f\u00b0C", e$tm)),
          shiny::div(style = "font-size:0.75rem;color:var(--muted);", e$range),
          shiny::div(class = "history-val", style = "color:var(--accent-warm);",
                     sprintf("A=%.4f", e$area))
        )
      })
      do.call(shiny::div, rows)
    })

    output$history_dl <- shiny::renderUI({
      shiny::req(length(cpm_history()) > 0)
      shiny::downloadButton(ns("history_csv"), "\u2193 Export History CSV",
                            class = "btn-download")
    })

    output$history_csv <- shiny::downloadHandler(
      filename = function() ts_filename("CPM_history", "csv"),
      content  = function(file) {
        h  <- cpm_history()
        df <- do.call(rbind, lapply(h, function(e)
          data.frame(Time = e$time, Sample = e$sample, Tm_degC = e$tm,
                     Range = e$range, Area = e$area,
                     FWHM = if (is.null(e$fwhm) || is.na(e$fwhm)) NA else e$fwhm,
                     Mode = e$mode)))
        utils::write.csv(df, file, row.names = FALSE)
      })

    # ---- Public reactive ------------------------------------------------
    shiny::reactive(cpm_results())
  })
}


# -- Internal helpers ---------------------------------------------------------
.cpm_results_table_ui <- function(r) {
  th_style <- "background:#161E2E;color:#7A8FAD;font-family:monospace;
               font-size:0.72rem;letter-spacing:0.08em;text-transform:uppercase;
               padding:0.55rem 0.75rem;text-align:left;border-bottom:1px solid #1E2D45;"
  td_p     <- "padding:0.5rem 0.75rem;color:#7A8FAD;font-size:0.82rem;
               font-weight:500;border-bottom:1px solid #1E2D45;"
  td_v     <- "padding:0.5rem 0.75rem;color:#E8F0FE;font-size:0.82rem;
               font-family:monospace;border-bottom:1px solid #1E2D45;"

  make_tbl <- function(params, vals) {
    rows <- mapply(function(p, v)
      shiny::tags$tr(shiny::tags$td(p, style = td_p),
                     shiny::tags$td(v, style = td_v)),
      params, vals, SIMPLIFY = FALSE)
    shiny::tags$table(style = "width:100%;border-collapse:collapse;",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("PARAMETER", style = th_style),
        shiny::tags$th("VALUE",     style = th_style))),
      shiny::tags$tbody(rows))
  }

  if (r$mode == "manual") {
    make_tbl(
      c("Sample", "Tm (\u00b0C)", "Integration Range", "Peak Area", "FWHM (\u00b0C)", "N points"),
      c(r$sample_name, sprintf("%.2f", r$res$tm),
        sprintf("%.1f \u2013 %.1f \u00b0C", r$res$T_lower, r$res$T_upper),
        sprintf("%.4f", r$res$area),
        if (!is.null(r$res$fwhm) && !is.na(r$res$fwhm))
          sprintf("%.2f", r$res$fwhm) else "N/A",
        as.character(r$res$n_points)))
  } else {
    n       <- r$res$n_peaks
    hdr     <- "font-size:0.72rem;letter-spacing:0.1em;text-transform:uppercase;
                color:#00C2FF;padding:0.75rem 0.75rem 0.35rem;font-family:monospace;"
    div_sep <- "border-top:2px solid #1E2D45;margin-top:0.75rem;"
    blocks  <- lapply(seq_len(n), function(i) {
      pk <- r$res$peak_results[[i]]
      shiny::tagList(
        if (i > 1) shiny::tags$div(style = div_sep) else NULL,
        shiny::tags$div(
          if (n == 1) "Detected Peak" else paste0("Peak ", i, " of ", n),
          style = hdr),
        make_tbl(
          c("Tm (\u00b0C)", "Integration Range", "Peak Area", "Height",
            "Prominence", "FWHM (\u00b0C)", "N points"),
          c(sprintf("%.2f", pk$tm),
            sprintf("%.1f \u2013 %.1f \u00b0C", pk$T_start, pk$T_end),
            sprintf("%.4f", pk$area),
            sprintf("%.3f", pk$height),
            sprintf("%.3f", pk$prominence),
            if (!is.null(pk$fwhm) && !is.na(pk$fwhm))
              sprintf("%.2f", pk$fwhm) else "N/A",
            as.character(pk$n_points)))
      )
    })
    do.call(shiny::tagList, blocks)
  }
}

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
    shiny::div(style = "display: none;",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    shiny::div(class = "sticky-tool",
      shiny::fluidRow(
        shiny::column(4,
          shiny::div(class = "workflow-col",
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
          )  # close workflow-col
        ),

        shiny::column(8,
          shiny::div(class = "preview-col",
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
              )
            )
          )  # close preview-col
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

    # Single source of truth for "which file is loaded?". Same pattern as
    # AKTA/BCA/CPM QC - populated by user upload OR the bundled example
    # on session start. Shape matches shiny::fileInput's data.frame.
    current_file <- shiny::reactiveVal(NULL)

    # When the example loader fires, it sets this to "2" so the sample
    # dropdown opens to sample [2] by default. After the renderUI fires
    # it's reset to NULL so subsequent user uploads use the default
    # "first choice" behaviour without any special-casing.
    next_default_sample <- shiny::reactiveVal(NULL)

    .clear_state <- function(reset_inputs = TRUE) {
      cpm_data(NULL); cpm_results(NULL)
      if (reset_inputs) {
        for (id in c("custom_name", "mode", "tlow", "thigh",
                     "tmin", "tmax", "prominence")) shinyjs::reset(id)
      }
    }

    # ---- Clear button (still in DOM, fired by global navbar Clear) -----
    shiny::observeEvent(input$clear, {
      current_file(NULL); .clear_state()
      tryCatch(.load_example_file(), error = function(e) NULL)
      shiny::showNotification("CPM data cleared", type = "message", duration = 2)
    })

    # ---- Sample name resolution -----------------------------------------
    cpm_resolved_name <- shiny::reactive({
      d <- cpm_data()
      shiny::req(d, !is.null(d$sample_names))
      custom <- trimws(input$custom_name %||% "")
      sid    <- input$sample_id
      if (nchar(custom) > 0) custom
      else {
        idx <- which(d$sample_ids == sid)
        if (length(idx)) d$sample_names[idx[1]] else sid
      }
    })

    # ---- File upload routing -------------------------------------------
    shiny::observeEvent(input$file, {
      .clear_state()
      current_file(input$file)
    }, ignoreInit = TRUE)

    # Parse whatever's in current_file. Single observer = both upload
    # and example-load paths share the same parse logic.
    shiny::observeEvent(current_file(), {
      cf <- current_file()
      shiny::req(cf)
      tryCatch({
        cpm_data(read_rotorgene_csv(cf$datapath))
      }, error = function(e) cpm_data(list(error = conditionMessage(e))))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

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
      # Consult-and-clear the deferred default. The example loader sets
      # this to "2" so the example opens on sample [2]; user uploads
      # don't set it so we fall back to the first choice as before.
      pending <- shiny::isolate(next_default_sample())
      shiny::isolate(next_default_sample(NULL))
      sel <- if (!is.null(pending) && pending %in% as.character(d$sample_ids))
               pending
             else NULL   # let shiny default to first choice
      shiny::tagList(
        shiny::selectInput(ns("sample_id"), "Sample", choices = choices,
                           selected = sel),
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

    # ---- Analysis core --------------------------------------------------
    # Extracted helper so we can fire it from:
    #   - user clicks "Run Analysis" (input$run)
    #   - auto-run after a successful parse + sample selection (input$sample_id)
    .run_analysis <- function() {
      d <- cpm_data()
      if (is.null(d) || !is.null(d$error)) return(invisible())
      sid <- input$sample_id
      if (is.null(sid)) return(invisible())
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
              sample_id   = sid
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
              sample_id        = sid
            )
          }
          shiny::incProgress(0.5, detail = "Rendering\u2026")
          cpm_results(list(res = res, mode = input$mode, sample_name = sname))
          bslib::nav_select(ns("tabs"), "\U0001f4c8  Results")
        }, error = function(e) {
          shiny::showNotification(paste("Analysis error:", conditionMessage(e)),
                                  type = "error", duration = 12)
        })
      })
    }

    # Trigger 1: user clicked "Run Analysis"
    shiny::observeEvent(input$run, .run_analysis())

    # Trigger 2: auto-run when both data and sample selection are ready.
    # Same pending-flag pattern as CPM QC - the sample dropdown renders
    # AFTER cpm_data() is set, so we can't auto-run directly from
    # observeEvent(cpm_data()). The flag is set when data parses
    # successfully and cleared after the first auto-run, so changing the
    # sample selection later doesn't re-trigger automatically.
    .auto_run_pending <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(cpm_data(), {
      d <- cpm_data()
      if (!is.null(d) && is.null(d$error)) .auto_run_pending(TRUE)
    }, ignoreInit = TRUE)
    shiny::observe({
      if (!isTRUE(.auto_run_pending())) return()
      shiny::req(cpm_data(), input$sample_id)
      .auto_run_pending(FALSE)
      .run_analysis()
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

    # ---- Trigger 3: session start with bundled example -----------------
    # Reuses inst/examples/cpm_qc_simple_example.csv - the same RotorGene
    # export the CPM QC simple tab uses. We default to sample [2] (the
    # +GDP measurement) for the CPM Peak preview - a typical use case.
    .load_example_file <- function() {
      cf <- .cpm_example_file()
      if (!is.null(cf)) {
        next_default_sample("2")
        current_file(cf)
      }
    }
    .example_loader_obs <- shiny::observe({
      .example_loader_obs$destroy()
      tryCatch(.load_example_file(), error = function(e)
        message("[CPM Peak] example load failed: ", conditionMessage(e)))
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

# ---- Example data loader --------------------------------------------------
# Reuses the same RotorGene Q export bundled for CPM QC's simple tab -
# inst/examples/cpm_qc_simple_example.csv. We don't ship a duplicate
# because the CPM Peak picker and the CPM QC simple comparison both
# operate on the same dF/dT data format; one file serves both tools.
.cpm_example_cache <- new.env(parent = emptyenv())

.cpm_example_file <- function() {
  if (!is.null(.cpm_example_cache$path) &&
      file.exists(.cpm_example_cache$path)) {
    return(data.frame(
      name = "251210_HsUCP1_QC_ug_Screen_Transpose.csv",
      datapath = .cpm_example_cache$path,
      stringsAsFactors = FALSE
    ))
  }

  app_dir_local <- if (exists("app_dir", envir = globalenv())) {
    get("app_dir", envir = globalenv())
  } else getwd()

  candidates <- unique(c(
    file.path(app_dir_local, "inst", "examples", "cpm_qc_simple_example.csv"),
    file.path(getwd(),       "inst", "examples", "cpm_qc_simple_example.csv"),
    file.path("inst", "examples", "cpm_qc_simple_example.csv")
  ))
  src <- candidates[file.exists(candidates)][1]
  if (is.na(src)) return(NULL)

  tryCatch({
    out <- file.path(tempdir(), "251210_HsUCP1_QC_ug_Screen_Transpose.csv")
    file.copy(src, out, overwrite = TRUE)
    .cpm_example_cache$path <- out
    data.frame(name = "251210_HsUCP1_QC_ug_Screen_Transpose.csv",
               datapath = out, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

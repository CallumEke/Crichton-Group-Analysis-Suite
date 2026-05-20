################################################################################
#  mod_bca.R  --  BCA Protein Quantification (Shiny module)
################################################################################
#
#  Worked example of how each tool should be packaged.
#
#  STRUCTURE
#  ---------
#  bca_ui(id)     -> tagList for the nav_panel
#  bca_server(id) -> moduleServer; returns a reactive() exposing this tool's
#                    current results so the top-level app can pick them up for
#                    the cross-tool "Export Report" zip.
#
#  NAMESPACING
#  -----------
#  Inside the module everything is referenced by its bare name (e.g. input$file,
#  output$results_table). The NS() helper prefixes IDs with the module's
#  namespace, so two instances could coexist - that matters less here than
#  the isolation benefit: nothing inside the BCA module can collide with
#  IDs in any other tool, and you can find every BCA input by grep'ing this
#  file alone.
#
################################################################################

# -- UI -----------------------------------------------------------------------
bca_ui <- function(id) {
  ns <- shiny::NS(id)

  # Make sure the analytic helper is loaded. If global.R already did it,
  # this is a no-op; otherwise we try again here so the user has a recovery
  # path without restarting (e.g. they installed a missing package, hit Run
  # App again, the helper now sources, and the tool renders).
  .bca_expected <- c("read_softmax_bca",
                     "create_standard_curve",
                     "calculate_protein_yield")
  if (!ensure_helper_loaded("softmax_bca_improved.R", .bca_expected)) {
    return(missing_helper_warning("softmax_bca_improved.R", .bca_expected))
  }

  shiny::tagList(
    # Clear-all button (top right)
    shiny::div(style = "display: none;",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    shiny::div(class = "sticky-tool",
      shiny::fluidRow(
        # ----- Left column: controls (1-4) --------------------------------
        # `workflow-col` (in custom.css) gives this column its own scroll
        # so the preview on the right can stay sticky.
        shiny::column(4,
          shiny::div(class = "workflow-col",
            lab_card(
              step_title(1, "Upload Data File"),
              shiny::fileInput(ns("file"), NULL,
                               accept = c(".xls", ".xlsx", ".csv", ".txt"),
                               buttonLabel = "Browse\u2026",
                               placeholder = "SoftMax Pro export"),
              shiny::uiOutput(ns("file_status"))
            ),

            lab_card(
              step_title(2, "Concentration Source"),
              info_box("Use Manual if multiple samples share one plate."),
              shiny::div(class = "mode-pills",
                shiny::radioButtons(ns("mode"), NULL,
                  choices = c("Automatic (from file)" = "auto",
                              "Manual entry"          = "manual"),
                  selected = "auto", inline = TRUE)
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'manual'", ns("mode")),
                shiny::br(),
                shiny::numericInput(ns("manual_conc"),
                                    "Protein concentration (mg/mL)",
                                    value = NULL, min = 0, step = 0.01)
              )
            ),

            lab_card(
              step_title(3, "Parameters"),
              shiny::numericInput(ns("volume"),  "Sample volume (mL)", value = 1.0, min = 0.001, step = 0.1),
              shiny::textInput(ns("title"),     "Result table title", value = "BCA Assay Protein Yield Summary"),
              shiny::numericInput(ns("digits"), "Decimal places",     value = 2, min = 1, max = 4)
            ),

            lab_card(
              step_title(4, "Analyse"),
              shiny::actionButton(ns("run"), "\u25b6  Run Analysis", class = "btn-run"),
              shiny::br(), shiny::br(),
              shiny::uiOutput(ns("download_buttons"))
            )
          )  # close workflow-col
        ),

        # ----- Right column: results (sticky) -----------------------------
        # The Results table card was removed - all values shown in the
        # badge row (Concentration / Total Yield / R^2) plus the standard
        # curve. PNG/CSV exports of the table still work via the
        # download buttons - those build the table from bca_results()
        # directly, not from any rendered DT widget.
        shiny::column(8,
          shiny::div(class = "preview-col",
            shiny::uiOutput(ns("result_badges")),
            lab_card(
              shiny::div(class = "lab-card-title", "\U0001f4c8  Standard Curve"),
              shiny::uiOutput(ns("curve_placeholder")),
              shiny::plotOutput(ns("curve_plot"), height = "360px")
            )
          )
        )
      )
    )
  )
}


# -- Server -------------------------------------------------------------------
#' BCA module server
#'
#' @param id module id (must match bca_ui id)
#' @return a reactive() yielding the current results list, or NULL.
#'   Shape: list(curve, conc, vol, yield, gt, title, file_name).
#'   The top-level app uses this for the cross-tool "Export Report" bundle.
bca_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State -----------------------------------------------------------
    bca_data    <- shiny::reactiveVal(NULL)
    bca_results <- shiny::reactiveVal(NULL)

    # current_file mirrors AKTA's current_files pattern: a single source
    # of truth for "which file are we analysing?" with shape matching
    # what shiny::fileInput returns:
    #   data.frame(name, datapath, ...) with a single row.
    # Populated from two sources: user upload (input$file) or the bundled
    # example on session start.
    current_file <- shiny::reactiveVal(NULL)

    # ---- Internal: clear all derived state -----------------------------
    .clear_state <- function(reset_inputs = TRUE) {
      bca_data(NULL); bca_results(NULL)
      if (reset_inputs) {
        for (i in c("mode", "manual_conc", "volume", "title", "digits"))
          shinyjs::reset(i)
      }
    }

    # ---- Clear button (still in DOM, fired by global navbar Clear) -----
    shiny::observeEvent(input$clear, {
      current_file(NULL)
      .clear_state()
      # Reload the bundled example so the preview is never empty after
      # clearing. Same pattern as AKTA's Clear behaviour.
      tryCatch(.load_example_file(), error = function(e) NULL)
      shiny::showNotification("BCA data cleared", type = "message", duration = 2)
    })

    # ---- File upload: route into current_file, then run -----------------
    # New upload = fresh dataset. Wipe derived state first (results,
    # title etc) so we don't carry stale numbers across, then set the
    # file and trigger analysis automatically. User can still hit
    # "Run Analysis" later to re-run with different volume/mode/etc.
    shiny::observeEvent(input$file, {
      .clear_state()
      current_file(input$file)
    }, ignoreInit = TRUE)

    # When current_file changes (from upload or example), parse it and
    # populate bca_data(). Doing this in a dedicated observer keeps the
    # upload-vs-example paths sharing exactly the same parse logic.
    shiny::observeEvent(current_file(), {
      cf <- current_file()
      shiny::req(cf)
      tryCatch({
        fp       <- cf$datapath
        raw_data <- read_softmax_bca(fp)
        if (is.null(raw_data$groups) || nrow(raw_data$groups) == 0)
          stop("No group data found. Ensure this is a valid SoftMax Pro export.")
        bca_data(list(filepath = fp, raw = raw_data, name = cf$name))
      }, error = function(e) {
        bca_data(list(error = conditionMessage(e)))
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$file_status <- shiny::renderUI({
      shiny::req(bca_data())
      d <- bca_data()
      if (!is.null(d$error)) {
        shiny::tagList(
          status_pill("error", "Error loading file"),
          shiny::div(
            style = "color: #FF5C5C; font-size: 0.85em; margin-top: 0.5rem;
                     padding: 0.5rem; background: rgba(255,92,92,0.1);
                     border-radius: 4px;",
            paste("Details:", d$error))
        )
      } else {
        status_pill("ready", "File loaded")
      }
    })

    output$curve_placeholder <- shiny::renderUI({
      if (is.null(bca_results()))
        plot_placeholder("\U0001f4c8", "Run analysis to see standard curve")
    })

    # ---- Analysis core --------------------------------------------------
    # Extracted so we can call it from both the user-clicked Run button
    # and the auto-run trigger after a successful parse. Reads the current
    # form inputs (mode, volume, etc.) each time it's called.
    .run_analysis <- function() {
      d <- bca_data()
      if (is.null(d) || !is.null(d$error)) return(invisible(NULL))

      mode <- input$mode %||% "auto"
      if (mode == "manual" &&
          (is.null(input$manual_conc) || is.na(input$manual_conc) ||
           input$manual_conc <= 0)) {
        shiny::showNotification("Please enter a valid manual concentration.",
                                type = "warning"); return(invisible(NULL))
      }
      vol <- input$volume
      if (is.null(vol) || is.na(vol) || vol <= 0) vol <- 1.0   # safety net

      shiny::withProgress(message = "Analysing BCA\u2026", value = 0, {
        tryCatch({
          shiny::incProgress(0.3, detail = "Fitting standard curve\u2026")
          std_curve <- create_standard_curve(d$raw)

          shiny::incProgress(0.4, detail = "Calculating yield\u2026")
          manual_conc <- if (mode == "manual") input$manual_conc else NULL
          res <- calculate_protein_yield(
            file_path            = d$filepath,
            std_curve            = std_curve,
            volume_ml            = vol,
            digits               = input$digits %||% 2,
            title                = input$title %||% "",
            manual_concentration = manual_conc
          )
          res_row   <- as.data.frame(res$data)[1, ]
          conc_val  <- as.numeric(res_row[[1]])
          vol_val   <- as.numeric(res_row[[2]])
          yield_val <- as.numeric(res_row[[3]])

          shiny::incProgress(0.3, detail = "Rendering\u2026")
          bca_results(list(
            curve     = std_curve,
            conc      = conc_val,
            vol       = vol_val,
            yield     = yield_val,
            gt        = res$gt,
            title     = input$title %||% "",
            file_name = d$name
          ))
        }, error = function(e) {
          shiny::showNotification(paste("Analysis error:", conditionMessage(e)),
                                  type = "error", duration = 10)
        })
      })
    }

    # Trigger 1: user clicked "Run Analysis"
    shiny::observeEvent(input$run, .run_analysis())

    # Trigger 2: auto-run after a successful parse. Fires when bca_data
    # changes to a non-error value (i.e. a successful upload or example
    # load). User can still hit Run Analysis later to re-run with
    # different parameters.
    shiny::observeEvent(bca_data(), {
      d <- bca_data()
      if (is.null(d) || !is.null(d$error)) return()
      .run_analysis()
    }, ignoreInit = TRUE)

    # ---- Trigger 3: session start with bundled example -----------------
    # Same one-shot observe() pattern as AKTA. Defers until after Shiny's
    # first reactive flush so input bindings are resolved, and provides a
    # reactive context for the inner code. Self-destructs after one run.
    .load_example_file <- function() {
      cf <- .bca_example_file()
      if (!is.null(cf)) current_file(cf)
    }
    .example_loader_obs <- shiny::observe({
      .example_loader_obs$destroy()
      tryCatch(.load_example_file(), error = function(e)
        message("[BCA] example load failed: ", conditionMessage(e)))
    })

    # ---- Plot ------------------------------------------------------------
    output$curve_plot <- shiny::renderPlot({
      shiny::req(bca_results())
      std       <- bca_results()$curve
      stds_data <- std$standards
      x_pred    <- seq(min(stds_data$conc), max(stds_data$conc), length.out = 100)
      y_pred    <- stats::predict(std$model, newdata = data.frame(conc = x_pred))
      pred_df   <- data.frame(conc = x_pred, mean_signal = y_pred)

      ggplot2::ggplot(stds_data, ggplot2::aes(x = conc, y = mean_signal)) +
        ggplot2::geom_ribbon(
          data = pred_df,
          ggplot2::aes(
            ymin = mean_signal - 0.02 * diff(range(stds_data$mean_signal, na.rm = TRUE)),
            ymax = mean_signal + 0.02 * diff(range(stds_data$mean_signal, na.rm = TRUE))),
          fill = CG_PALETTE$accent, alpha = 0.10) +
        ggplot2::geom_line(data = pred_df, colour = CG_PALETTE$accent, linewidth = 1.2) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = mean_signal - sd_signal,
                       ymax = mean_signal + sd_signal),
          colour = CG_PALETTE$muted, width = 0.015, linewidth = 0.7, na.rm = TRUE) +
        ggplot2::geom_point(colour = CG_PALETTE$accent_warm, size = 4,
                            shape = 21, fill = CG_PALETTE$accent_warm, stroke = 0) +
        ggplot2::annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.4,
                          label = sprintf("R^2 = %.4f", std$r2),
                          colour = CG_PALETTE$muted, size = 4, family = "mono") +
        ggplot2::labs(title = "BCA Standard Curve",
                      x = "BSA Concentration (mg/mL)",
                      y = "Mean Absorbance") +
        theme_cg_dark()
    }, bg = CG_PALETTE$bg_card)

    # ---- Result badges & table ------------------------------------------
    output$result_badges <- shiny::renderUI({
      shiny::req(bca_results())
      r <- bca_results()
      shiny::fluidRow(
        shiny::column(4, result_badge("Concentration", sprintf("%.2f mg/mL", r$conc))),
        shiny::column(4, result_badge("Total Yield",   sprintf("%.2f mg",    r$yield), tone = "green")),
        shiny::column(4, result_badge("R\u00b2",       sprintf("%.4f",       r$curve$r2), tone = "orange"))
      )
    })

    output$download_buttons <- shiny::renderUI({
      shiny::req(bca_results())
      shiny::tagList(
        shiny::downloadButton(ns("dl_png"), "\u2193 PNG Standard Curve", class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_tbl"), "\u2193 PNG Results Table",  class = "btn-download"), " ",
        shiny::downloadButton(ns("dl_csv"), "\u2193 CSV Results",        class = "btn-download")
      )
    })

    # ---- Downloads -------------------------------------------------------
    output$dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("BCA_standard_curve", "png"),
      content  = function(file) {
        shiny::req(bca_results())
        p <- bca_results()$curve$plot + theme_cg_publication()
        ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 300, bg = "white")
      }
    )

    output$dl_tbl <- shiny::downloadHandler(
      filename = function() ts_filename("BCA_results_table", "png"),
      content  = function(file) {
        shiny::req(bca_results())
        r     <- bca_results()
        title <- if (nchar(trimws(input$title)) > 0) trimws(input$title)
                 else "BCA Assay Protein Yield Summary"
        p <- .bca_results_table_plot(
          title  = title,
          params = c("Concentration (mg/mL)", "Sample volume (mL)",
                     "Total yield (mg)",      "R\u00b2 (standard curve)"),
          vals   = c(sprintf("%.2f", r$conc), sprintf("%.2f", r$vol),
                     sprintf("%.2f", r$yield), sprintf("%.4f", r$curve$r2))
        )
        ggplot2::ggsave(file, p, width = 5.5, height = 3.0, dpi = 300, bg = "white")
      }
    )

    output$dl_csv <- shiny::downloadHandler(
      filename = function() ts_filename("BCA_results", "csv"),
      content  = function(file) {
        r <- bca_results()
        utils::write.csv(data.frame(
          Concentration_mgmL = r$conc,
          Volume_mL          = r$vol,
          Total_yield_mg     = r$yield,
          R_squared          = r$curve$r2,
          Title              = input$title,
          Date               = Sys.time()
        ), file, row.names = FALSE)
      }
    )

    # ---- Public reactive -------------------------------------------------
    # The cross-tool "Export Report" zip needs to know what's in here.
    # Return a reactive so the top-level app can read this module's state
    # without reaching into its private inputs/outputs.
    shiny::reactive(bca_results())
  })
}


# -- Internal helpers ---------------------------------------------------------
# A faithful port of the ggplot-rendered results-table image that the old
# app produced in two places (the dl_tbl handler and the export-zip path).
# Defining it once kills the duplication.
.bca_results_table_plot <- function(title, params, vals) {
  n      <- length(params)
  row_h  <- 1.0
  ys     <- seq(n - 1, 0, by = -1) * row_h
  y_hdr  <- n * row_h
  y_rule <- y_hdr + row_h
  y_ttl  <- y_rule + 0.35
  tt     <- 0.65
  df_rows <- data.frame(y = ys, param = params, val = vals)

  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = y_ttl, label = title,
                      hjust = 0, vjust = 0, size = 4.5, fontface = "bold",
                      colour = "black", family = "sans") +
    ggplot2::annotate("segment", x = 0, xend = 1, y = y_rule, yend = y_rule,
                      colour = "black", linewidth = 0.9) +
    ggplot2::annotate("text", x = 0, y = y_hdr + tt, label = "PARAMETER",
                      hjust = 0, vjust = 1, size = 2.9,
                      colour = "grey45", family = "sans") +
    ggplot2::annotate("text", x = 1, y = y_hdr + tt, label = "VALUE",
                      hjust = 1, vjust = 1, size = 2.9,
                      colour = "grey45", family = "sans") +
    ggplot2::annotate("segment", x = 0, xend = 1, y = y_hdr, yend = y_hdr,
                      colour = "grey65", linewidth = 0.4) +
    ggplot2::geom_text(data = df_rows,
                       ggplot2::aes(x = 0, y = y + tt, label = param),
                       hjust = 0, vjust = 1, size = 3.3,
                       colour = "black", family = "sans") +
    ggplot2::geom_text(data = df_rows,
                       ggplot2::aes(x = 1, y = y + tt, label = val),
                       hjust = 1, vjust = 1, size = 3.3,
                       fontface = "bold", colour = "black", family = "mono") +
    ggplot2::geom_segment(data = df_rows,
                          ggplot2::aes(x = 0, xend = 1, y = y, yend = y),
                          colour = "grey88", linewidth = 0.35) +
    ggplot2::scale_x_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-0.05, y_ttl + 0.5), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin     = ggplot2::margin(16, 24, 16, 24))
}

# Make available to the export-zip code path in app.R
bca_results_table_plot <- .bca_results_table_plot

# ---- Example data loader ---------------------------------------------------
# Returns a data.frame matching what shiny::fileInput would have produced
# for a single .xls upload. Cached for the session so repeated visits to
# the BCA tab don't redo the work. NULL if the file is missing.
#
# Unlike AKTA's example we don't need to decompress - the SoftMax export
# is a small (<10 KB) tab-separated text file, so we just copy it into
# tempdir so the parser gets a normal-looking upload path.
.bca_example_cache <- new.env(parent = emptyenv())

.bca_example_file <- function() {
  if (!is.null(.bca_example_cache$path) &&
      file.exists(.bca_example_cache$path)) {
    return(data.frame(
      name = "251114_CE_HsUCP1_Lipids_Final_Columns.xls",
      datapath = .bca_example_cache$path,
      stringsAsFactors = FALSE
    ))
  }

  # Same defensive app_dir resolution as the AKTA example loader
  app_dir_local <- if (exists("app_dir", envir = globalenv())) {
    get("app_dir", envir = globalenv())
  } else getwd()

  candidates <- unique(c(
    file.path(app_dir_local, "inst", "examples", "bca_example.xls"),
    file.path(getwd(),       "inst", "examples", "bca_example.xls"),
    file.path("inst", "examples", "bca_example.xls")
  ))
  src_path <- candidates[file.exists(candidates)][1]
  if (is.na(src_path)) return(NULL)

  tryCatch({
    out_path <- file.path(tempdir(), "251114_CE_HsUCP1_Lipids_Final_Columns.xls")
    file.copy(src_path, out_path, overwrite = TRUE)
    .bca_example_cache$path <- out_path
    data.frame(name = "251114_CE_HsUCP1_Lipids_Final_Columns.xls",
               datapath = out_path,
               stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

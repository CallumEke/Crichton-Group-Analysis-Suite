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
    shiny::div(class = "clear-button-container",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    shiny::fluidRow(
      # ----- Left column: controls (1-4) ----------------------------------
      shiny::column(4,
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
      ),

      # ----- Right column: results ----------------------------------------
      shiny::column(8,
        shiny::uiOutput(ns("result_badges")),
        lab_card(
          shiny::div(class = "lab-card-title", "\U0001f4c8  Standard Curve"),
          shiny::uiOutput(ns("curve_placeholder")),
          shiny::plotOutput(ns("curve_plot"), height = "360px")
        ),
        lab_card(
          shiny::div(class = "lab-card-title", "\U0001f4ca  Results"),
          DT::DTOutput(ns("results_table"))
        ),
        lab_card(
          shiny::div(class = "lab-card-title", "\U0001f551  Session History"),
          shiny::uiOutput(ns("history_ui")),
          shiny::uiOutput(ns("history_dl"))
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
    bca_history <- shiny::reactiveVal(list())

    # ---- Clear -----------------------------------------------------------
    shiny::observeEvent(input$clear, {
      bca_data(NULL); bca_results(NULL); bca_history(list())
      shinyjs::reset("file")
      shinyjs::reset("mode")
      shinyjs::reset("manual_conc")
      shinyjs::reset("volume")
      shinyjs::reset("title")
      shinyjs::reset("digits")
      shiny::showNotification("BCA data cleared", type = "message", duration = 2)
    })

    # ---- File upload -----------------------------------------------------
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      bca_results(NULL)
      tryCatch({
        fp       <- input$file$datapath
        raw_data <- read_softmax_bca(fp)
        if (is.null(raw_data$groups) || nrow(raw_data$groups) == 0)
          stop("No group data found. Ensure this is a valid SoftMax Pro export.")
        bca_data(list(filepath = fp, raw = raw_data, name = input$file$name))
      }, error = function(e) {
        bca_data(list(error = conditionMessage(e)))
      })
    })

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

    # ---- Run analysis ----------------------------------------------------
    shiny::observeEvent(input$run, {
      shiny::req(bca_data())
      d <- bca_data()
      if (!is.null(d$error)) {
        shiny::showNotification(d$error, type = "error"); return()
      }
      if (input$mode == "manual" &&
          (is.na(input$manual_conc) || input$manual_conc <= 0)) {
        shiny::showNotification("Please enter a valid manual concentration.",
                                type = "warning"); return()
      }

      shiny::withProgress(message = "Analysing BCA\u2026", value = 0, {
        tryCatch({
          shiny::incProgress(0.3, detail = "Fitting standard curve\u2026")
          std_curve <- create_standard_curve(d$raw)

          shiny::incProgress(0.4, detail = "Calculating yield\u2026")
          manual_conc <- if (input$mode == "manual") input$manual_conc else NULL
          res <- calculate_protein_yield(
            file_path            = d$filepath,
            std_curve            = std_curve,
            volume_ml            = input$volume,
            digits               = input$digits,
            title                = input$title,
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
            title     = input$title,
            file_name = d$name
          ))

          bca_history(c(bca_history(), list(list(
            time  = format(Sys.time(), "%H:%M:%S"),
            file  = d$name,
            conc  = conc_val,
            yield = yield_val,
            r2    = std_curve$r2,
            mode  = input$mode
          ))))

        }, error = function(e) {
          shiny::showNotification(paste("Analysis error:", conditionMessage(e)),
                                  type = "error", duration = 10)
        })
      })
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

    output$results_table <- DT::renderDT({
      shiny::req(bca_results())
      r   <- bca_results()
      fmt <- paste0("%.", input$digits, "f")
      df  <- data.frame(
        Parameter = c("Concentration (mg/mL)", "Volume (mL)",
                      "Total Yield (mg)", "R\u00b2"),
        Value     = c(sprintf(fmt, r$conc), sprintf(fmt, r$vol),
                      sprintf(fmt, r$yield), sprintf("%.4f", r$curve$r2))
      )
      DT::datatable(df, options = list(dom = "t", ordering = FALSE),
                    rownames = FALSE, class = "compact")
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

    # ---- History ---------------------------------------------------------
    output$history_ui <- shiny::renderUI({
      h <- bca_history()
      if (length(h) == 0) {
        return(shiny::p("No analyses run yet this session.",
                        style = "color: var(--muted); font-size: 0.8rem;"))
      }
      rows <- lapply(rev(h), function(e) {
        shiny::div(class = "history-row",
          style = "grid-template-columns: 80px 1fr 1fr 1fr 1fr;",
          shiny::div(class = "history-time", e$time),
          shiny::div(style = "font-size:0.78rem; color:var(--txt);", e$file),
          shiny::div(class = "history-val", sprintf("%.3f mg/mL", e$conc)),
          shiny::div(class = "history-val", style = "color: var(--accent-green);",
                     sprintf("%.3f mg", e$yield)),
          shiny::div(class = "history-val", style = "color: var(--accent-warm);",
                     sprintf("R\u00b2 %.4f", e$r2))
        )
      })
      do.call(shiny::div, rows)
    })

    output$history_dl <- shiny::renderUI({
      shiny::req(length(bca_history()) > 0)
      shiny::downloadButton(ns("history_csv"), "\u2193 Export History CSV",
                            class = "btn-download")
    })

    output$history_csv <- shiny::downloadHandler(
      filename = function() ts_filename("BCA_history", "csv"),
      content  = function(file) {
        h  <- bca_history()
        df <- do.call(rbind, lapply(h, function(e) data.frame(
          Time = e$time, File = e$file, Conc_mgmL = e$conc,
          Yield_mg = e$yield, R2 = e$r2, Mode = e$mode
        )))
        utils::write.csv(df, file, row.names = FALSE)
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

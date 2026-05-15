################################################################################
#  mod_ucp1.R  --  UCP1 Proton Conductance (Shiny module)
################################################################################
#
#  Migrated from app_v10l.R:
#    UI:     lines 1661 - 1822
#    Server: lines 3858 - 4573  (raw / processed / Excel-export)
#            lines 5928 - 6892  (calibration logic)
#
#  Workflow this tool implements:
#    1. Upload proton calibration CSV (intensity vs time, plateaus = added H+)
#       - detect plateaus, fit 1/FU ~ [H+] linear curve
#    2. Upload capacity calibration CSV (SPQ titration)
#       - detect plateaus, fit FU ~ Total_SPQ to get internal volume
#    3. Upload 12-18 raw sample traces
#       - parse each, build wide table with Time + one column per sample
#    4. Auto-compute processed data: ((1/raw) - intercept) / slope -> [H+] in mM
#    5. Export complete .xlsx with all sheets + embedded plot images +
#       pre-populated formulas in the Rates sheet
#
#  The "clearing overlay" pattern from the original is preserved: clicking
#  Clear hides the entire content div via JS, drops all reactiveVals, and
#  un-hides after a short delay. This is needed because UCP1 has heavy
#  reactives that would otherwise re-fire on the way down.
#
################################################################################

# -- UI -----------------------------------------------------------------------
ucp1_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(class = "clear-button-container",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    # Clearing overlay (rendered when clearing flag is TRUE)
    shiny::uiOutput(ns("clearing_overlay")),

    shiny::div(id = ns("content_section"),

      # ---- Row 1: Proton calibration + curve --------------------------
      shiny::fluidRow(
        shiny::column(4,
          lab_card(
            step_title(1, "Upload Proton Calibration"),
            shiny::fileInput(ns("cal_file"), NULL, accept = ".csv",
                             buttonLabel = "Browse\u2026",
                             placeholder = "Proton calibration CSV"),
            shiny::uiOutput(ns("cal_status"))
          ),
          lab_card(
            step_title(2, "Upload Capacity Calibration"),
            shiny::fileInput(ns("cap_file"), NULL, accept = ".csv",
                             buttonLabel = "Browse\u2026",
                             placeholder = "Capacity calibration CSV"),
            shiny::uiOutput(ns("cap_status"))
          ),
          lab_card(
            step_title(3, "Plot Settings"),
            shiny::numericInput(ns("line_width"), "Line width",
                                value = 1, min = 0.5, max = 3, step = 0.25),
            shiny::checkboxInput(ns("show_titles"), "Show plot titles", value = TRUE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("show_titles")),
              shiny::textInput(ns("proton_trace_title"),
                "Proton calibration trace title",
                placeholder = "Default: Intensity (a.u.)"),
              shiny::textInput(ns("proton_curve_title"),
                "Proton calibration curve title",
                placeholder = "Default: FU/Proton Calibration Curve"),
              shiny::textInput(ns("capacity_trace_title"),
                "Capacity calibration trace title",
                placeholder = "Default: Intensity (a.u.)"),
              shiny::textInput(ns("capacity_curve_title"),
                "Capacity calibration curve title",
                placeholder = "Default: Internal Volume Calibration")
            )
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4ca  Detected Plateaus"),
            shiny::uiOutput(ns("plateau_info")),
            shiny::br(),
            shiny::div(style = "max-height: 300px; overflow-y: auto;",
              shiny::tableOutput(ns("plateau_table"))),
            shiny::br(),
            shiny::uiOutput(ns("plateau_selector"))
          )
        ),
        shiny::column(8,
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4c8  Proton Calibration Trace"),
            shiny::uiOutput(ns("cal_placeholder")),
            shiny::plotOutput(ns("cal_plot"), height = "320px"),
            shiny::br(),
            shiny::uiOutput(ns("cal_download"))
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4c9  FU/Proton Calibration Curve"),
            shiny::uiOutput(ns("calibcurve_ui"))
          )
        )
      ),

      # ---- Row 2: Capacity plateaus + curve ---------------------------
      shiny::fluidRow(
        shiny::column(4,
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4ca  Detected Plateaus (Capacity)"),
            shiny::uiOutput(ns("cap_plateau_info")),
            shiny::br(),
            shiny::div(style = "max-height: 300px; overflow-y: auto;",
              shiny::tableOutput(ns("cap_plateau_table"))),
            shiny::br(),
            shiny::uiOutput(ns("cap_plateau_selector"))
          )
        ),
        shiny::column(8,
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4c8  Capacity Calibration Trace"),
            shiny::uiOutput(ns("cap_placeholder")),
            shiny::plotOutput(ns("cap_plot"), height = "320px"),
            shiny::br(),
            shiny::uiOutput(ns("cap_download"))
          ),
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4c9  Internal Volume Calibration Curve"),
            shiny::uiOutput(ns("cap_calibcurve_ui"))
          )
        )
      ),

      # ---- Row 3: Raw data --------------------------------------------
      shiny::fluidRow(style = "margin-top: 0 !important;",
        shiny::column(12,
          lab_card(
            step_title(4, "Upload Raw Sample Data"),
            shiny::fileInput(ns("raw_files"), NULL,
                             accept = ".csv", multiple = TRUE,
                             buttonLabel = "Browse\u2026",
                             placeholder = "Select 12-18 raw trace CSV files"),
            shiny::uiOutput(ns("raw_status")),
            shiny::br(),
            shiny::div(style = "max-height: 400px; overflow-y: auto; overflow-x: auto;",
              shiny::tableOutput(ns("raw_table"))),
            shiny::br(),
            shiny::uiOutput(ns("raw_download"))
          )
        )
      ),

      # ---- Row 4: Processed data --------------------------------------
      shiny::fluidRow(style = "margin-top: 0 !important;",
        shiny::column(12,
          lab_card(
            step_title(5, "Processed Data ([H+] in mM)"),
            shiny::uiOutput(ns("processed_status")),
            shiny::br(),
            shiny::div(style = "max-height: 400px; overflow-y: auto; overflow-x: auto;",
              shiny::tableOutput(ns("processed_table"))),
            shiny::br(),
            shiny::uiOutput(ns("processed_download"))
          )
        )
      ),

      # ---- Row 5: Export ----------------------------------------------
      shiny::fluidRow(
        shiny::column(12,
          lab_card(
            shiny::div(class = "lab-card-title", "\U0001f4e6  Export Complete Analysis"),
            shiny::uiOutput(ns("export_status")),
            shiny::br(),
            shiny::uiOutput(ns("export_button"))
          )
        )
      )
    )  # close content_section
  )
}


# -- Server -------------------------------------------------------------------
ucp1_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State ----------------------------------------------------------
    cal_data    <- shiny::reactiveVal(NULL)
    cap_data    <- shiny::reactiveVal(NULL)
    raw_data    <- shiny::reactiveVal(NULL)
    clearing    <- shiny::reactiveVal(FALSE)

    # ---- Clearing overlay ----------------------------------------------
    output$clearing_overlay <- shiny::renderUI({
      if (clearing()) {
        shiny::div(class = "clearing-overlay",
          shiny::div(class = "clearing-spinner"),
          shiny::div(class = "clearing-text", "Clearing data..."))
      }
    })

    # ---- Clear handler --------------------------------------------------
    # Uses the same "nuclear option" as the original: hide the entire
    # content div via JS so reactives can't re-fire during teardown.
    shiny::observeEvent(input$clear, {
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.display = 'none';",
        ns("content_section")))
      clearing(TRUE)
      Sys.sleep(0.1)

      cal_data(NULL); cap_data(NULL); raw_data(NULL)

      shiny::isolate({
        for (i in c("cal_file", "cap_file", "raw_files",
                    "line_width", "show_titles"))
          shinyjs::reset(i)
      })

      gc(); gc(); gc()
      Sys.sleep(0.5)
      clearing(FALSE)
      Sys.sleep(0.2)
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.display = 'block';",
        ns("content_section")))
      shiny::showNotification("UCP1 data cleared", type = "message", duration = 2)
    }, ignoreInit = TRUE, priority = 1000)


    # =====================================================================
    # PROTON CALIBRATION
    # =====================================================================

    shiny::observeEvent(input$cal_file, {
      shiny::req(input$cal_file)
      cal_data(NULL)
      tryCatch({
        df <- .ucp1_read_trace_csv(input$cal_file$datapath)
        cal_data(df)
      }, error = function(e) cal_data(list(error = conditionMessage(e))))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Plateau detection - shared logic for both proton and capacity calibrations
    plateaus <- shiny::reactive({
      shiny::req(!clearing())
      shiny::req(cal_data())
      d <- cal_data()
      if (!is.null(d$error)) return(NULL)
      .ucp1_detect_plateaus(d)
    })

    output$cal_status <- shiny::renderUI({
      shiny::req(!clearing(), cal_data())
      d <- cal_data()
      if (!is.null(d$error)) {
        status_pill("error", paste("Error:", d$error))
      } else {
        n_points <- nrow(d)
        t_range  <- range(d$Time, na.rm = TRUE)
        plat     <- plateaus()
        shiny::div(
          status_pill("ready", sprintf("%d points | %.1f - %.1f s",
                                       n_points, t_range[1], t_range[2])),
          if (!is.null(plat))
            status_pill("ready", sprintf("%d plateaus detected",
                                         nrow(plat$plateau_summary)))
        )
      }
    })

    output$plateau_info <- shiny::renderUI({
      shiny::req(!clearing())
      plat <- plateaus()
      if (is.null(plat)) {
        shiny::div(style = "color:#7A8FAD;font-size:0.9em;padding:0.5rem;",
          "Upload proton calibration data to detect plateaus")
      } else {
        shiny::div(style = "color:#22C55E;font-size:0.9em;padding:0.5rem;",
          sprintf("\u2713 %d plateau regions detected and quantified",
                  nrow(plat$plateau_summary)))
      }
    })

    output$plateau_table <- shiny::renderTable({
      shiny::req(!clearing(), plateaus())
      plat <- shiny::isolate(plateaus())
      df <- plat$plateau_summary
      df$time_range <- sprintf("%.1f - %.1f s", df$time_start, df$time_end)
      data.frame(
        "Plateau"          = df$plateau_number,
        "Time Range"       = df$time_range,
        "Median Intensity" = round(df$median_intensity, 2),
        "Trimmed Mean"     = round(df$trimmed_mean, 2),
        "SD"               = round(df$sd, 2),
        "Points"           = df$n_points,
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
       width = "100%", align = "c")

    output$plateau_selector <- shiny::renderUI({
      shiny::req(!clearing())
      plat <- plateaus()
      if (is.null(plat)) return(NULL)
      n_total   <- nrow(plat$plateau_summary)
      default_n <- max(2, n_total - 1)  # Default: exclude last plateau
      shiny::tagList(
        shiny::div(style = "margin-top:1rem;"),
        shiny::numericInput(ns("n_plateaus"),
          "Number of plateaus to use for calibration",
          value = default_n, min = 2, max = n_total, step = 1),
        shiny::div(style = "color:#7A8FAD;font-size:0.85em;margin-top:-0.5rem;",
          sprintf("Total detected: %d | Recommended: %d (excludes last plateau)",
                  n_total, default_n))
      )
    })

    output$cal_placeholder <- shiny::renderUI({
      shiny::req(!clearing())
      if (is.null(cal_data()))
        plot_placeholder("\U0001f4c8",
          "Upload a proton calibration CSV file to visualize the trace")
    })

    output$cal_plot <- shiny::renderPlot({
      shiny::req(!clearing(), cal_data())
      d <- cal_data()
      if (!is.null(d$error)) return(NULL)
      lw       <- shiny::isolate(input$line_width) %||% 1
      title_in <- shiny::isolate(input$proton_trace_title)
      title    <- if (isTRUE(input$show_titles))
                    if (!is.null(title_in) && nchar(title_in) > 0) title_in
                    else "Intensity (a.u.)"
                  else NULL
      .ucp1_trace_plot(d, lw = lw, title = title,
                       line_colour = "#2E5CB8", dark = TRUE)
    }, bg = CG_PALETTE$bg_card)

    output$cal_download <- shiny::renderUI({
      shiny::req(!clearing(), cal_data())
      d <- cal_data()
      if (!is.null(d$error)) return(NULL)
      shiny::downloadButton(ns("cal_dl_png"), "\u2193 PNG Plot",
                            class = "btn-download")
    })

    output$cal_dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_proton_calibration", "png"),
      content  = function(file) {
        shiny::req(cal_data())
        d        <- cal_data()
        lw       <- shiny::isolate(input$line_width) %||% 1
        title_in <- shiny::isolate(input$proton_trace_title)
        title    <- if (isTRUE(input$show_titles))
                      if (!is.null(title_in) && nchar(title_in) > 0) title_in
                      else "Intensity (a.u.)"
                    else NULL
        p <- .ucp1_trace_plot(d, lw = lw, title = title,
                              line_colour = "#2E5CB8", dark = FALSE)
        ggplot2::ggsave(file, p, width = 10, height = 5, dpi = 300, bg = "white")
      }
    )

    # ---- Proton calibration regression ----------------------------------
    calibration <- shiny::reactive({
      shiny::req(!clearing(), plateaus(), input$n_plateaus)
      plat    <- plateaus()
      n_use   <- as.integer(input$n_plateaus)
      n_total <- nrow(plat$plateau_summary)
      if (n_use < 2 || n_use > n_total) return(NULL)

      # Fixed H+ concentration series (mM): 0, 4, 8, 12, 16, 20, ...
      h_conc   <- seq(0, (n_use - 1) * 4, by = 4)
      fu_vals  <- plat$plateau_summary$median_intensity[1:n_use]
      inv_fu   <- 1 / fu_vals
      calib_df <- data.frame(H_conc = h_conc, inv_FU = inv_fu)
      lm_fit   <- stats::lm(inv_FU ~ H_conc, data = calib_df)

      list(data      = calib_df,
           fu_values = fu_vals,
           model     = lm_fit,
           slope     = stats::coef(lm_fit)[2],
           intercept = stats::coef(lm_fit)[1],
           r_squared = summary(lm_fit)$r.squared,
           n_used    = n_use)
    })

    output$calibcurve_ui <- shiny::renderUI({
      shiny::req(!clearing())
      calib <- calibration()
      if (is.null(calib)) {
        return(plot_placeholder("\U0001f4c9",
          "Configure plateau selection to generate calibration curve"))
      }
      shiny::tagList(
        shiny::plotOutput(ns("calibcurve_plot"), height = "300px"),
        shiny::div(style = "background:#1E2D45;padding:0.6rem;border-radius:8px;margin-top:0.5rem;",
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(style = "font-weight:bold;color:#E8F0FE;margin-bottom:0.5rem;",
                "Calibration Parameters"),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("Slope: %.6f", calib$slope)),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("Intercept: %.5f", calib$intercept))),
            shiny::column(6,
              shiny::div(style = "font-weight:bold;color:#E8F0FE;margin-bottom:0.5rem;",
                "Fit Quality"),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("R\u00b2 = %.4f", calib$r_squared)),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("Points used: %d", calib$n_used)))
          )),
        shiny::div(style = "margin-top:0.5rem;",
          shiny::downloadButton(ns("calib_dl_png"),
            "\u2193 PNG Calibration Curve", class = "btn-download"))
      )
    })

    output$calibcurve_plot <- shiny::renderPlot({
      shiny::req(!clearing(), calibration())
      calib    <- calibration()
      title_in <- shiny::isolate(input$proton_curve_title)
      title    <- if (isTRUE(input$show_titles))
                    if (!is.null(title_in) && nchar(title_in) > 0) title_in
                    else "FU/Proton Calibration Curve"
                  else NULL
      .ucp1_calib_curve_plot(calib$data, x = "H_conc", y = "inv_FU",
                             slope = calib$slope, intercept = calib$intercept,
                             r2 = calib$r_squared, title = title,
                             xlab = "[H+] (mM)", ylab = "1/FU",
                             eq_fmt = "y = %.6fx + %.5f", dark = TRUE)
    }, bg = CG_PALETTE$bg_card)

    output$calib_dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_calibration_curve", "png"),
      content  = function(file) {
        shiny::req(calibration())
        calib    <- calibration()
        title_in <- shiny::isolate(input$proton_curve_title)
        title    <- if (isTRUE(input$show_titles))
                      if (!is.null(title_in) && nchar(title_in) > 0) title_in
                      else "FU/Proton Calibration Curve"
                    else NULL
        p <- .ucp1_calib_curve_plot(calib$data, x = "H_conc", y = "inv_FU",
                                    slope = calib$slope, intercept = calib$intercept,
                                    r2 = calib$r_squared, title = title,
                                    xlab = "[H+] (mM)", ylab = "1/FU",
                                    eq_fmt = "y = %.6fx + %.5f", dark = FALSE)
        ggplot2::ggsave(file, p, width = 6, height = 5, dpi = 300, bg = "white")
      })


    # =====================================================================
    # CAPACITY CALIBRATION  (mirrors proton calibration above)
    # =====================================================================

    shiny::observeEvent(input$cap_file, {
      shiny::req(input$cap_file)
      cap_data(NULL)
      tryCatch({
        df <- .ucp1_read_trace_csv(input$cap_file$datapath)
        cap_data(df)
      }, error = function(e) cap_data(list(error = conditionMessage(e))))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    cap_plateaus <- shiny::reactive({
      shiny::req(!clearing(), cap_data())
      d <- cap_data()
      if (!is.null(d$error)) return(NULL)
      .ucp1_detect_plateaus(d)
    })

    output$cap_status <- shiny::renderUI({
      shiny::req(!clearing(), cap_data())
      d <- cap_data()
      if (!is.null(d$error)) {
        status_pill("error", paste("Error:", d$error))
      } else {
        n_points <- nrow(d); t_range <- range(d$Time, na.rm = TRUE)
        plat     <- cap_plateaus()
        shiny::div(
          status_pill("ready", sprintf("%d points | %.1f - %.1f s",
                                       n_points, t_range[1], t_range[2])),
          if (!is.null(plat))
            status_pill("ready", sprintf("%d plateaus detected",
                                         nrow(plat$plateau_summary)))
        )
      }
    })

    output$cap_plateau_info <- shiny::renderUI({
      shiny::req(!clearing())
      plat <- cap_plateaus()
      if (is.null(plat)) {
        shiny::div(style = "color:#7A8FAD;font-size:0.9em;padding:0.5rem;",
          "Upload capacity calibration data to detect plateaus")
      } else {
        shiny::div(style = "color:#22C55E;font-size:0.9em;padding:0.5rem;",
          sprintf("\u2713 %d plateau regions detected",
                  nrow(plat$plateau_summary)))
      }
    })

    output$cap_plateau_selector <- shiny::renderUI({
      shiny::req(!clearing())
      plat <- cap_plateaus()
      if (is.null(plat)) return(NULL)
      n_total <- nrow(plat$plateau_summary)
      shiny::tagList(
        shiny::div(style = "margin-top:1rem;"),
        shiny::numericInput(ns("n_cap_plateaus"),
          "Number of plateaus to use for calibration",
          value = n_total, min = 2, max = n_total, step = 1),
        shiny::div(style = "color:#7A8FAD;font-size:0.85em;margin-top:-0.5rem;",
          sprintf("Total detected: %d | Using all plateaus", n_total))
      )
    })

    output$cap_plateau_table <- shiny::renderTable({
      shiny::req(!clearing(), cap_plateaus())
      plat <- shiny::isolate(cap_plateaus())
      df <- plat$plateau_summary
      df$time_range <- sprintf("%.1f - %.1f s", df$time_start, df$time_end)
      data.frame(
        "Plateau"          = df$plateau_number,
        "Time Range"       = df$time_range,
        "Median Intensity" = round(df$median_intensity, 2),
        "Trimmed Mean"     = round(df$trimmed_mean, 2),
        "SD"               = round(df$sd, 2),
        "Points"           = df$n_points,
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
       width = "100%", align = "c")

    output$cap_placeholder <- shiny::renderUI({
      shiny::req(!clearing())
      if (is.null(cap_data()))
        plot_placeholder("\U0001f4c8", "Upload capacity calibration CSV to visualize")
    })

    output$cap_plot <- shiny::renderPlot({
      shiny::req(!clearing(), cap_data())
      d <- cap_data()
      if (!is.null(d$error)) return(NULL)
      lw       <- shiny::isolate(input$line_width) %||% 1
      title_in <- shiny::isolate(input$capacity_trace_title)
      title    <- if (isTRUE(input$show_titles))
                    if (!is.null(title_in) && nchar(title_in) > 0) title_in
                    else "Intensity (a.u.)"
                  else NULL
      .ucp1_trace_plot(d, lw = lw, title = title,
                       line_colour = "#2E5CB8", dark = TRUE)
    }, bg = CG_PALETTE$bg_card)

    output$cap_download <- shiny::renderUI({
      shiny::req(!clearing(), cap_data())
      d <- cap_data()
      if (!is.null(d$error)) return(NULL)
      shiny::downloadButton(ns("cap_dl_png"), "\u2193 PNG Plot",
                            class = "btn-download")
    })

    output$cap_dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_capacity_calibration", "png"),
      content  = function(file) {
        shiny::req(cap_data())
        d        <- cap_data()
        lw       <- shiny::isolate(input$line_width) %||% 1
        title_in <- shiny::isolate(input$capacity_trace_title)
        title    <- if (isTRUE(input$show_titles))
                      if (!is.null(title_in) && nchar(title_in) > 0) title_in
                      else "Intensity (a.u.)"
                    else NULL
        p <- .ucp1_trace_plot(d, lw = lw, title = title,
                              line_colour = "#2E5CB8", dark = FALSE)
        ggplot2::ggsave(file, p, width = 10, height = 5, dpi = 300, bg = "white")
      })

    cap_calibration <- shiny::reactive({
      shiny::req(!clearing(), cap_plateaus(), input$n_cap_plateaus)
      plat    <- cap_plateaus()
      n_use   <- as.integer(input$n_cap_plateaus)
      n_total <- nrow(plat$plateau_summary)
      if (n_use < 2 || n_use > n_total) return(NULL)

      # SPQ series: 0, 1, 2, 3 ... µM cumulative
      total_spq <- seq(0, n_use - 1, by = 1)
      fu_vals   <- plat$plateau_summary$median_intensity[1:n_use]
      calib_df  <- data.frame(Total_SPQ = total_spq, FU = fu_vals)
      lm_fit    <- stats::lm(FU ~ Total_SPQ, data = calib_df)

      list(data = calib_df, model = lm_fit,
           slope = stats::coef(lm_fit)[2],
           intercept = stats::coef(lm_fit)[1],
           r_squared = summary(lm_fit)$r.squared,
           n_used = n_use)
    })

    cap_summary <- shiny::reactive({
      shiny::req(!clearing(), cap_calibration(), cap_plateaus())
      cap_cal    <- cap_calibration()
      plateaus_d <- cap_plateaus()$plateau_summary

      fu_per_um   <- cap_cal$slope               # FU/µM = slope
      initial_fu  <- plateaus_d$median_intensity[1]
      um          <- initial_fu / fu_per_um      # µM = initial_FU / (FU/µM)
      ul_per_75ul <- 500 / (2000 / um)           # µL/75 µL sample

      data.frame(
        Parameter = c("FU/\u00b5M", "Initial FU", "\u00b5M", "\u00b5L/75 \u00b5L sample"),
        Value     = c(fu_per_um, initial_fu, um, ul_per_75ul)
      )
    })

    output$cap_calibcurve_ui <- shiny::renderUI({
      shiny::req(!clearing())
      calib <- cap_calibration()
      if (is.null(calib))
        return(plot_placeholder("\U0001f4c9",
          "Configure plateau selection to generate calibration"))
      cs <- cap_summary()
      shiny::tagList(
        shiny::plotOutput(ns("cap_calibcurve_plot"), height = "300px"),
        shiny::div(style = "background:#1E2D45;padding:0.6rem;border-radius:8px;margin-top:0.5rem;",
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(style = "font-weight:bold;color:#E8F0FE;margin-bottom:0.5rem;",
                "Calibration Parameters"),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("Slope (a): %.3f", calib$slope)),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("Intercept (b): %.3f", calib$intercept)),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("R\u00b2 = %.4f", calib$r_squared))),
            shiny::column(6,
              shiny::div(style = "font-weight:bold;color:#E8F0FE;margin-bottom:0.5rem;",
                "Summary Values"),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("FU/\u00b5M: %.2f", cs$Value[1])),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("Initial FU: %.2f", cs$Value[2])),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("\u00b5M: %.2f", cs$Value[3])),
              shiny::div(style = "color:#7A8FAD;font-size:0.9em;",
                sprintf("\u00b5L/75 \u00b5L sample: %.2f", cs$Value[4])))
          )),
        shiny::div(style = "margin-top:0.5rem;",
          shiny::downloadButton(ns("cap_calib_dl_png"),
            "\u2193 PNG Calibration Curve", class = "btn-download"))
      )
    })

    output$cap_calibcurve_plot <- shiny::renderPlot({
      shiny::req(!clearing(), cap_calibration())
      calib    <- cap_calibration()
      title_in <- shiny::isolate(input$capacity_curve_title)
      title    <- if (isTRUE(input$show_titles))
                    if (!is.null(title_in) && nchar(title_in) > 0) title_in
                    else "Internal Volume Calibration"
                  else NULL
      .ucp1_calib_curve_plot(calib$data, x = "Total_SPQ", y = "FU",
                             slope = calib$slope, intercept = calib$intercept,
                             r2 = calib$r_squared, title = title,
                             xlab = "Total SPQ (\u00b5M)", ylab = "FU",
                             eq_fmt = "y = %.3fx + %.3f", dark = TRUE)
    }, bg = CG_PALETTE$bg_card)

    output$cap_calib_dl_png <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_capacity_curve", "png"),
      content  = function(file) {
        shiny::req(cap_calibration())
        calib    <- cap_calibration()
        title_in <- shiny::isolate(input$capacity_curve_title)
        title    <- if (isTRUE(input$show_titles))
                      if (!is.null(title_in) && nchar(title_in) > 0) title_in
                      else "Internal Volume Calibration"
                    else NULL
        p <- .ucp1_calib_curve_plot(calib$data, x = "Total_SPQ", y = "FU",
                                    slope = calib$slope, intercept = calib$intercept,
                                    r2 = calib$r_squared, title = title,
                                    xlab = "Total SPQ (\u00b5M)", ylab = "FU",
                                    eq_fmt = "y = %.3fx + %.3f", dark = FALSE)
        ggplot2::ggsave(file, p, width = 6, height = 5, dpi = 300, bg = "white")
      })


    # =====================================================================
    # RAW DATA  (multi-file upload, combined wide-format)
    # =====================================================================

    shiny::observeEvent(input$raw_files, {
      shiny::req(input$raw_files, !clearing())
      raw_data(NULL)
      tryCatch({
        raw_data(.ucp1_combine_raw_files(input$raw_files))
      }, error = function(e) raw_data(list(error = conditionMessage(e))))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    output$raw_status <- shiny::renderUI({
      shiny::req(!clearing(), raw_data())
      d <- raw_data()
      if (!is.null(d$error)) {
        status_pill("error", paste("Error:", d$error))
      } else {
        n_samples <- ncol(d) - 1L
        n_points  <- nrow(d)
        t_range   <- range(d$Time, na.rm = TRUE)
        shiny::div(
          status_pill("ready", sprintf("%d samples loaded", n_samples)),
          status_pill("ready", sprintf("%d data points per sample (%.1f - %.1f s)",
                                       n_points, t_range[1], t_range[2]))
        )
      }
    })

    output$raw_table <- shiny::renderTable({
      shiny::req(!clearing(), raw_data())
      d <- shiny::isolate(raw_data())
      if (!is.null(d$error)) return(NULL)
      d$Time <- round(d$Time, 1)
      for (col in names(d)[-1]) d[[col]] <- round(d[[col]], 2)
      d
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
       width = "100%", align = "c", digits = 2)

    output$raw_download <- shiny::renderUI({
      shiny::req(!clearing(), raw_data())
      d <- raw_data()
      if (!is.null(d$error)) return(NULL)
      shiny::downloadButton(ns("raw_dl_csv"),
        "\u2193 CSV for GraphPad Prism", class = "btn-download")
    })

    output$raw_dl_csv <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_raw_data", "csv"),
      content  = function(file) {
        shiny::req(raw_data())
        d <- raw_data(); d$Time <- round(d$Time, 1)
        utils::write.csv(d, file, row.names = FALSE)
      })


    # =====================================================================
    # PROCESSED DATA  (raw -> [H+] mM via proton calibration)
    # =====================================================================

    processed_data <- shiny::reactive({
      shiny::req(!clearing(), raw_data(), calibration())
      raw <- raw_data()
      if (!is.null(raw$error)) return(NULL)
      cal <- calibration()
      processed <- raw
      for (col in names(raw)[-1])
        processed[[col]] <- ((1 / raw[[col]]) - cal$intercept) / cal$slope
      processed
    })

    output$processed_status <- shiny::renderUI({
      shiny::req(!clearing())
      proc <- processed_data()
      if (is.null(proc)) {
        shiny::div(style = "color:#7A8FAD;font-size:0.9em;padding:0.5rem;",
          "Upload raw data and complete proton calibration to generate processed data")
      } else {
        n_samples <- ncol(proc) - 1L
        shiny::div(
          status_pill("ready", sprintf("%d samples processed", n_samples)),
          status_pill("ready", "Converted to [H+] (mM) using proton calibration")
        )
      }
    })

    output$processed_table <- shiny::renderTable({
      shiny::req(!clearing(), processed_data())
      proc <- shiny::isolate(processed_data())
      proc$Time <- round(proc$Time, 1)
      for (col in names(proc)[-1]) proc[[col]] <- round(proc[[col]], 4)
      proc
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
       width = "100%", align = "c", digits = 4)

    output$processed_download <- shiny::renderUI({
      shiny::req(!clearing(), processed_data())
      shiny::downloadButton(ns("processed_dl_csv"),
        "\u2193 CSV Processed Data", class = "btn-download")
    })

    output$processed_dl_csv <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_processed_data", "csv"),
      content  = function(file) {
        shiny::req(processed_data())
        proc <- processed_data(); proc$Time <- round(proc$Time, 1)
        utils::write.csv(proc, file, row.names = FALSE)
      })


    # =====================================================================
    # COMPLETE EXCEL EXPORT
    # =====================================================================

    export_ready <- shiny::reactive({
      list(proton_cal    = !is.null(calibration()),
           capacity_cal  = !is.null(cap_calibration()),
           raw_data      = !is.null(raw_data()),
           processed_data= !is.null(processed_data()))
    })

    output$export_status <- shiny::renderUI({
      shiny::req(!clearing())
      ready <- export_ready()
      if (all(unlist(ready))) {
        shiny::div(
          status_pill("ready", "\u2713 All data available - ready to export"),
          shiny::div(style = "color:#7A8FAD;font-size:0.9em;margin-top:0.5rem;",
            "Complete Excel analysis file with all sheets populated"))
      } else {
        missing <- names(ready)[!unlist(ready)]
        shiny::div(
          shiny::div(class = "status-pill warning",
            shiny::div(class = "dot"),
            sprintf("Missing: %s",
                    paste(gsub("_", " ", missing), collapse = ", "))),
          shiny::div(style = "color:#7A8FAD;font-size:0.9em;margin-top:0.5rem;",
            "Complete all steps above to export analysis file"))
      }
    })

    output$export_button <- shiny::renderUI({
      shiny::req(!clearing())
      if (!all(unlist(export_ready()))) return(NULL)
      shiny::downloadButton(ns("export_xlsx"),
        "\u2193 Download Complete Analysis (.xlsx)",
        class = "btn-download",
        style = "font-size:1.1em;padding:0.8rem 1.5rem;")
    })

    output$export_xlsx <- shiny::downloadHandler(
      filename = function() ts_filename("UCP1_Analysis_Complete", "xlsx"),
      content  = function(file) {
        shiny::req(cal_data(), calibration(),
                   cap_calibration(), cap_summary(),
                   raw_data(), processed_data())
        .ucp1_build_excel(
          file        = file,
          raw         = raw_data(),
          processed   = processed_data(),
          proton_raw  = cal_data(),
          proton_cal  = calibration(),
          proton_plat = plateaus()$plateau_summary,
          cap_raw     = cap_data(),
          cap_cal     = cap_calibration(),
          cap_plat    = cap_plateaus()$plateau_summary,
          cap_sum     = cap_summary()
        )
      })

    # ---- Public reactive (for cross-tool export) ------------------------
    shiny::reactive({
      if (!all(unlist(export_ready()))) return(NULL)
      list(calibration     = calibration(),
           cap_calibration = cap_calibration(),
           cap_summary     = cap_summary(),
           raw             = raw_data(),
           processed       = processed_data())
    })
  })
}


# -- Internal helpers ---------------------------------------------------------
# Shared helpers, defined once and reused by both proton/capacity paths.

# Parse one of the SPQ-style fluorimeter trace CSVs:
#   line 1: "Proton Cal," (header)    -- skipped
#   line 2: column headers            -- skipped
#   lines 3+: time, intensity
.ucp1_read_trace_csv <- function(filepath) {
  lines      <- readLines(filepath, warn = FALSE)
  data_lines <- lines[3:length(lines)]
  parsed     <- strsplit(data_lines, ",")
  df <- data.frame(
    Time      = sapply(parsed, function(x) as.numeric(x[1])),
    Intensity = sapply(parsed, function(x) as.numeric(x[2]))
  )
  stats::na.omit(df)
}

# Detect plateau regions: intensity < 10% of max counts as "gap", everything
# else is a candidate plateau. Returns a data.frame summary or NULL if no
# plateau survives the 3-point minimum.
.ucp1_detect_plateaus <- function(d) {
  threshold <- max(d$Intensity, na.rm = TRUE) * 0.1
  d$is_gap  <- d$Intensity < threshold
  rle_res   <- rle(d$is_gap)
  seg_id    <- rep(seq_along(rle_res$lengths), rle_res$lengths)
  d$segment <- seg_id

  plateau_segs <- unique(seg_id[!d$is_gap])
  stats_list <- lapply(plateau_segs, function(seg) {
    sd_block <- d[d$segment == seg & !d$is_gap, ]
    if (nrow(sd_block) < 3) return(NULL)
    list(segment          = seg,
         n_points         = nrow(sd_block),
         time_start       = min(sd_block$Time, na.rm = TRUE),
         time_end         = max(sd_block$Time, na.rm = TRUE),
         median_intensity = stats::median(sd_block$Intensity, na.rm = TRUE),
         trimmed_mean     = mean(sd_block$Intensity, trim = 0.1, na.rm = TRUE),
         sd               = stats::sd(sd_block$Intensity, na.rm = TRUE))
  })
  stats_list <- stats_list[!sapply(stats_list, is.null)]
  if (length(stats_list) == 0) return(NULL)

  plateau_df <- do.call(rbind, lapply(seq_along(stats_list), function(i) {
    x <- stats_list[[i]]
    data.frame(plateau_number   = i,
               segment          = x$segment,
               n_points         = x$n_points,
               time_start       = x$time_start,
               time_end         = x$time_end,
               time_mid         = mean(c(x$time_start, x$time_end)),
               median_intensity = x$median_intensity,
               trimmed_mean     = x$trimmed_mean,
               sd               = x$sd)
  }))
  list(data_with_segments = d, plateau_summary = plateau_df)
}

# Combine multiple raw-sample CSVs into a single wide table.
# Sample name is the text after the final underscore in each filename.
.ucp1_combine_raw_files <- function(files) {
  n <- nrow(files)
  per_file <- lapply(seq_len(n), function(i) {
    df <- .ucp1_read_trace_csv(files$datapath[i])
    df <- df[df$Time >= 0.1 & df$Time <= 140.11, ]   # original hard cap
    fname_no_ext <- sub("\\.csv$", "", files$name[i], ignore.case = TRUE)
    parts <- strsplit(fname_no_ext, "_")[[1]]
    list(name = parts[length(parts)], data = df)
  })

  time_col <- per_file[[1]]$data$Time
  combined <- data.frame(Time = time_col)
  for (i in seq_along(per_file)) {
    sample_df <- per_file[[i]]$data
    matched   <- rep(NA_real_, length(time_col))
    for (j in seq_along(time_col)) {
      d_diff <- abs(sample_df$Time - time_col[j])
      idx    <- which.min(d_diff)
      if (d_diff[idx] < 0.05)   # 50 ms tolerance, as in original
        matched[j] <- sample_df$Intensity[idx]
    }
    combined[[ per_file[[i]]$name ]] <- matched
  }
  combined
}

# Shared trace plot. `dark = TRUE` -> dark theme for screen; `dark = FALSE` ->
# white theme for PNG export.
.ucp1_trace_plot <- function(d, lw, title, line_colour, dark = TRUE) {
  base_theme <- ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title  = ggplot2::element_text(face = "bold", size = 11),
      axis.text   = ggplot2::element_text(size = 10),
      axis.line   = ggplot2::element_line(linewidth = 0.6),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3))

  theme_colours <- if (dark) {
    ggplot2::theme(
      plot.title       = ggplot2::element_text(colour = CG_PALETTE$txt),
      axis.title       = ggplot2::element_text(colour = CG_PALETTE$txt),
      axis.text        = ggplot2::element_text(colour = CG_PALETTE$muted),
      axis.line        = ggplot2::element_line(colour = CG_PALETTE$muted),
      panel.grid.major = ggplot2::element_line(colour = CG_PALETTE$border),
      plot.background  = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
      panel.background = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA))
  } else {
    ggplot2::theme(
      plot.title       = ggplot2::element_text(colour = "black"),
      axis.title       = ggplot2::element_text(colour = "black"),
      axis.text        = ggplot2::element_text(colour = "black"),
      axis.line        = ggplot2::element_line(colour = "black"),
      panel.grid.major = ggplot2::element_line(colour = "grey85"),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA))
  }

  ggplot2::ggplot(d, ggplot2::aes(x = Time, y = Intensity)) +
    ggplot2::geom_line(colour = line_colour, linewidth = lw) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05)),
                                breaks = scales::pretty_breaks(n = 7)) +
    ggplot2::labs(title = title, x = "Time (s)", y = "Intensity (a.u.)") +
    base_theme + theme_colours
}

# Shared linear-fit plot for both proton and capacity calibrations.
.ucp1_calib_curve_plot <- function(data, x, y, slope, intercept, r2,
                                   title, xlab, ylab, eq_fmt, dark = TRUE) {
  txt_colour <- if (dark) CG_PALETTE$txt else "black"
  ann_label  <- sprintf("%s\nR\u00b2 = %.4f", sprintf(eq_fmt, slope, intercept), r2)

  base_theme <- ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title       = ggplot2::element_text(face = "bold", size = 11),
      axis.text        = ggplot2::element_text(size = 10),
      axis.line        = ggplot2::element_line(linewidth = 0.6),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3))

  theme_colours <- if (dark) {
    ggplot2::theme(
      plot.title       = ggplot2::element_text(colour = CG_PALETTE$txt),
      axis.title       = ggplot2::element_text(colour = CG_PALETTE$txt),
      axis.text        = ggplot2::element_text(colour = CG_PALETTE$muted),
      axis.line        = ggplot2::element_line(colour = CG_PALETTE$muted),
      panel.grid.major = ggplot2::element_line(colour = CG_PALETTE$border),
      plot.background  = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
      panel.background = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA))
  } else {
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA))
  }

  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(size = 3, colour = "#2E5CB8") +
    ggplot2::geom_smooth(method = "lm", se = FALSE,
                         colour = "#2E5CB8", linewidth = 1,
                         formula = y ~ x) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.1))) +
    ggplot2::annotate("text", x = Inf, y = Inf, label = ann_label,
                      hjust = 1.1, vjust = 1.5, size = 4, colour = txt_colour) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    base_theme + theme_colours
}

# Build the complete .xlsx export.
# Verbatim port of the original (lines 4114-4570) - the layout is precise
# because users paste Plateau/Top/K values from Prism into fixed cells.
.ucp1_build_excel <- function(file, raw, processed,
                              proton_raw, proton_cal, proton_plat,
                              cap_raw,    cap_cal,    cap_plat, cap_sum) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    utils::install.packages("openxlsx", repos = "https://cloud.r-project.org/")
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Raw data")
  openxlsx::addWorksheet(wb, "Processed data")
  openxlsx::addWorksheet(wb, "Proton calibration")
  openxlsx::addWorksheet(wb, "Capacity calibration")
  openxlsx::addWorksheet(wb, "Rates")

  # Raw + processed
  raw$Time       <- round(raw$Time, 1)
  processed$Time <- round(processed$Time, 1)
  openxlsx::writeData(wb, "Raw data",       raw)
  openxlsx::writeData(wb, "Processed data", processed)

  # ---- Proton calibration sheet --------------------------------------
  # Columns A-B: raw trace
  openxlsx::writeData(wb, "Proton calibration", "Time (s)",         startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "Proton calibration", "Intensity (a.u.)", startRow = 1, startCol = 2)
  openxlsx::writeData(wb, "Proton calibration", round(proton_raw$Time, 1),
                      startRow = 2, startCol = 1)
  openxlsx::writeData(wb, "Proton calibration", round(proton_raw$Intensity, 4),
                      startRow = 2, startCol = 2)

  # FU/proton table at column M
  openxlsx::writeData(wb, "Proton calibration", "FU/proton", startRow = 5, startCol = 13)
  openxlsx::writeData(wb, "Proton calibration", "Relative change in conc (mM)",
                      startRow = 6, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration", "Addition",
                      startRow = 7, startCol = 13)
  openxlsx::writeData(wb, "Proton calibration", "H+ stock",
                      startRow = 7, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration", "[H+]",
                      startRow = 7, startCol = 15)
  openxlsx::writeData(wb, "Proton calibration", "FU",
                      startRow = 7, startCol = 16)
  openxlsx::writeData(wb, "Proton calibration", "1/FU",
                      startRow = 7, startCol = 17)
  openxlsx::writeData(wb, "Proton calibration", "(\u00b5L)",
                      startRow = 8, startCol = 13)
  openxlsx::writeData(wb, "Proton calibration", "(mM)",
                      startRow = 8, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration", "(mM)",
                      startRow = 8, startCol = 15)

  n_plateaus <- length(proton_cal$fu_values)
  openxlsx::writeData(wb, "Proton calibration",
                      c(0, rep(1,    n_plateaus - 1)), startRow = 9, startCol = 13)
  openxlsx::writeData(wb, "Proton calibration",
                      c(0, rep(2000, n_plateaus - 1)), startRow = 9, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration",
                      proton_cal$data$H_conc,  startRow = 9, startCol = 15)
  openxlsx::writeData(wb, "Proton calibration",
                      proton_cal$fu_values,    startRow = 9, startCol = 16)
  openxlsx::writeData(wb, "Proton calibration",
                      proton_cal$data$inv_FU,  startRow = 9, startCol = 17)

  last_data_row <- 9 + n_plateaus
  openxlsx::writeData(wb, "Proton calibration", "Addition:",
                      startRow = last_data_row + 2, startCol = 13)
  openxlsx::writeData(wb, "Proton calibration", "1\u00b5L nigericin (0.5mM)",
                      startRow = last_data_row + 2, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration", "H+ stock:",
                      startRow = last_data_row + 3, startCol = 13)
  openxlsx::writeData(wb, "Proton calibration", "1M H2SO4",
                      startRow = last_data_row + 3, startCol = 14)

  openxlsx::writeData(wb, "Proton calibration", "a",        startRow = 25, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration", proton_cal$slope,
                      startRow = 25, startCol = 15)
  openxlsx::writeData(wb, "Proton calibration", "b",        startRow = 26, startCol = 14)
  openxlsx::writeData(wb, "Proton calibration", proton_cal$intercept,
                      startRow = 26, startCol = 15)
  openxlsx::writeData(wb, "Proton calibration", "y=ax+b",   startRow = 25, startCol = 16)
  openxlsx::writeData(wb, "Proton calibration", "x=(y-b)/a", startRow = 26, startCol = 16)

  # ---- Capacity calibration sheet -------------------------------------
  openxlsx::writeData(wb, "Capacity calibration", "Time (s)",
                      startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "Capacity calibration", "Intensity (a.u.)",
                      startRow = 2, startCol = 2)
  openxlsx::writeData(wb, "Capacity calibration",
                      round(cap_raw$Time, 1),      startRow = 3, startCol = 1)
  openxlsx::writeData(wb, "Capacity calibration",
                      round(cap_raw$Intensity, 4), startRow = 3, startCol = 2)

  openxlsx::writeData(wb, "Capacity calibration", "Internal volume",
                      startRow = 2, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", "Addition",
                      startRow = 5, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", "SPQ",
                      startRow = 5, startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration", "Total",
                      startRow = 5, startCol = 14)
  openxlsx::writeData(wb, "Capacity calibration", "FU",
                      startRow = 5, startCol = 15)
  openxlsx::writeData(wb, "Capacity calibration", "(\u00b5L)",
                      startRow = 6, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", "(\u00b5M)",
                      startRow = 6, startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration", "(\u00b5M)",
                      startRow = 6, startCol = 14)

  n_cap   <- cap_cal$n_used
  fu_cap  <- cap_plat$median_intensity[1:n_cap]
  openxlsx::writeData(wb, "Capacity calibration", 0,         startRow = 8, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", 0,         startRow = 8, startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration", 0,         startRow = 8, startCol = 14)
  openxlsx::writeData(wb, "Capacity calibration", fu_cap[1], startRow = 8, startCol = 15)
  for (i in 2:n_cap) {
    rn <- 8 + i - 1
    openxlsx::writeData(wb, "Capacity calibration", 1,        startRow = rn, startCol = 12)
    openxlsx::writeData(wb, "Capacity calibration", 1,        startRow = rn, startCol = 13)
    openxlsx::writeData(wb, "Capacity calibration", i - 1,    startRow = rn, startCol = 14)
    openxlsx::writeData(wb, "Capacity calibration", fu_cap[i], startRow = rn, startCol = 15)
  }
  note_row <- 8 + n_cap + 2
  openxlsx::writeData(wb, "Capacity calibration", "Addition:",
                      startRow = note_row, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", "1\u00b5L SPQ (0.5mM)",
                      startRow = note_row, startCol = 13)

  param_row <- note_row + 3
  openxlsx::writeData(wb, "Capacity calibration", "a",       startRow = param_row,     startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration", "b",       startRow = param_row,     startCol = 14)
  openxlsx::writeData(wb, "Capacity calibration", "y=ax+b",  startRow = param_row,     startCol = 15)
  openxlsx::writeData(wb, "Capacity calibration", cap_cal$slope,
                      startRow = param_row + 1, startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration", cap_cal$intercept,
                      startRow = param_row + 1, startCol = 14)
  openxlsx::writeData(wb, "Capacity calibration", "x=(y-b)/a",
                      startRow = param_row + 1, startCol = 15)
  openxlsx::writeData(wb, "Capacity calibration", "internal vol",
                      startRow = param_row + 2, startCol = 15)

  results_header_row <- param_row + 3
  openxlsx::writeData(wb, "Capacity calibration", "FU/\u00b5M",
                      startRow = results_header_row, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", "Initial FU",
                      startRow = results_header_row, startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration", "\u00b5M",
                      startRow = results_header_row, startCol = 14)
  openxlsx::writeData(wb, "Capacity calibration", "\u00b5L/75 \u00b5L sample",
                      startRow = results_header_row, startCol = 15)
  results_row <- results_header_row + 2
  openxlsx::writeData(wb, "Capacity calibration",
                      cap_sum$Value[cap_sum$Parameter == "FU/\u00b5M"],
                      startRow = results_row, startCol = 12)
  openxlsx::writeData(wb, "Capacity calibration", fu_cap[1],
                      startRow = results_row, startCol = 13)
  openxlsx::writeData(wb, "Capacity calibration",
                      cap_sum$Value[cap_sum$Parameter == "\u00b5M"],
                      startRow = results_row, startCol = 14)
  openxlsx::writeData(wb, "Capacity calibration",
                      cap_sum$Value[cap_sum$Parameter == "\u00b5L/75 \u00b5L sample"],
                      startRow = results_row, startCol = 15)
  openxlsx::writeData(wb, "Capacity calibration", "(reaction vol 500 \u00b5L)",
                      startRow = results_row + 1, startCol = 15)

  # ---- Rates sheet  ---------------------------------------------------
  openxlsx::writeData(wb, "Rates", "Sample",          startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "Rates", "Date",            startRow = 2, startCol = 1)
  openxlsx::writeData(wb, "Rates", "Internal volume", startRow = 3, startCol = 1)
  openxlsx::writeFormula(wb, "Rates",
                         "='Capacity calibration'!O24",
                         startRow = 3, startCol = 2)
  openxlsx::writeData(wb, "Rates", "Assumed protein (\u00b5g)",
                      startRow = 4, startCol = 1)
  openxlsx::writeFormula(wb, "Rates", "=20/(1400/75)",
                         startRow = 4, startCol = 2)
  openxlsx::writeData(wb, "Rates", "Liposome additions", startRow = 6, startCol = 1)

  n_samples <- ncol(raw) - 1L
  for (i in seq_len(n_samples)) {
    col_num    <- i + 1L
    col_letter <- LETTERS[col_num]
    openxlsx::writeFormula(wb, "Rates",
                           paste0("='Raw data'!", col_letter, "1"),
                           startRow = 6, startCol = col_num)
  }
  openxlsx::writeData(wb, "Rates", "Plateau", startRow = 7,  startCol = 1)
  openxlsx::writeData(wb, "Rates", "Top",     startRow = 8,  startCol = 1)
  openxlsx::writeData(wb, "Rates", "K",       startRow = 9,  startCol = 1)
  openxlsx::writeData(wb, "Rates", "Span",    startRow = 10, startCol = 1)
  for (i in seq_len(n_samples)) {
    cn <- i + 1L; cl <- LETTERS[cn]
    openxlsx::writeFormula(wb, "Rates",
                           paste0("=", cl, "8-", cl, "7"),
                           startRow = 10, startCol = cn)
  }
  openxlsx::writeData(wb, "Rates", "mM [H+]/s", startRow = 12, startCol = 1)
  for (i in seq_len(n_samples)) {
    cn <- i + 1L; cl <- LETTERS[cn]
    openxlsx::writeFormula(wb, "Rates",
                           paste0("=", cl, "9*", cl, "10"),
                           startRow = 12, startCol = cn)
  }
  openxlsx::writeData(wb, "Rates", "\u00b5mol H+/s", startRow = 13, startCol = 1)
  for (i in seq_len(n_samples)) {
    cn <- i + 1L; cl <- LETTERS[cn]
    openxlsx::writeFormula(wb, "Rates",
                           paste0("=", cl, "12*1000*($B$3/1000000)"),
                           startRow = 13, startCol = cn)
  }
  openxlsx::writeData(wb, "Rates", "\u00b5mol H+/min", startRow = 14, startCol = 1)
  for (i in seq_len(n_samples)) {
    cn <- i + 1L; cl <- LETTERS[cn]
    openxlsx::writeFormula(wb, "Rates", paste0("=", cl, "13*60"),
                           startRow = 14, startCol = cn)
  }
  openxlsx::writeData(wb, "Rates", "\u00b5mol H+/min/mg", startRow = 15, startCol = 1)
  for (i in seq_len(n_samples)) {
    cn <- i + 1L; cl <- LETTERS[cn]
    openxlsx::writeFormula(wb, "Rates",
                           paste0("=", cl, "14/($B$4/1000)"),
                           startRow = 15, startCol = cn)
  }

  # ---- Embedded chart images ------------------------------------------
  td <- tempdir()
  c1 <- file.path(td, "proton_trace_chart.png")
  c2 <- file.path(td, "proton_calib_chart.png")
  c3 <- file.path(td, "capacity_trace_chart.png")
  c4 <- file.path(td, "capacity_calib_chart.png")

  grDevices::png(c1, width = 800, height = 400, res = 100)
  graphics::plot(proton_raw$Time, proton_raw$Intensity, type = "l",
                 col = "#3B82F6", lwd = 2,
                 main = "Proton Calibration Trace",
                 xlab = "Time (s)", ylab = "Intensity (a.u.)",
                 las = 1, cex.main = 1.2, cex.lab = 1.1)
  graphics::grid(col = "gray80", lty = 2); grDevices::dev.off()

  grDevices::png(c2, width = 600, height = 500, res = 100)
  graphics::par(mar = c(5, 5, 4, 2))
  graphics::plot(proton_cal$data$H_conc, proton_cal$data$inv_FU,
                 pch = 19, col = "#3B82F6", cex = 1.5,
                 main = "FU/Proton Calibration Curve",
                 xlab = "[H+] (mM)", ylab = "1/FU",
                 las = 1, cex.main = 1.2, cex.lab = 1.1)
  graphics::grid(col = "gray80", lty = 2)
  graphics::abline(a = proton_cal$intercept, b = proton_cal$slope,
                   col = "#EF4444", lwd = 2, lty = 2)
  graphics::legend("topleft",
    legend = c(sprintf("y = %.6fx + %.5f", proton_cal$slope, proton_cal$intercept),
               sprintf("R\u00b2 = %.4f", proton_cal$r_squared)),
    bty = "n", cex = 1.1, text.col = "#1F2937")
  grDevices::dev.off()

  grDevices::png(c3, width = 800, height = 400, res = 100)
  graphics::plot(cap_raw$Time, cap_raw$Intensity, type = "l",
                 col = "#10B981", lwd = 2,
                 main = "Capacity Calibration Trace",
                 xlab = "Time (s)", ylab = "Intensity (a.u.)",
                 las = 1, cex.main = 1.2, cex.lab = 1.1)
  graphics::grid(col = "gray80", lty = 2); grDevices::dev.off()

  grDevices::png(c4, width = 600, height = 500, res = 100)
  graphics::par(mar = c(5, 5, 4, 2))
  graphics::plot(cap_cal$data$Total_SPQ, fu_cap,
                 pch = 19, col = "#10B981", cex = 1.5,
                 main = "Capacity Calibration Curve",
                 xlab = "Total SPQ (\u00b5M)", ylab = "FU",
                 las = 1, cex.main = 1.2, cex.lab = 1.1)
  graphics::grid(col = "gray80", lty = 2)
  graphics::abline(a = cap_cal$intercept, b = cap_cal$slope,
                   col = "#EF4444", lwd = 2, lty = 2)
  graphics::legend("topleft",
    legend = c(sprintf("y = %.6fx + %.5f", cap_cal$slope, cap_cal$intercept),
               sprintf("R\u00b2 = %.4f", cap_cal$r_squared)),
    bty = "n", cex = 1.1, text.col = "#1F2937")
  grDevices::dev.off()

  openxlsx::insertImage(wb, "Proton calibration",   c1,
                        startRow = 1, startCol = 3,
                        width = 6, height = 3, units = "in")
  openxlsx::insertImage(wb, "Proton calibration",   c2,
                        startRow = 7, startCol = 19,
                        width = 5, height = 4.5, units = "in")
  openxlsx::insertImage(wb, "Capacity calibration", c3,
                        startRow = 1, startCol = 3,
                        width = 6, height = 3, units = "in")
  openxlsx::insertImage(wb, "Capacity calibration", c4,
                        startRow = 7, startCol = 17,
                        width = 5, height = 4.5, units = "in")

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  unlink(c1); unlink(c2); unlink(c3); unlink(c4)
}

################################################################################
#  mod_export.R  --  Cross-tool "Export Report" zip bundle
################################################################################
#
#  Each tool module returns a reactive() with its current results. This
#  module is the single place where those reactives are pulled together
#  into one zip download.
#
#  Why a module at all? Because the export button lives in the navbar, not
#  inside any tool tab. Putting it in its own module keeps app.R uncluttered
#  and means future tools just need to be added to the `tools` list passed in.
#
#  USAGE (from app.R):
#    bca_res    <- bca_server("bca")
#    cpm_res    <- cpm_server("cpm")
#    ...
#    export_server("export", tools = list(bca = bca_res, cpm = cpm_res, ...))
#
################################################################################

export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("zip"), "\U0001f4e6 Export Report",
                        class = "btn-download")
}

#' Export module server
#'
#' @param id module id
#' @param tools named list of reactives, each returning the latest
#'   results from one tool. Names are used as filename prefixes.
export_server <- function(id, tools) {
  shiny::moduleServer(id, function(input, output, session) {

    output$zip <- shiny::downloadHandler(
      filename = function() ts_filename("CrichtonAnalysisReport", "zip"),
      content  = function(file) {
        tmp <- tempfile("report_"); dir.create(tmp)
        files_to_zip <- character()

        # ---- BCA ---------------------------------------------------------
        if (!is.null(tools$bca)) {
          r <- tools$bca()
          if (!is.null(r)) {
            bca_csv <- file.path(tmp, "BCA_results.csv")
            utils::write.csv(data.frame(
              Concentration_mgmL = r$conc, Volume_mL = r$vol,
              Total_yield_mg = r$yield, R_squared = r$curve$r2,
              Date = Sys.time()), bca_csv, row.names = FALSE)
            files_to_zip <- c(files_to_zip, bca_csv)

            bca_png <- file.path(tmp, "BCA_standard_curve.png")
            ggplot2::ggsave(bca_png,
              r$curve$plot + theme_cg_publication(),
              width = 10, height = 6, dpi = 300, bg = "white")
            files_to_zip <- c(files_to_zip, bca_png)

            tryCatch({
              title <- if (!is.null(r$title) && nchar(trimws(r$title)) > 0)
                trimws(r$title) else "BCA Assay Protein Yield Summary"
              p_tbl <- bca_results_table_plot(
                title  = title,
                params = c("Concentration (mg/mL)", "Sample volume (mL)",
                           "Total yield (mg)",      "R\u00b2 (standard curve)"),
                vals   = c(sprintf("%.2f", r$conc),  sprintf("%.2f", r$vol),
                           sprintf("%.2f", r$yield), sprintf("%.4f", r$curve$r2))
              )
              bca_tbl <- file.path(tmp, "BCA_results_table.png")
              ggplot2::ggsave(bca_tbl, p_tbl,
                              width = 5.5, height = 3.0, dpi = 300, bg = "white")
              files_to_zip <- c(files_to_zip, bca_tbl)
            }, error = function(e) NULL)
          }
        }

        # ---- CPM ---------------------------------------------------------
        if (!is.null(tools$cpm)) {
          r <- tools$cpm()
          if (!is.null(r)) {
            cpm_png <- file.path(tmp, "CPM_Tm.png")
            ggplot2::ggsave(cpm_png,
              r$res$plot + theme_cg_publication(),
              width = 12, height = 6, dpi = 300, bg = "white")
            files_to_zip <- c(files_to_zip, cpm_png)

            cpm_csv <- file.path(tmp, "CPM_results.csv")
            df <- if (r$mode == "manual") {
              data.frame(Sample = r$sample_name, Mode = "Manual",
                Tm_degC = r$res$tm, T_lower = r$res$T_lower,
                T_upper = r$res$T_upper, Area = r$res$area,
                FWHM = r$res$fwhm, N_points = r$res$n_points,
                Date = Sys.time())
            } else {
              cbind(Sample = r$sample_name, Mode = "Automatic",
                    r$res$summary, Date = Sys.time())
            }
            utils::write.csv(df, cpm_csv, row.names = FALSE)
            files_to_zip <- c(files_to_zip, cpm_csv)
          }
        }

        # ---- AKTA --------------------------------------------------------
        if (!is.null(tools$akta)) {
          r <- tools$akta()
          if (!is.null(r) && !is.null(r$plot)) {
            akta_png <- file.path(tmp, "AKTA_chromatogram.png")
            p <- r$plot
            if (!is.null(r$annotation)) {
              ann <- r$annotation
              lbl <- sprintf("Ve = %.2f mL\n%.1f kDa",
                             ann$centroid, ann$mw_kda)
              p <- p +
                ggplot2::geom_vline(xintercept = ann$centroid,
                  colour = "#E07030", linewidth = 0.7, linetype = "dashed") +
                ggplot2::annotate("label", x = ann$centroid, y = Inf,
                  label = lbl, vjust = 1.3, size = 3.5,
                  colour = "#E07030", fill = "white",
                  label.size = 0.3,
                  label.padding = ggplot2::unit(0.25, "lines"))
            }
            ggplot2::ggsave(akta_png, p + theme_cg_publication(),
              width = 12, height = 6, dpi = 300, bg = "white")
            files_to_zip <- c(files_to_zip, akta_png)
          }
        }

        # ---- CPM QC ------------------------------------------------------
        # qc_res may contain simple-mode results, multi-mode results, or both.
        if (!is.null(tools$cpm_qc)) {
          r <- tools$cpm_qc()
          if (!is.null(r)) {
            for (mode_name in c("simple", "multi")) {
              m <- r[[mode_name]]
              if (is.null(m)) next
              prefix <- if (mode_name == "simple") "CPM_QC" else "CPM_multiQC"
              for (which_p in c("p_raw", "p_dfdt", "p_tm")) {
                if (!is.null(m[[which_p]])) {
                  tag <- switch(which_p,
                                p_raw  = "fluorescence",
                                p_dfdt = "dFdT",
                                p_tm   = "Tm")
                  fp <- file.path(tmp, sprintf("%s_%s.png", prefix, tag))
                  ggplot2::ggsave(fp, m[[which_p]],
                    width = 6.5, height = 4.5, dpi = 300, bg = "white")
                  files_to_zip <- c(files_to_zip, fp)
                }
              }
            }
          }
        }

        # ---- CPM Contour -------------------------------------------------
        if (!is.null(tools$cpm_contour)) {
          r <- tools$cpm_contour()
          if (!is.null(r) && !is.null(r$dfdt_processed)) {
            p     <- r$dfdt_processed
            stats_csv <- file.path(tmp, "CPM_contour_statistics.csv")
            export_df <- data.frame(Temperature = p$temperatures)
            for (i in seq_along(p$sample_names)) {
              s <- p$sample_names[i]
              export_df[[paste0(s, "_Mean")]] <- p$mean[, i]
              export_df[[paste0(s, "_SEM")]]  <- p$sem[, i]
            }
            utils::write.csv(export_df, stats_csv, row.names = FALSE)
            files_to_zip <- c(files_to_zip, stats_csv)
          }
        }

        # ---- UCP1 --------------------------------------------------------
        # UCP1 has its own dedicated xlsx export inside the tool. For the
        # cross-tool bundle we just include a CSV of processed data.
        if (!is.null(tools$ucp1)) {
          r <- tools$ucp1()
          if (!is.null(r) && !is.null(r$processed)) {
            ucp1_csv <- file.path(tmp, "UCP1_processed_data.csv")
            proc <- r$processed; proc$Time <- round(proc$Time, 1)
            utils::write.csv(proc, ucp1_csv, row.names = FALSE)
            files_to_zip <- c(files_to_zip, ucp1_csv)
          }
        }

        # ---- Gel ---------------------------------------------------------
        # The gel module's reactive exposes the magick image and markers.
        # Re-rendering for the report requires the same canvas math used
        # inside the module; rather than duplicate that here, we write the
        # current magick image directly as the "raw cropped gel" PNG and
        # leave full annotated exports to the in-tool download buttons.
        if (!is.null(tools$gel)) {
          r <- tools$gel()
          if (!is.null(r) && !is.null(r$image)) {
            gel_png <- file.path(tmp, "Gel_image.png")
            tryCatch(magick::image_write(r$image, gel_png, format = "png"),
                     error = function(e) NULL)
            if (file.exists(gel_png))
              files_to_zip <- c(files_to_zip, gel_png)
          }
          if (!is.null(r) && !is.null(r$western_image)) {
            western_png <- file.path(tmp, "Western_image.png")
            tryCatch(magick::image_write(r$western_image, western_png,
                                         format = "png"),
                     error = function(e) NULL)
            if (file.exists(western_png))
              files_to_zip <- c(files_to_zip, western_png)
          }
        }

        if (length(files_to_zip) == 0) {
          shiny::showNotification(
            "Nothing to export - run at least one analysis first.",
            type = "warning")
          return()
        }

        zip::zipr(zipfile = file, files = files_to_zip)
      }
    )
  })
}

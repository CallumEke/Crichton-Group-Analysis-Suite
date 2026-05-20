################################################################################
#                  AKTA DATA PLOTTER - IMPROVED VERSION
################################################################################
#
# This function provides enhanced plotting capabilities for AKTA chromatography
# data with modern aesthetics and user-friendly features.
#
# Author: Improved version for ease of use
# Compatible with: UNICORN 7 CSV exports
#
################################################################################

# Load required libraries
library(ggplot2)
library(gridExtra)
library(scales)

#' Plot AKTA Chromatography Data
#' 
#' Creates publication-quality plots of AKTA chromatography data with options
#' for overlaying multiple traces, showing fractions, and customizing appearance.
#'
#' @param files Character vector of CSV file paths from UNICORN 7
#' @param volume_range Numeric vector of length 2: c(min, max) volume in mL (NULL for auto)
#' @param show_fractions Logical: display fraction marker lines (dashed verticals)
#' @param show_fraction_labels Logical: display fraction text labels above the
#'   markers. Independent of `show_fractions` so the user can keep the dashed
#'   lines as positional references while hiding labels that overlap on runs
#'   with very small elution fractions.
#' @param show_conductance Logical: show conductance trace (from first file)
#' @param show_uv260 Logical: show UV 260nm trace (from first file)
#' @param show_percent_b Logical: show concentration B trace (from first file)
#' @param highlight_fractions Character vector of fraction labels to highlight (e.g., c("B8", "B9"))
#' @param save_plot Logical: save plot to PDF and PNG
#' @param output_dir Character: directory for saved plots (NULL = same as first file)
#' @param output_name Character: base name for output files (NULL = auto-generate)
#' @param plot_width Numeric: plot width in inches
#' @param plot_height Numeric: plot height in inches
#' @param theme Character: "publication", "presentation", or "minimal"
#' @param colors Character vector: custom colors for UV traces (NULL for defaults)
#' @param line_width Numeric: line width for UV traces
#' @param dpi Numeric: resolution for PNG output
#'
#' @return ggplot object (invisibly)
#' @export
plot_akta_improved <- function(
    files,
    volume_range = NULL,
    show_fractions = TRUE,
    show_fraction_labels = TRUE,
    show_conductance = FALSE,
    show_uv260 = FALSE,
    show_percent_b = FALSE,
    highlight_fractions = NULL,
    save_plot = TRUE,
    output_dir = NULL,
    output_name = NULL,
    plot_width = 12,
    plot_height = 6,
    theme = "publication",
    colors = NULL,
    line_width = 1.2,
    dpi = 300
) {
  
  ## ========================================================================
  ##                        INPUT VALIDATION
  ## ========================================================================
  
  if (missing(files) || length(files) == 0) {
    stop("ERROR: No files provided. Please specify at least one CSV file.")
  }
  
  files <- as.character(files)
  
  # Check that files exist
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop("ERROR: The following files do not exist:\n",
         paste("  -", missing_files, collapse = "\n"))
  }
  
  cat("\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("                AKTA DATA PLOTTER - Processing\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("\n")
  cat(sprintf("📁 Processing %d file(s)...\n", length(files)))
  cat("\n")
  
  ## ========================================================================
  ##                        HELPER FUNCTIONS
  ## ========================================================================

  # Detect which UNICORN CSV format a file uses.
  # Returns "matrix"   — UTF-16LE tab-separated transposed matrix (classic UNICORN 7 export)
  #         "columnar" — UTF-8 comma-separated columnar export (newer UNICORN 7 export)
  detect_unicorn_format <- function(filepath) {
    # Try reading the first line as UTF-8 plain text
    first_lines <- tryCatch(
      readLines(filepath, n = 3, encoding = "UTF-8", warn = FALSE),
      error = function(e) character(0)
    )
    # Columnar format has trace-type names (UV, Fraction, Cond etc.) in row 2
    if (length(first_lines) >= 2) {
      row2 <- first_lines[2]
      if (grepl("UV|Fraction|Cond", row2, ignore.case = FALSE)) {
        return("columnar")
      }
    }
    return("matrix")
  }

  # Parse columnar CSV (UTF-8, comma-separated, 3-row header)
  # Row 1: run names  |  Row 2: trace type  |  Row 3: units  |  Rows 4+: data pairs
  # Returns a named list: $uv, $fractions, $conductance, $percent_b  (each data.frame or NULL)
  read_unicorn_columnar <- function(filepath) {
    raw <- utils::read.csv(filepath, header = FALSE,
                           stringsAsFactors = FALSE, check.names = FALSE,
                           fileEncoding = "UTF-8-BOM", na.strings = "")
    if (nrow(raw) < 4) stop("Columnar file has fewer than 4 rows.")

    trace_types <- trimws(as.character(raw[2, ]))   # Row 2: UV / Fraction / % Cond / Cond
    units       <- trimws(as.character(raw[3, ]))   # Row 3: ml / mAU / ...
    data_rows   <- raw[4:nrow(raw), ]               # Rows 4+: actual data

    # Helper: find column index where row 2 exactly matches a label
    find_col <- function(label) which(trace_types == label)

    # Extract a (volume, value) pair from two column indices
    extract_col_pair <- function(vol_col, val_col) {
      vols <- suppressWarnings(as.numeric(as.character(data_rows[[vol_col]])))
      vals <- suppressWarnings(as.numeric(as.character(data_rows[[val_col]])))
      ok   <- !is.na(vols) & !is.na(vals)
      if (sum(ok) == 0) return(NULL)
      data.frame(volume = vols[ok], value = vals[ok])
    }

    result <- list(uv = NULL, fractions = NULL, conductance = NULL, percent_b = NULL)

    # UV trace (label "UV", units col "mAU")
    uv_cols <- find_col("UV")
    if (length(uv_cols) >= 1) {
      vc <- uv_cols[1]
      result$uv <- extract_col_pair(vc, vc + 1)
    }

    # Fractions (label "Fraction")
    frac_cols <- find_col("Fraction")
    if (length(frac_cols) >= 1) {
      vc <- frac_cols[1]
      vols  <- suppressWarnings(as.numeric(as.character(data_rows[[vc]])))
      lbls  <- trimws(gsub('"', '', as.character(data_rows[[vc + 1]])))
      ok    <- !is.na(vols) & nchar(lbls) > 0
      if (sum(ok) > 0)
        result$fractions <- data.frame(volume = vols[ok], label = lbls[ok],
                                        stringsAsFactors = FALSE)
    }

    # Conductance (label "Cond", not "% Cond")
    cond_cols <- find_col("Cond")
    if (length(cond_cols) >= 1) {
      vc <- cond_cols[1]
      result$conductance <- extract_col_pair(vc, vc + 1)
    }

    # % Buffer B / % Cond (label "% Cond")
    pctb_cols <- find_col("% Cond")
    if (length(pctb_cols) >= 1) {
      vc <- pctb_cols[1]
      result$percent_b <- extract_col_pair(vc, vc + 1)
    }

    return(result)
  }

  # Format-aware file reader.
  # For matrix format: returns transposed aktalst matrix (backward compatible).
  # For columnar format: returns a parsed list tagged with $format = "columnar".
  read_unicorn_csv <- function(filepath) {
    tryCatch({
      cat(sprintf("   Reading: %s\n", basename(filepath)))
      fmt <- detect_unicorn_format(filepath)

      if (fmt == "columnar") {
        parsed <- read_unicorn_columnar(filepath)
        parsed$format <- "columnar"
        return(parsed)
      }

      # Original matrix format (UTF-16LE tab-separated)
      raw    <- utils::read.delim(filepath, header = FALSE, sep = "\t",
                  fileEncoding = "UTF-16LE", stringsAsFactors = FALSE,
                  check.names = FALSE)
      aktalst        <- t(as.matrix(raw))
      attr(aktalst, "format") <- "matrix"
      return(aktalst)

    }, error = function(e) {
      stop(sprintf("ERROR reading file '%s': %s", basename(filepath), e$message))
    })
  }

  # Extract a trace, dispatching on format.
  extract_trace <- function(parsed, label_name, row_offset = 1) {
    fmt <- if (is.list(parsed) && !is.null(parsed$format)) parsed$format else "matrix"

    if (fmt == "columnar") {
      # Map old label names to columnar list slots
      slot <- switch(label_name,
        "UV 1_280" = "uv", "UV" = "uv",
        "Cond"     = "conductance",
        "UV 2_260" = NULL,   # not typically in columnar export
        "Conc B"   = "percent_b",
        NULL
      )
      if (is.null(slot) || is.null(parsed[[slot]])) return(NULL)
      d <- parsed[[slot]]
      return(data.frame(volume = d$volume, value = d$value))
    }

    # Original matrix logic
    aktalst <- parsed
    n_rows  <- nrow(aktalst); n_cols <- ncol(aktalst)
    for (r in seq_len(n_rows)) {
      label <- trimws(as.character(aktalst[r, 2]))
      if (is.na(label) || label == "") next
      if (label == label_name) {
        if (r + row_offset > n_rows) return(NULL)
        x_vals <- suppressWarnings(as.numeric(aktalst[r,              4:n_cols]))
        y_vals <- suppressWarnings(as.numeric(aktalst[r + row_offset, 4:n_cols]))
        x_vals <- x_vals[!is.na(x_vals)]; y_vals <- y_vals[!is.na(y_vals)]
        m <- min(length(x_vals), length(y_vals))
        if (m > 0) return(data.frame(volume = x_vals[1:m], value = y_vals[1:m]))
      }
    }
    return(NULL)
  }

  # Extract fractions, dispatching on format.
  extract_fractions <- function(parsed) {
    fmt <- if (is.list(parsed) && !is.null(parsed$format)) parsed$format else "matrix"

    if (fmt == "columnar") {
      if (is.null(parsed$fractions)) return(NULL)
      return(parsed$fractions)
    }

    # Original matrix logic
    aktalst <- parsed
    n_rows  <- nrow(aktalst); n_cols <- ncol(aktalst)
    for (r in seq_len(n_rows)) {
      label <- trimws(as.character(aktalst[r, 2]))
      if (is.na(label) || label == "") next
      if (label == "Fraction") {
        if (r + 1 > n_rows) return(NULL)
        x_vals     <- suppressWarnings(as.numeric(aktalst[r,     4:n_cols]))
        frac_labels <- aktalst[r + 1, 4:n_cols]
        x_vals      <- x_vals[!is.na(x_vals)]
        frac_labels <- frac_labels[!is.na(frac_labels) & frac_labels != ""]
        m <- min(length(x_vals), length(frac_labels))
        if (m > 0) return(data.frame(volume = x_vals[1:m], label = frac_labels[1:m],
                                     stringsAsFactors = FALSE))
      }
    }
    return(NULL)
  }

  ## ========================================================================
  ##                        READ DATA FILES
  ## ========================================================================
  
  all_uv_data <- list()
  primary_data <- list()
  file_labels <- c()
  
  for (i in seq_along(files)) {
    filepath <- files[i]
    aktalst <- read_unicorn_csv(filepath)
    
    # Extract UV trace (try both label formats)
    uv_data <- extract_trace(aktalst, "UV 1_280")
    if (is.null(uv_data)) {
      uv_data <- extract_trace(aktalst, "UV")
    }
    
    if (is.null(uv_data)) {
      warning(sprintf("No UV trace found in '%s'. Skipping this file.", basename(filepath)))
      next
    }
    
    uv_data$file <- basename(tools::file_path_sans_ext(filepath))
    all_uv_data[[i]] <- uv_data
    file_labels[i] <- basename(tools::file_path_sans_ext(filepath))
    
    # For the first file, extract additional data if requested
    if (i == 1) {
      if (show_fractions) {
        fractions <- extract_fractions(aktalst)
        primary_data$fractions <- fractions
      }
      
      if (show_conductance) {
        cond_data <- extract_trace(aktalst, "Cond")
        primary_data$conductance <- cond_data
      }
      
      if (show_uv260) {
        uv260_data <- extract_trace(aktalst, "UV 2_260")
        primary_data$uv260 <- uv260_data
      }
      
      if (show_percent_b) {
        concb_data <- extract_trace(aktalst, "Conc B")
        primary_data$percent_b <- concb_data
      }
    }
  }
  
  # Remove NULL entries
  all_uv_data <- all_uv_data[!sapply(all_uv_data, is.null)]
  
  if (length(all_uv_data) == 0) {
    stop("ERROR: No valid UV data found in any of the provided files.")
  }
  
  cat("\n")
  cat(sprintf("✓ Successfully loaded %d file(s)\n", length(all_uv_data)))
  cat("\n")
  
  ## ========================================================================
  ##                        PREPARE PLOT DATA
  ## ========================================================================
  
  # Combine all UV data
  uv_combined <- do.call(rbind, all_uv_data)
  
  # Apply volume range filter
  if (!is.null(volume_range)) {
    if (length(volume_range) != 2 || !is.numeric(volume_range)) {
      warning("volume_range must be a numeric vector of length 2. Ignoring.")
    } else {
      uv_combined <- uv_combined[uv_combined$volume >= volume_range[1] & 
                                 uv_combined$volume <= volume_range[2], ]
      
      # Also filter primary data
      if (!is.null(primary_data$fractions)) {
        primary_data$fractions <- primary_data$fractions[
          primary_data$fractions$volume >= volume_range[1] & 
          primary_data$fractions$volume <= volume_range[2], ]
      }
      if (!is.null(primary_data$conductance)) {
        primary_data$conductance <- primary_data$conductance[
          primary_data$conductance$volume >= volume_range[1] & 
          primary_data$conductance$volume <= volume_range[2], ]
      }
      if (!is.null(primary_data$uv260)) {
        primary_data$uv260 <- primary_data$uv260[
          primary_data$uv260$volume >= volume_range[1] & 
          primary_data$uv260$volume <= volume_range[2], ]
      }
      if (!is.null(primary_data$percent_b)) {
        primary_data$percent_b <- primary_data$percent_b[
          primary_data$percent_b$volume >= volume_range[1] & 
          primary_data$percent_b$volume <= volume_range[2], ]
      }
    }
  }
  
  ## ========================================================================
  ##                        THEME SELECTION
  ## ========================================================================
  
  base_theme <- theme_minimal(base_size = 14)
  
  if (theme == "publication") {
    plot_theme <- base_theme +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray90", size = 0.3),
        panel.grid.minor = element_line(color = "gray95", size = 0.2),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.background = element_rect(fill = "white", color = "gray70"),
        legend.key.size = unit(1.5, "lines")
      )
  } else if (theme == "presentation") {
    plot_theme <- base_theme +
      theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, color = "black", face = "bold"),
        axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", size = 0.8),
        panel.grid.major = element_line(color = "gray85", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "white", color = "gray50", size = 1),
        legend.key.size = unit(2, "lines")
      )
  } else {  # minimal
    plot_theme <- base_theme +
      theme(
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank()
      )
  }
  
  ## ========================================================================
  ##                        COLOR SCHEME
  ## ========================================================================
  
  if (is.null(colors)) {
    # Professional color palette optimized for colorblind accessibility
    colors <- c(
      "#0072B2",  # Blue
      "#D55E00",  # Red-Orange
      "#009E73",  # Green
      "#CC79A7",  # Pink
      "#F0E442",  # Yellow
      "#56B4E9",  # Light Blue
      "#E69F00",  # Orange
      "#999999"   # Gray
    )
  }
  
  # Ensure we have enough colors
  if (length(all_uv_data) > length(colors)) {
    colors <- rep(colors, ceiling(length(all_uv_data) / length(colors)))
  }
  
  ## ========================================================================
  ##                        CREATE MAIN PLOT
  ## ========================================================================
  
  cat("🎨 Creating plot...\n")
  
  # Calculate y-axis limits with some headroom
  uv_max <- max(uv_combined$value, na.rm = TRUE)
  uv_min <- min(uv_combined$value, na.rm = TRUE)
  y_expand <- (uv_max - uv_min) * 0.15
  
  p <- ggplot(uv_combined, aes(x = volume, y = value, color = file, group = file)) +
    geom_line(size = line_width) +
    scale_color_manual(
      values = colors[1:length(all_uv_data)],
      name = if (length(all_uv_data) > 1) "Sample" else NULL
    ) +
    scale_x_continuous(
      expand = c(0.01, 0),
      breaks = pretty_breaks(n = 10)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.15)),
      breaks = pretty_breaks(n = 8)
    ) +
    labs(
      title = "ÄKTA Chromatography Profile",
      subtitle = if (length(all_uv_data) > 1) {
        sprintf("Overlay of %d samples", length(all_uv_data))
      } else {
        file_labels[1]
      },
      x = "Volume (mL)",
      y = "Absorbance 280 nm (mAU)"
    ) +
    plot_theme
  
  # Remove legend if single file
  if (length(all_uv_data) == 1) {
    p <- p + theme(legend.position = "none")
  }
  
  ## ========================================================================
  ##                        ADD FRACTIONS
  ## ========================================================================
  
  if (show_fractions && !is.null(primary_data$fractions)) {
    fractions <- primary_data$fractions
    
    if (nrow(fractions) > 0) {
      cat("   Adding fraction markers...\n")
      
      # Calculate label positions (midpoint between fraction boundaries)
      if (nrow(fractions) > 1) {
        label_positions <- data.frame(
          volume = (fractions$volume[-nrow(fractions)] + fractions$volume[-1]) / 2,
          label = fractions$label[-nrow(fractions)]
        )
      } else {
        label_positions <- data.frame(
          volume = fractions$volume,
          label = fractions$label
        )
      }
      
      # Determine which fractions to highlight
      if (!is.null(highlight_fractions)) {
        # Find indices of fractions to highlight
        highlight_indices <- which(fractions$label %in% highlight_fractions)
        
        if (length(highlight_indices) > 0) {
          cat(sprintf("   Highlighting %d fraction(s): %s\n", 
                     length(highlight_indices),
                     paste(highlight_fractions, collapse = ", ")))
          
          # Add shaded region for each highlighted fraction
          for (idx in highlight_indices) {
            # Get start volume (this fraction boundary)
            xmin <- fractions$volume[idx]
            
            # Get end volume (next fraction boundary, or extend slightly if last)
            if (idx < nrow(fractions)) {
              xmax <- fractions$volume[idx + 1]
            } else {
              # For last fraction, extend by the average fraction width
              if (nrow(fractions) > 1) {
                avg_width <- mean(diff(fractions$volume))
                xmax <- xmin + avg_width
              } else {
                xmax <- xmin + 1  # Default 1 mL width
              }
            }
            
            # Add semi-transparent yellow rectangle
            p <- p + annotate("rect",
                             xmin = xmin,
                             xmax = xmax,
                             ymin = -Inf, 
                             ymax = Inf,
                             fill = "yellow", 
                             alpha = 0.25)
          }
        } else {
          warning(sprintf(
            "No fractions matched highlight_fractions: %s\n  Available fractions: %s",
            paste(highlight_fractions, collapse = ", "),
            paste(head(fractions$label, 10), collapse = ", ")
          ))
        }
      }
      
      # Add vertical lines at fraction boundaries
      p <- p + geom_vline(data = fractions,
                          aes(xintercept = volume),
                          linetype = "dashed",
                          color = "gray40",
                          size = 0.4,
                          alpha = 0.7)
      
      # Add fraction labels. Gated separately from the dashed lines so
      # the user can keep the markers visible (useful as positional
      # references) while turning off the text - which can become an
      # overlapping blur on runs with very small elution fractions.
      if (show_fraction_labels) {
        label_y <- uv_min + (uv_max - uv_min) * 0.05
        p <- p + geom_text(data = label_positions,
                           aes(x = volume, y = label_y, label = label),
                           angle = 90,
                           size = 3.5,
                           hjust = 0,
                           vjust = 0.5,
                           color = "gray20",
                           inherit.aes = FALSE,
                           fontface = "bold")
      }
    }
  }
  
  ## ========================================================================
  ##                        ADD SECONDARY Y-AXES
  ## ========================================================================
  
  # Note: ggplot2 doesn't support multiple y-axes easily
  # For conductance and other traces, we'll scale them to the UV axis range
  
  if (show_conductance && !is.null(primary_data$conductance)) {
    cat("   Adding conductance trace...\n")
    cond <- primary_data$conductance
    
    # Scale conductance to UV range
    cond_range <- range(cond$value, na.rm = TRUE)
    uv_range <- range(uv_combined$value, na.rm = TRUE)
    cond$value_scaled <- scales::rescale(cond$value, 
                                         to = uv_range,
                                         from = cond_range)
    
    p <- p + geom_line(data = cond,
                       aes(x = volume, y = value_scaled),
                       color = "brown",
                       size = line_width * 0.9,
                       linetype = "solid",
                       inherit.aes = FALSE)
  }
  
  if (show_percent_b && !is.null(primary_data$percent_b)) {
    cat("   Adding %B trace...\n")
    pctb <- primary_data$percent_b
    
    # Scale %B to UV range (0-100% -> UV range)
    uv_range <- range(uv_combined$value, na.rm = TRUE)
    pctb$value_scaled <- scales::rescale(pctb$value,
                                         to = uv_range,
                                         from = c(0, 100))
    
    p <- p + geom_line(data = pctb,
                       aes(x = volume, y = value_scaled),
                       color = "forestgreen",
                       size = line_width * 0.8,
                       linetype = "dashed",
                       inherit.aes = FALSE)
  }
  
  ## ========================================================================
  ##                        DISPLAY PLOT
  ## ========================================================================
  
  print(p)
  
  ## ========================================================================
  ##                        SAVE PLOT
  ## ========================================================================
  
  if (save_plot) {
    cat("\n")
    cat("💾 Saving plots...\n")
    
    # Determine output directory
    if (is.null(output_dir)) {
      output_dir <- dirname(files[1])
    }
    
    # Determine output name
    if (is.null(output_name)) {
      if (length(files) == 1) {
        output_name <- tools::file_path_sans_ext(basename(files[1]))
      } else {
        output_name <- "AKTA_overlay"
      }
    }
    
    # Create output paths
    pdf_path <- file.path(output_dir, paste0(output_name, "_plot.pdf"))
    png_path <- file.path(output_dir, paste0(output_name, "_plot.png"))
    
    # Save PDF
    ggsave(
      filename = pdf_path,
      plot = p,
      width = plot_width,
      height = plot_height,
      units = "in",
      device = "pdf"
    )
    cat(sprintf("   ✓ PDF saved: %s\n", basename(pdf_path)))
    
    # Save high-resolution PNG
    ggsave(
      filename = png_path,
      plot = p,
      width = plot_width,
      height = plot_height,
      units = "in",
      dpi = dpi,
      device = "png"
    )
    cat(sprintf("   ✓ PNG saved: %s\n", basename(png_path)))
    
    cat("\n")
    cat(sprintf("📂 Output location: %s\n", output_dir))
  }
  
  cat("\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("                    ✓ COMPLETE!\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("\n")
  
  invisible(p)
}

## ========================================================================
##                        UTILITY FUNCTIONS
## ========================================================================

#' Quick plot with sensible defaults
#' 
#' Simplified wrapper for common use cases
#' 
#' @param ... Arguments passed to plot_akta_improved
#' @export
quick_plot_akta <- function(...) {
  plot_akta_improved(
    show_fractions = TRUE,
    show_conductance = FALSE,
    theme = "publication",
    ...
  )
}

#' Publication-ready plot
#' 
#' @param ... Arguments passed to plot_akta_improved
#' @export
publication_plot <- function(...) {
  plot_akta_improved(
    theme = "publication",
    plot_width = 10,
    plot_height = 5,
    dpi = 600,
    line_width = 1.0,
    ...
  )
}

#' Presentation plot with larger text
#' 
#' @param ... Arguments passed to plot_akta_improved
#' @export
presentation_plot <- function(...) {
  plot_akta_improved(
    theme = "presentation",
    plot_width = 14,
    plot_height = 7,
    dpi = 150,
    line_width = 2.0,
    ...
  )
}

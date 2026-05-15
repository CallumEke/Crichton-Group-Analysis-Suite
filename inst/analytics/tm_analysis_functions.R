################################################################################
#              CPM TM ANALYSIS - HELPER FUNCTIONS
################################################################################
#
# This file contains all the core functions for CPM thermostability analysis
# from RotorGene Q Series Software data.
#
################################################################################

library(ggplot2)
library(scales)

#' Read RotorGene Q CSV Export
#' 
#' Parses the RotorGene Q "Transpose" CSV format for CPM assay data
#' 
#' @param filepath Path to the RotorGene Q CSV file
#' @return List containing temperature vector, sample IDs, names, and data matrix
#' @export
read_rotorgene_csv <- function(filepath) {
  
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  tryCatch({
    # Read the entire file
    raw_data <- readLines(filepath, warn = FALSE)
    
    # Find "Melt analysis" section - this marks the dF/dT data
    melt_idx <- which(grepl('Melt analysis', raw_data, ignore.case = TRUE))[1]
    
    if (is.na(melt_idx)) {
      stop("Could not find 'Melt analysis' section. Is this a RotorGene Q export with dF/dT data?")
    }
    
    # Find ID row AFTER the melt analysis marker
    # Look for line starting with "ID" after the melt_idx
    id_row_idx <- NA
    for (i in (melt_idx + 1):length(raw_data)) {
      if (grepl('^"ID"', raw_data[i])) {
        id_row_idx <- i
        break
      }
    }
    
    if (is.na(id_row_idx)) {
      stop("Could not find ID row in dF/dT section. Check file format.")
    }
    
    # Parse ID row (should be row 97 in your file)
    id_line <- raw_data[id_row_idx]
    id_parts <- strsplit(id_line, '","')[[1]]
    id_parts <- gsub('"', '', id_parts)
    sample_ids <- id_parts[-1]  # Remove "ID" label
    
    # Parse Name row (next line after IDs, should be row 98)
    name_line <- raw_data[id_row_idx + 1]
    name_parts <- strsplit(name_line, '","')[[1]]
    name_parts <- gsub('"', '', name_parts)
    sample_names <- name_parts[-1]  # Remove first column (usually "Page 1")
    
    # Ensure we have matching lengths
    n_samples <- min(length(sample_ids), length(sample_names))
    sample_ids <- sample_ids[1:n_samples]
    sample_names <- trimws(sample_names[1:n_samples])
    
    # Read dF/dT data starting from next row (should be row 99)
    data_start <- id_row_idx + 2
    data_lines <- raw_data[data_start:length(raw_data)]
    
    # Parse data lines
    temperature <- c()
    data_matrix <- matrix(nrow = 0, ncol = n_samples)
    
    for (line in data_lines) {
      if (nchar(trimws(line)) == 0) next  # Skip empty lines
      
      # Parse the line
      parts <- strsplit(line, '","')[[1]]
      parts <- gsub('"', '', parts)
      
      if (length(parts) < (n_samples + 1)) next  # Skip if not enough columns
      
      # First column is temperature
      temp_val <- suppressWarnings(as.numeric(parts[1]))
      if (is.na(temp_val)) next  # Skip if not a valid temperature
      
      # Remaining columns are dF/dT data
      data_vals <- suppressWarnings(as.numeric(parts[2:(n_samples + 1)]))
      
      # Only add if we have valid data
      if (any(!is.na(data_vals))) {
        temperature <- c(temperature, temp_val)
        data_matrix <- rbind(data_matrix, data_vals)
      }
    }
    
    if (length(temperature) == 0) {
      stop("No valid dF/dT data found. Check file format.")
    }
    
    # Convert to data frame for easier handling
    colnames(data_matrix) <- sample_names
    
    cat("   Found dF/dT data: ", length(temperature), " temperature points\n")
    cat("   Temperature range: ", min(temperature), " - ", max(temperature), "°C\n")
    
    # Return structured data
    list(
      temperature = temperature,
      sample_ids = sample_ids,
      sample_names = sample_names,
      data = data_matrix,
      n_samples = n_samples,
      n_points = length(temperature)
    )
    
  }, error = function(e) {
    stop("Error parsing RotorGene CSV: ", e$message, "\n",
         "Please ensure this is a RotorGene Q export with dF/dT data (Melt analysis section).")
  })
}


#' Read raw fluorescence AND dF/dT from RotorGene Q CSV
#'
#' Parses both the raw fluorescence block (rows ~23-25+) and the
#' dF/dT Melt Analysis block (rows ~97-99+).
#'
#' @param filepath Path to RotorGene Q CSV
#' @return List with: temperature, sample_ids, sample_names, data (dF/dT matrix),
#'         raw_temperature, raw_data (fluorescence matrix)
#' @export
read_rotorgene_csv_full <- function(filepath) {

  if (!file.exists(filepath)) stop("File not found: ", filepath)

  # Read all lines; strip Windows \r so trimws works correctly
  file_lines <- gsub("\r", "", readLines(filepath, warn = FALSE))

  # ── helper: split a CSV line on commas, strip surrounding quotes ────────────
  split_csv <- function(line) {
    trimws(gsub('^"|"$', '', strsplit(line, '","')[[1]]))
  }

  # ── helper: parse one ID/Name/Data block starting at id_idx ─────────────────
  # Reads until it hits another "ID" row, a known section header, or EOF.
  # Uses `next` (never `break`) so blank lines and stray text rows are skipped.
  parse_block <- function(id_idx, stop_before = integer(0)) {

    ids <- split_csv(file_lines[id_idx])[-1]
    ids <- ids[nchar(ids) > 0]

    nms <- split_csv(file_lines[id_idx + 1])[-1]
    # "Page 1" style label in col 1 already stripped; align length
    if (length(nms) > length(ids)) nms <- nms[seq_along(ids)]
    if (length(nms) < length(ids)) nms <- c(nms, rep("", length(ids) - length(nms)))

    n <- length(ids)
    if (n == 0) return(list(temperature = numeric(0), ids = character(0),
                            names = character(0), data = matrix(nrow=0, ncol=0), n = 0))

    temp_vec <- numeric(0)
    rows     <- list()

    # Determine the last line to scan: either the next stop_before index or EOF
    end_line <- if (length(stop_before) > 0) min(stop_before) - 1 else length(file_lines)
    end_line <- min(end_line, length(file_lines))

    for (i in (id_idx + 2):end_line) {
      line <- file_lines[i]
      if (nchar(trimws(line)) == 0) next          # skip blank lines
      parts <- split_csv(line)
      if (length(parts) == 0) next
      tv <- suppressWarnings(as.numeric(parts[1]))
      if (is.na(tv)) next                         # skip header/label rows
      if (length(parts) < n + 1) next             # skip short rows
      dv <- suppressWarnings(as.numeric(parts[2:(n + 1)]))
      temp_vec <- c(temp_vec, tv)
      rows[[length(rows) + 1]] <- dv
    }

    mat <- if (length(rows) > 0) {
      do.call(rbind, rows)
    } else {
      matrix(numeric(0), nrow = 0, ncol = n)
    }
    colnames(mat) <- nms

    list(temperature = temp_vec, ids = ids, names = nms, data = mat, n = n)
  }

  # ── locate all "ID" rows ────────────────────────────────────────────────────
  id_rows <- which(grepl('^"?ID"?\\s*,', file_lines) |
                   grepl('^"ID"', file_lines))
  if (length(id_rows) < 1)
    stop("No ID row found. Is this a RotorGene Q CSV export?")

  # dF/dT block: after the "Melt analysis" section header
  melt_idx <- which(grepl("Melt analysis", file_lines, ignore.case = TRUE))[1]
  if (is.na(melt_idx)) stop("Could not find 'Melt analysis' section.")

  dfdt_candidates <- id_rows[id_rows > melt_idx]
  if (length(dfdt_candidates) == 0)
    stop("No ID row found after 'Melt analysis' section.")
  dfdt_id_row <- min(dfdt_candidates)

  # Raw fluorescence block: first ID row in the file
  raw_id_row <- id_rows[1]

  # ── parse ──────────────────────────────────────────────────────────────────
  # Tell raw parser to stop before the Melt analysis section so it doesn't
  # accidentally pick up dF/dT data
  raw_blk  <- parse_block(raw_id_row,  stop_before = melt_idx)
  dfdt_blk <- parse_block(dfdt_id_row, stop_before = integer(0))

  # ── validate ───────────────────────────────────────────────────────────────
  if (length(dfdt_blk$temperature) == 0)
    stop("dF/dT data block is empty — check file format.")
  if (length(raw_blk$temperature) == 0)
    warning("Raw fluorescence block appears empty — only dF/dT will be available.")

  list(
    temperature      = dfdt_blk$temperature,
    sample_ids       = dfdt_blk$ids,
    sample_names     = dfdt_blk$names,
    data             = dfdt_blk$data,
    n_samples        = dfdt_blk$n,
    n_points         = length(dfdt_blk$temperature),
    raw_temperature  = raw_blk$temperature,
    raw_sample_ids   = raw_blk$ids,
    raw_sample_names = raw_blk$names,
    raw_data         = raw_blk$data
  )
}

#' Create Preview Plot
#' 
#' Generates a simple preview plot to help user identify peak regions
#' 
#' @param data Data frame with Temperature and dFdT columns
#' @param sample_name Name of the sample
#' @return ggplot object
#' @export
create_preview_plot <- function(data, sample_name) {
  
  # Normalize data for preview
  min_val <- min(data$dFdT, na.rm = TRUE)
  max_val <- max(data$dFdT, na.rm = TRUE)
  data$dFdT_norm <- (data$dFdT - min_val) / (max_val - min_val)
  
  # Create plot
  p <- ggplot(data, aes(x = Temperature, y = dFdT_norm)) +
    geom_line(color = "black", size = 1.2) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(
      title = "CPM Thermostability - Data Preview",
      subtitle = paste0("Sample: ", sample_name),
      x = "Temperature (°C)",
      y = "Normalized dF/dT"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_line(color = "gray95"),
      panel.grid.major = element_line(color = "gray90"),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5)
    )
  
  return(p)
}

#' Calculate Tm from CPM Data
#' 
#' Calculates melting temperature by finding the centroid of the dF/dT peak
#' within the specified temperature range
#' 
#' @param data Data frame with Temperature and dFdT columns
#' @param T_lower Lower temperature bound for integration
#' @param T_upper Upper temperature bound for integration
#' @param sample_name Name of the sample
#' @param sample_id ID of the sample
#' @return List with Tm, area, plot, and other results
#' @export

#' Calculate Full Width at Half Maximum (FWHM)
#' @keywords internal
.calc_fwhm <- function(temp, signal_norm) {
  peak_idx <- which.max(signal_norm)
  half_max  <- signal_norm[peak_idx] / 2

  # Left crossing
  left_t <- NA_real_
  for (i in seq(peak_idx, 1, -1)) {
    if (signal_norm[i] <= half_max) {
      if (i < peak_idx) {
        s1 <- signal_norm[i]; s2 <- signal_norm[i + 1]
        t1 <- temp[i];        t2 <- temp[i + 1]
        if (s2 != s1) left_t <- t1 + (half_max - s1) / (s2 - s1) * (t2 - t1)
      } else { left_t <- temp[i] }
      break
    }
  }
  # Right crossing
  right_t <- NA_real_
  for (i in seq(peak_idx, length(signal_norm))) {
    if (signal_norm[i] <= half_max) {
      if (i > peak_idx) {
        s1 <- signal_norm[i - 1]; s2 <- signal_norm[i]
        t1 <- temp[i - 1];        t2 <- temp[i]
        if (s2 != s1) right_t <- t1 + (half_max - s1) / (s2 - s1) * (t2 - t1)
      } else { right_t <- temp[i] }
      break
    }
  }
  if (is.na(left_t) || is.na(right_t)) return(NA_real_)
  round(right_t - left_t, 2)
}

calculate_tm <- function(data, T_lower, T_upper, sample_name, sample_id) {
  
  # Normalize the entire dataset
  min_val <- min(data$dFdT, na.rm = TRUE)
  max_val <- max(data$dFdT, na.rm = TRUE)
  data$dFdT_norm <- (data$dFdT - min_val) / (max_val - min_val)
  
  # Filter for the peak region
  peak_region <- subset(data, Temperature >= T_lower & Temperature <= T_upper)
  
  if (nrow(peak_region) < 2) {
    stop("Not enough data points in the selected temperature range.\n",
         "Try a wider range or check your data.")
  }
  
  # Calculate area under the curve (trapezoidal rule)
  x <- peak_region$Temperature
  y_norm <- peak_region$dFdT_norm
  
  # Trapezoidal integration
  area_norm <- sum(diff(x) * (head(y_norm, -1) + tail(y_norm, -1)) / 2)
  
  # Calculate centroid (weighted average temperature)
  # This is the Tm - the "center of mass" of the peak
  tm <- sum(x * y_norm) / sum(y_norm)
  
  # Create publication-quality plot
  plot <- ggplot(data, aes(x = Temperature, y = dFdT_norm)) +
    # Main curve
    geom_line(color = "black", size = 1.0) +
    # Shaded integration area
    geom_area(
      data = peak_region,
      aes(x = Temperature, y = dFdT_norm),
      fill = "#2E8B57",  # Sea green
      alpha = 0.4
    ) +
    # Tm line
    geom_vline(
      xintercept = tm,
      color = "#FF6347",  # Tomato red
      linetype = "dashed",
      size = 1.2
    ) +
    # Tm label - positioned at 85% height to avoid cutoff
    annotate(
      "label",
      x = tm,
      y = 0.85,
      label = sprintf("Tm = %.1f°C", tm),
      color = "#FF6347",
      fontface = "bold",
      size = 5,
      fill = "white",
      label.padding = unit(0.5, "lines"),
      label.size = 0.5
    ) +
    # Scales
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = pretty_breaks(n = 10),
      expand = c(0.01, 0)
    ) +
    # Labels
    labs(
      title = "CPM Thermostability Assay - Tm Analysis",
      subtitle = sprintf("Sample: %s (ID: %s)\nIntegration Range: %.0f - %.0f°C",
                        sample_name, sample_id, T_lower, T_upper),
      x = "Temperature (°C)",
      y = "Normalized dF/dT"
    ) +
    # Theme
    theme_minimal(base_size = 14) +
    theme(
      # Title styling
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "gray30",
        margin = margin(b = 10)
      ),
      # Axis styling
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, color = "black"),
      axis.line = element_line(color = "black", size = 0.6),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(0.2, "cm"),
      # Grid styling
      panel.grid.major = element_line(color = "gray85", size = 0.3),
      panel.grid.minor = element_line(color = "gray92", size = 0.2),
      # Background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # Return comprehensive results
  fwhm <- .calc_fwhm(peak_region$Temperature, peak_region$dFdT_norm)

  list(
    tm = tm,
    area = area_norm,
    fwhm = fwhm,
    n_points = nrow(peak_region),
    peak_data = peak_region,
    full_data = data,
    plot = plot,
    sample_name = sample_name,
    sample_id = sample_id,
    T_lower = T_lower,
    T_upper = T_upper
  )
}

#' Batch Analyze Multiple Samples
#' 
#' Convenience function to analyze multiple samples from the same file
#' 
#' @param filepath Path to RotorGene Q CSV
#' @param sample_ids Vector of sample IDs to analyze
#' @param T_lower Lower temperature for all samples
#' @param T_upper Upper temperature for all samples
#' @return List of results for each sample
#' @export
batch_analyze_tm <- function(filepath, sample_ids, T_lower, T_upper) {
  
  # Read data
  data_all <- read_rotorgene_csv(filepath)
  
  results_list <- list()
  
  for (sid in sample_ids) {
    # Find sample index
    idx <- which(data_all$sample_ids == sid)
    
    if (length(idx) == 0) {
      warning("Sample ID ", sid, " not found. Skipping.")
      next
    }
    
    # Extract sample data
    sample_data <- data.frame(
      Temperature = data_all$temperature,
      dFdT = data_all$data[, idx]
    )
    
    sample_data <- na.omit(sample_data)
    
    # Calculate Tm
    results <- calculate_tm(
      data = sample_data,
      T_lower = T_lower,
      T_upper = T_upper,
      sample_name = data_all$sample_names[idx],
      sample_id = sid
    )
    
    results_list[[sid]] <- results
  }
  
  return(results_list)
}

#' Compare Multiple Samples
#' 
#' Create overlay plot comparing Tm values across samples
#' 
#' @param results_list List of results from batch_analyze_tm or multiple calculate_tm calls
#' @return ggplot object with overlay
#' @export
compare_tm_samples <- function(results_list) {
  
  # Combine all data
  combined_data <- data.frame()
  
  for (i in seq_along(results_list)) {
    res <- results_list[[i]]
    temp_df <- res$full_data
    temp_df$Sample <- res$sample_name
    temp_df$Sample_ID <- res$sample_id
    combined_data <- rbind(combined_data, temp_df)
  }
  
  # Create overlay plot
  p <- ggplot(combined_data, aes(x = Temperature, y = dFdT_norm, color = Sample)) +
    geom_line(size = 1.2) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "CPM Thermostability - Sample Comparison",
      x = "Temperature (°C)",
      y = "Normalized dF/dT",
      color = "Sample"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_line(color = "gray95"),
      panel.grid.major = element_line(color = "gray88")
    )
  
  return(p)
}

#' Export Tm Summary Table
#' 
#' Create a summary data frame from multiple Tm analyses
#' 
#' @param results_list List of results from batch analysis
#' @return Data frame with Tm summary
#' @export
create_tm_summary <- function(results_list) {
  
  summary_df <- data.frame(
    Sample_ID = character(),
    Sample_Name = character(),
    Tm_degC = numeric(),
    Integration_Range = character(),
    Peak_Area = numeric(),
    N_Points = integer(),
    stringsAsFactors = FALSE
  )
  
  for (res in results_list) {
    summary_df <- rbind(summary_df, data.frame(
      Sample_ID = res$sample_id,
      Sample_Name = res$sample_name,
      Tm_degC = round(res$tm, 2),
      Integration_Range = sprintf("%.1f - %.1f°C", res$T_lower, res$T_upper),
      Peak_Area = round(res$area, 4),
      N_Points = res$n_points,
      stringsAsFactors = FALSE
    ))
  }
  
  return(summary_df)
}


# =============================================================================
# AUTOMATIC PEAK DETECTION  (v3 – complete rewrite)
# =============================================================================
#
# Algorithm:
#  1. Gaussian-smooth the signal (for detection only; Tm still uses raw data)
#  2. Find local maxima on the smoothed curve
#  3. Score each maximum by proper isolation-based prominence
#  4. Merge maxima that are too close (keep the taller one)
#  5. For EACH accepted peak:
#       • Inner boundary  = valley between this peak and its neighbour (smoothed)
#       • Outer boundary  = where smoothed signal falls below threshold × peak-height
#  6. Calculate centroid Tm on the unsmoothed signal within those boundaries
#
# Key fixes vs v2:
#  • Outer boundaries are THRESHOLD-capped, not walk-to-end
#  • sigma is expressed in °C, converted to index-space automatically
#  • Default min separation = 8 °C (prevents shoulder splitting)
#  • Baseline level removed before prominence scoring (handles elevated baselines)
# =============================================================================

calculate_tm_automatic <- function(
    data,
    T_min            = NULL,
    T_max            = NULL,
    min_prominence   = 0.10,   # on 0-1 normalised scale
    min_peak_sep_deg = 8,      # minimum distance between peaks (°C)
    smooth_sigma_deg = 3,      # Gaussian sigma for peak detection (°C)
    boundary_thresh  = 0.12,   # outer boundary at this fraction of peak height
    sample_name      = "Unknown",
    sample_id        = "Unknown"
) {

  # ── 0. Restrict to analysis window ─────────────────────────────────────────
  full_plot_data <- data    # keep ALL points for the full trace in plot
  if (!is.null(T_min)) data <- data[data$Temperature >= T_min, ]
  if (!is.null(T_max)) data <- data[data$Temperature <= T_max, ]

  if (nrow(data) < 10)
    stop("Not enough data points in the specified temperature range.")

  temp <- data$Temperature
  n    <- length(temp)

  # Average step size → convert degree-based params to index-based
  step  <- mean(diff(temp))
  sigma_idx   <- max(1, smooth_sigma_deg   / step)
  min_sep_idx <- max(1, min_peak_sep_deg   / step)

  # ── 1. Normalise using the FULL trace range so outside-window points
  #    stay within [0,1] and are never dropped by the y-axis scale
  lo_full <- min(full_plot_data$dFdT, na.rm = TRUE)
  hi_full <- max(full_plot_data$dFdT, na.rm = TRUE)
  if (hi_full == lo_full) stop("Signal is flat – no peaks to detect.")
  full_plot_data$dFdT_norm <- (full_plot_data$dFdT - lo_full) / (hi_full - lo_full)
  # Filtered window uses the same scale
  data$dFdT_norm <- (data$dFdT - lo_full) / (hi_full - lo_full)
  lo <- lo_full; hi <- hi_full   # keep lo/hi for any downstream use

  # ── 2. Gaussian smooth (for detection only) ────────────────────────────────
  sm <- .gaussian_smooth(data$dFdT_norm, sigma_idx)

  # ── 3. Local maxima on smoothed signal ────────────────────────────────────
  # A maximum at index i: sm[i] > sm[i-1]  AND  sm[i] >= sm[i+1]
  lmax <- which(sm[-1] <= sm[-n] & c(FALSE, sm[-1] > sm[-n])[-1])
  # Simpler: sign-change of first differences
  d   <- diff(sm)
  lmax <- which(d[-length(d)] > 0 & d[-1] <= 0) + 1L

  if (length(lmax) == 0)
    stop("No local maxima found. Try widening the temperature range.")

  # ── 4. Prominence (isolation-based) ───────────────────────────────────────
  # Estimate baseline as the 5th-percentile of the smoothed signal
  baseline <- quantile(sm, 0.05)
  sm_base  <- pmax(sm - baseline, 0)   # baseline-corrected

  prom <- .calc_prominence(sm_base, lmax)

  keep <- prom >= min_prominence
  lmax <- lmax[keep]
  prom <- prom[keep]

  if (length(lmax) == 0)
    stop(sprintf(
      "No peaks above min_prominence=%.2f. Try lowering it or adjusting the temperature range.",
      min_prominence))

  # ── 5. Merge peaks that are too close (keep taller) ───────────────────────
  lmax <- .merge_close(lmax, sm, min_sep_idx)

  # ── 6. Boundaries for every accepted peak ─────────────────────────────────
  bounds <- .valley_boundaries(sm, lmax, boundary_thresh)

  # ── 7. Compute Tm on unsmoothed signal per peak ───────────────────────────
  colors_palette <- c("#2E8B57","#4169E1","#D55E00","#9370DB",
                      "#DC143C","#008B8B","#B8860B")

  peak_results <- list()

  for (i in seq_along(lmax)) {
    si <- bounds$starts[i]
    ei <- bounds$ends[i]
    if (ei <= si) next

    pr <- data[si:ei, ]
    if (nrow(pr) < 2) next

    x <- pr$Temperature
    y <- pr$dFdT_norm
    if (max(y) < 0.04) next           # skip essentially-flat regions

    tm   <- sum(x * y) / sum(y)
    area <- sum(diff(x) * (head(y,-1) + tail(y,-1)) / 2)
    j    <- length(peak_results) + 1

    fwhm_pk <- .calc_fwhm(pr$Temperature, (pr$dFdT - lo_full) / (hi_full - lo_full))
    peak_results[[j]] <- list(
      peak_number = j,
      tm          = tm,
      area        = area,
      fwhm        = fwhm_pk,
      T_start     = x[1],
      T_end       = x[length(x)],
      height      = sm[lmax[i]],
      prominence  = .calc_prominence(sm_base, lmax[i]),
      n_points    = nrow(pr),
      peak_data   = pr
    )
  }

  if (length(peak_results) == 0)
    stop("All detected peaks were filtered out. Try adjusting boundary_thresh or min_prominence.")

  # ── 8. Build plot – same style as manual mode ─────────────────────────────
  # Green fill colours for up to 5 peaks, matching manual sea-green
  fill_colors <- c("#2E8B57", "#4169E1", "#D55E00", "#9370DB", "#DC143C",
                   "#008B8B", "#B8860B")
  # Tm line colours – use orange (same as manual) for single peak,
  # otherwise use the fill colour family
  tm_colors   <- c("#FF6347", "#4169E1", "#D55E00", "#9370DB", "#DC143C",
                   "#008B8B", "#B8860B")

  # Build n_peaks subtitle clause
  n_pk <- length(peak_results)
  peak_clause <- if (n_pk == 1) "1 peak detected" else sprintf("%d peaks detected", n_pk)

  p <- ggplot(full_plot_data, aes(x = Temperature, y = dFdT_norm)) +
    # Full trace in black
    geom_line(color = "black", size = 1.0)

  for (i in seq_along(peak_results)) {
    pk    <- peak_results[[i]]
    fcol  <- fill_colors[((i - 1) %% length(fill_colors)) + 1]
    tcol  <- tm_colors  [((i - 1) %% length(tm_colors))   + 1]

    # Shaded integration region
    p <- p + geom_area(
      data        = pk$peak_data,
      aes(x = Temperature, y = dFdT_norm),
      inherit.aes = FALSE,
      fill = fcol, alpha = 0.40
    )

    # Dashed Tm vertical line
    p <- p + geom_vline(
      xintercept = pk$tm,
      color      = tcol,
      linetype   = "dashed",
      size       = 1.2
    )

    # Tm label box – stagger vertically for multi-peak
    lbl_y <- 0.85 - (i - 1) * 0.11
    lbl_y <- max(lbl_y, 0.20)
    lbl   <- if (n_pk == 1) sprintf("Tm = %.1f\u00b0C", pk$tm) else
                             sprintf("Tm%d = %.1f\u00b0C", i, pk$tm)

    p <- p + annotate(
      "label",
      x = pk$tm, y = lbl_y,
      label         = lbl,
      color         = tcol,
      fontface      = "bold",
      size          = 5,
      fill          = "white",
      label.padding = unit(0.5, "lines"),
      label.size    = 0.5
    )
  }

  # Build subtitle including integration range(s)
  if (n_pk == 1) {
    pk1 <- peak_results[[1]]
    sub_text <- sprintf(
      "Sample: %s (ID: %s)\nAnalysis window: %.0f - %.0f\u00b0C  |  %s",
      sample_name, sample_id,
      if (!is.null(T_min)) T_min else min(full_plot_data$Temperature),
      if (!is.null(T_max)) T_max else max(full_plot_data$Temperature),
      peak_clause
    )
  } else {
    sub_text <- sprintf(
      "Sample: %s (ID: %s)  |  %s",
      sample_name, sample_id, peak_clause
    )
  }

  p <- p +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = c(0.01, 0)
    ) +
    coord_cartesian(ylim = c(-0.02, 1.05)) +
    labs(
      title    = "CPM Thermostability Assay - Tm Analysis",
      subtitle = sub_text,
      x        = "Temperature (\u00b0C)",
      y        = "Normalized dF/dT"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title    = element_text(size = 18, face = "bold", hjust = 0.5,
                                   margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30",
                                   margin = margin(b = 10)),
      axis.title        = element_text(size = 14, face = "bold"),
      axis.text         = element_text(size = 12, color = "black"),
      axis.line         = element_line(color = "black", size = 0.6),
      axis.ticks        = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(0.2, "cm"),
      panel.grid.major  = element_line(color = "gray85", size = 0.3),
      panel.grid.minor  = element_line(color = "gray92", size = 0.2),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      plot.margin       = margin(15, 15, 15, 15)
    )

  # ── 9. Summary table ──────────────────────────────────────────────────────
  summary_df <- data.frame(
    Peak       = sapply(peak_results, `[[`, "peak_number"),
    Tm_degC    = round(sapply(peak_results, `[[`, "tm"),         2),
    Range      = sapply(peak_results, function(x)
                   sprintf("%.1f\u2013%.1f\u00b0C", x$T_start, x$T_end)),
    Area       = round(sapply(peak_results, `[[`, "area"),       4),
    Height     = round(sapply(peak_results, `[[`, "height"),     3),
    Prominence = round(sapply(peak_results, `[[`, "prominence"), 3),
    N_Points   = sapply(peak_results, `[[`, "n_points")
  )

  list(
    peak_results = peak_results,
    summary      = summary_df,
    plot         = p,
    full_data    = full_plot_data,
    sample_name  = sample_name,
    sample_id    = sample_id,
    T_min        = T_min,
    T_max        = T_max,
    n_peaks      = length(peak_results)
  )
}

# =============================================================================
# Internal helpers (prefixed with . so they don't clutter the namespace)
# =============================================================================

.gaussian_smooth <- function(y, sigma) {
  # Pure-R Gaussian convolution; no external packages needed
  n      <- length(y)
  half   <- ceiling(4 * sigma)          # window half-width: ±4σ
  result <- numeric(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - half)
    hi <- min(n,  i + half)
    j  <- lo:hi
    w  <- exp(-0.5 * ((j - i) / sigma)^2)
    result[i] <- sum(w * y[j]) / sum(w)
  }
  result
}

.calc_prominence <- function(y, peaks) {
  # For each peak, prominence = peak_height - max(left_col_min, right_col_min)
  # where a "col" min is the minimum between this peak and the nearest
  # TALLER peak on that side.
  n <- length(y)
  sapply(peaks, function(pk) {
    h             <- y[pk]
    taller_left   <- peaks[peaks  < pk & y[peaks] >= h]
    taller_right  <- peaks[peaks  > pk & y[peaks] >= h]
    left_col_min  <- if (length(taller_left)  > 0) min(y[max(taller_left):pk])  else y[1]
    right_col_min <- if (length(taller_right) > 0) min(y[pk:min(taller_right)]) else y[n]
    h - max(left_col_min, right_col_min)
  })
}

.merge_close <- function(peaks, y, min_sep_idx) {
  # Iteratively merge the pair of peaks that is too close, keeping the taller
  if (length(peaks) <= 1) return(peaks)
  repeat {
    gaps <- diff(peaks)
    too_close <- which(gaps < min_sep_idx)
    if (length(too_close) == 0) break
    i <- too_close[1]          # handle left-most conflict first
    if (y[peaks[i]] >= y[peaks[i + 1]]) {
      peaks <- peaks[-(i + 1)]
    } else {
      peaks <- peaks[-i]
    }
  }
  peaks
}

.valley_boundaries <- function(sm, peaks, boundary_thresh) {
  # Returns list(starts, ends) – one per peak
  n <- length(sm)
  starts <- integer(length(peaks))
  ends   <- integer(length(peaks))

  for (i in seq_along(peaks)) {
    pk <- peaks[i]
    ph <- sm[pk]
    thr <- ph * boundary_thresh   # absolute threshold for outer edges

    # ── Left boundary ────────────────────────────────────────────────────
    if (i > 1) {
      # Inner boundary: minimum of smoothed signal between this and prev peak
      prev  <- peaks[i - 1]
      seg   <- prev:pk
      starts[i] <- seg[which.min(sm[seg])]
    } else {
      # Outer boundary: walk left until signal < threshold OR rising again
      idx <- pk
      while (idx > 1 && sm[idx - 1] >= thr && sm[idx - 1] <= sm[idx]) idx <- idx - 1L
      # Also cap at the local minimum (stop if signal starts rising again)
      starts[i] <- max(1L, idx)
    }

    # ── Right boundary ───────────────────────────────────────────────────
    if (i < length(peaks)) {
      # Inner boundary: minimum between this and next peak
      nxt <- peaks[i + 1]
      seg <- pk:nxt
      ends[i] <- seg[which.min(sm[seg])]
    } else {
      # Outer boundary: walk right until signal < threshold OR rising again
      idx <- pk
      while (idx < n && sm[idx + 1] >= thr && sm[idx + 1] <= sm[idx]) idx <- idx + 1L
      ends[i] <- min(n, idx)
    }
  }

  list(starts = starts, ends = ends)
}

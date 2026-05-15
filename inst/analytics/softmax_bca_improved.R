################################################################################
#               BCA ASSAY ANALYZER - IMPROVED FUNCTIONS
################################################################################
#
# This file contains improved helper functions for analyzing BCA assay data
# from SoftMax Pro Software exports.
#
# Key improvements:
# - Better error handling with clear messages
# - Modern ggplot2 aesthetics for standard curves
# - Professional table formatting
# - Progress indicators
# - Input validation
#
################################################################################

# Load required libraries quietly
suppressPackageStartupMessages({
  library(readr)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(scales)
})

################################################################################
#                         CORE READING FUNCTIONS
################################################################################

#' Read SoftMax Pro BCA Assay Data
#' 
#' Reads .xls exports from SoftMax Pro (tab-delimited with UTF-16LE encoding)
#' 
#' @param file_path Path to the .xls file from SoftMax Pro
#' @return List containing plate data, groups, metadata, and standards
#' @export
read_softmax_bca <- function(file_path) {
  
  # Check required packages are available
  required_pkgs <- c("readr", "dplyr", "tidyr")
  missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required packages: %s\n\nPlease install with:\n  install.packages(c(%s))",
      paste(missing, collapse = ", "),
      paste(paste0('"', missing, '"'), collapse = ", ")
    ))
  }
  
  if (!file.exists(file_path)) {
    stop(sprintf(
      "ERROR: File not found: %s\n\nPlease check:\n  - File path is correct\n  - File exists\n  - You have read permissions",
      file_path
    ))
  }
  
  tryCatch({
    # Read file with proper encoding handling
    lines <- .read_file_safe(file_path)
    
    # Parse the data structure
    parsed_data <- .parse_softmax_structure(lines, file_path)
    
    return(parsed_data)
    
  }, error = function(e) {
    stop(sprintf(
      "ERROR reading SoftMax file\nFile path: %s\nFile name: %s\nError: %s",
      file_path, basename(file_path), e$message
    ))
  })
}

#' Safe file reader handling UTF-16LE and NUL characters
#' @keywords internal
.read_file_safe <- function(path) {
  # Helper: split into lines and normalize line endings
  split_lines <- function(txt) {
    # Normalize line endings (handles Windows CRLF and Mac CR)
    txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
    txt <- gsub("\r", "\n", txt, fixed = TRUE)
    # Split on newlines
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    return(lines)
  }

  # Try encodings in order most-to-least common for SoftMax Pro
  for (enc in c("UTF-16LE", "UTF-16", "UTF-8", "latin1")) {
    lines <- tryCatch({
      txt <- readr::read_file(path, locale = readr::locale(encoding = enc))
      ln  <- split_lines(txt)
      # Accept if we got non-trivial content (> 5 non-blank lines)
      if (sum(nzchar(trimws(ln))) > 5) ln else NULL
    }, error = function(e) NULL)

    if (!is.null(lines)) return(lines)
  }

  # Binary fallback: read raw bytes, strip NUL, try iconv
  tryCatch({
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    raw_bytes <- readBin(con, what = "raw", n = file.info(path)$size)
    # Detect UTF-16LE BOM (FF FE) or UTF-16BE BOM (FE FF)
    if (length(raw_bytes) >= 2 &&
        raw_bytes[1] == as.raw(0xFF) && raw_bytes[2] == as.raw(0xFE)) {
      txt <- iconv(rawToChar(raw_bytes[raw_bytes != as.raw(0)]),
                   from = "UTF-8", to = "UTF-8", sub = "")
    } else {
      txt <- rawToChar(raw_bytes[raw_bytes != as.raw(0)])
    }
    split_lines(txt)
  }, error = function(e) {
    stop(sprintf(
      "Could not read file.\nPath: %s\nBasename: %s\nError: %s",
      path, basename(path), e$message))
  })
}

#' Parse SoftMax file structure
#' @keywords internal
.parse_softmax_structure <- function(lines, file_path) {
  
  # Extract metadata
  meta <- .extract_metadata(lines, file_path)
  
  # Extract groups (including standards and unknowns)
  groups <- .extract_groups(lines, meta$plate_id)
  
  # Return structured data
  list(
    groups = groups,
    meta = meta,
    file = file_path
  )
}

#' Extract metadata from file
#' @keywords internal
.extract_metadata <- function(lines, file_path) {
  meta_line <- lines[grepl("^Original Filename:", lines)]
  original_filename <- NA_character_
  date_last_saved <- NA_character_
  
  if (length(meta_line)) {
    m <- regmatches(meta_line, regexec("^Original Filename:\\s*(.*?);\\s*Date Last Saved:\\s*(.*)$", meta_line))[[1]]
    if (length(m) >= 2) original_filename <- m[2]
    if (length(m) >= 3) date_last_saved <- m[3]
  }
  
  list(
    file = file_path,
    original_filename = original_filename,
    date_last_saved = date_last_saved,
    plate_id = if (!is.na(original_filename)) original_filename else basename(file_path)
  )
}

#' Extract group data (standards, unknowns, etc.)
#' @keywords internal
.extract_groups <- function(lines, plate_id) {
  
  split_tab <- function(x) strsplit(x, "\t", fixed = TRUE)[[1]]
  clean_names <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }
  
  # Trim each line then test – handles BOM, leading whitespace, \r artefacts
  lines_trim <- trimws(lines)
  group_starts <- which(grepl("^Group:", lines_trim, ignore.case = TRUE))
  all_groups <- tibble()
  
  if (length(group_starts) == 0) {
    warning("No groups found in file. Check if file is a valid SoftMax export.")
    return(all_groups)
  }
  
  for (k in seq_along(group_starts)) {
    start <- group_starts[k]
    group_name <- sub("^Group:\\s*", "", lines_trim[start], ignore.case = TRUE)
    
    # Find header
    h <- start + 1
    while (h <= length(lines) && trimws(lines[h]) == "") h <- h + 1
    if (h > length(lines)) next
    
    header_raw <- split_tab(lines[h])
    header <- clean_names(header_raw)
    
    # Extract data lines
    data_lines <- character()
    p <- h + 1
    stopper <- function(ln) grepl("^(Group Column|Group Summaries|~End|Group:)", ln)
    
    while (p <= length(lines)) {
      ln <- lines[p]
      if (trimws(ln) == "") {
        nxt <- if (p + 1 <= length(lines)) lines[p + 1] else ""
        if (stopper(nxt)) break
        p <- p + 1
        next
      }
      if (stopper(ln)) break
      data_lines <- c(data_lines, ln)
      p <- p + 1
    }
    
    if (length(data_lines) == 0) next
    
    # Parse data
    rows <- strsplit(data_lines, "\t", fixed = TRUE)
    maxlen <- length(header)
    rows <- lapply(rows, function(x) {
      length(x) <- max(maxlen, length(x))
      x
    })
    
    df <- as_tibble(do.call(rbind, rows), .name_repair = "minimal")
    names(df) <- header[seq_len(ncol(df))]
    
    # Fill down sample and concentration columns
    # Replace whitespace-only cells with NA so tidyr::fill propagates correctly.
    # Some SoftMax exports use " " (a space) instead of "" in blank cells,
    # which prevents fill-down from working on concentration/sample columns.
    df <- df |>
      mutate(across(where(is.character), ~ if_else(trimws(.x) == "", NA_character_, .x)))

    fill_cols <- intersect(c("sample", "conc", "backcalcconc", "back_calc_conc"), names(df))
    if (length(fill_cols)) {
      df <- tidyr::fill(df, all_of(fill_cols), .direction = "down")
    }
    
    # Convert numeric columns
    keep_char <- intersect(c("sample", "wells", "r"), names(df))
    num_cols <- setdiff(names(df), keep_char)
    if (length(num_cols)) {
      df <- df |>
        mutate(across(all_of(num_cols), ~ suppressWarnings(readr::parse_number(.x, na = c("", "NA")))))
    }
    
    # Add group and file info
    df <- df |>
      mutate(
        group = group_name,
        file = plate_id
      )
    
    all_groups <- bind_rows(all_groups, df)
  }
  
  all_groups
}

################################################################################
#                    STANDARD CURVE FUNCTIONS
################################################################################

#' Extract Standards from Data
#' 
#' @param data_out Output from read_softmax_bca
#' @return Tibble with concentration, mean, SD, and n
#' @export
extract_standards <- function(data_out) {
  
  groups <- data_out$groups
  
  if (is.null(groups) || nrow(groups) == 0) {
    stop("ERROR: No group data found in file. Cannot extract standards.")
  }
  
  # Look for "Standards" group
  standards <- groups |>
    filter(grepl("(?i)standard", group))
  
  if (nrow(standards) == 0) {
    stop(
      "ERROR: No 'Standards' group found in data.\n\n",
      "Available groups:\n  ",
      paste(unique(groups$group), collapse = "\n  "),
      "\n\nPlease ensure your SoftMax file contains a 'Standards' group."
    )
  }
  
  # Check for required columns
  if (!"conc" %in% names(standards)) {
    stop("ERROR: 'conc' column not found in Standards group.")
  }
  
  # Look for signal column - can be "value", "od", "meanvalue", etc.
  signal_col <- NULL
  possible_cols <- c("value", "meanvalue", "od", "absorbance", "signal")
  for (col in possible_cols) {
    if (col %in% names(standards)) {
      signal_col <- col
      break
    }
  }
  
  if (is.null(signal_col)) {
    stop(
      "ERROR: No signal/absorbance column found in Standards group.\n",
      "  Available columns: ", paste(names(standards), collapse = ", "), "\n",
      "  Looking for one of: ", paste(possible_cols, collapse = ", ")
    )
  }
  
  # Summarize standards using the detected signal column
  std_summary <- standards |>
    rename(signal = !!signal_col) |>
    filter(!is.na(conc), !is.na(signal)) |>
    group_by(conc) |>
    summarise(
      mean_signal = mean(signal, na.rm = TRUE),
      sd_signal = sd(signal, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    arrange(conc)
  
  if (nrow(std_summary) < 3) {
    warning(
      "WARNING: Only ", nrow(std_summary), " standard concentrations found.\n",
      "  Linear regression may not be reliable with < 3 points.\n",
      "  Consider using more standards."
    )
  }
  
  std_summary
}

#' Create Standard Curve with Linear Fit
#' 
#' Fits a linear model to standard curve data and creates a publication-quality plot
#' 
#' @param data_out Output from read_softmax_bca
#' @param theme_style "publication", "presentation", or "minimal"
#' @return List with model, R², equation, and plot
#' @export
create_standard_curve <- function(data_out, theme_style = "publication") {
  
  # Extract standards
  standards <- extract_standards(data_out)
  
  # Fit linear model
  model <- lm(mean_signal ~ conc, data = standards)
  r2 <- summary(model)$r.squared
  
  # Get coefficients
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # Create equation text
  equation <- sprintf("y = %.4f + %.4fx", intercept, slope)
  r2_text <- sprintf("R² = %.4f", r2)
  
  # Generate predictions for smooth line
  conc_range <- range(standards$conc)
  pred_data <- tibble(
    conc = seq(conc_range[1], conc_range[2], length.out = 100)
  )
  pred_data$mean_signal <- predict(model, newdata = pred_data)
  
  # Create plot with modern aesthetics
  p <- ggplot(standards, aes(x = conc, y = mean_signal)) +
    # Add error bars if we have SD
    geom_errorbar(
      aes(ymin = mean_signal - sd_signal, ymax = mean_signal + sd_signal),
      width = 0.02 * diff(conc_range),
      color = "gray40",
      size = 0.5
    ) +
    # Add fitted line
    geom_line(
      data = pred_data,
      aes(x = conc, y = mean_signal),
      color = "#0072B2",
      size = 1.2
    ) +
    # Add points
    geom_point(
      size = 4,
      shape = 21,
      fill = "#D55E00",
      color = "black",
      stroke = 1
    ) +
    # Add equation and R² annotation
    annotate(
      "text",
      x = min(standards$conc) + 0.05 * diff(conc_range),
      y = max(standards$mean_signal) - 0.05 * diff(range(standards$mean_signal)),
      label = paste(equation, r2_text, sep = "\n"),
      hjust = 0,
      vjust = 1,
      size = 4.5,
      fontface = "bold"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.05)),
      breaks = pretty_breaks(n = 8)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.15)),
      breaks = pretty_breaks(n = 8)
    ) +
    labs(
      title = "BCA Assay Standard Curve",
      subtitle = "Linear Regression Fit",
      x = "Protein Concentration (mg/mL)",
      y = "Absorbance (562 nm)"
    )
  
  # Apply theme
  if (theme_style == "publication") {
    p <- p + theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.minor = element_line(color = "gray95", linewidth = 0.2),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      )
  } else if (theme_style == "presentation") {
    p <- p + theme_minimal(base_size = 18) +
      theme(
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18, color = "black", face = "bold"),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(color = "black", linewidth = 0.8),
        panel.grid.major = element_line(color = "gray85", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  } else {
    p <- p + theme_minimal() +
      theme(
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank()
      )
  }
  
  # Return comprehensive results
  list(
    model = model,
    r2 = r2,
    intercept = intercept,
    slope = slope,
    equation = equation,
    standards = standards,
    plot = p
  )
}

################################################################################
#                    PROTEIN YIELD CALCULATION
################################################################################

#' Get Mean Adjusted Result from SoftMax File
#' 
#' Extracts the calculated protein concentration from the file
#' 
#' @param file_path Path to SoftMax file
#' @return Numeric concentration value
#' @export
get_mean_adjusted_result <- function(file_path) {
  
  lines <- .read_file_safe(file_path)
  label_regex <- "(?i)^\\s*Mean\\s+Adjusted\\s+Result:?\\s*$"
  
  for (i in seq_along(lines)) {
    cells <- strsplit(lines[i], "\t", fixed = TRUE)[[1]]
    if (!length(cells)) next
    
    hits <- which(grepl(label_regex, cells, perl = TRUE))
    if (!length(hits)) next
    
    h <- hits[1]
    if (h + 1 <= length(cells)) {
      val_raw <- cells[h + 1]
      val <- suppressWarnings(readr::parse_number(val_raw))
      if (!is.na(val)) return(val)
    }
  }
  
  warning(
    "Could not find 'Mean Adjusted Result' in file.\n",
    "  Check that:\n",
    "  - File contains analyzed sample data\n",
    "  - SoftMax has calculated the concentration\n",
    "  - File export includes all results"
  )
  return(NA_real_)
}

#' Calculate Protein Yield and Create Results Table
#' 
#' @param file_path Path to SoftMax file
#' @param std_curve Standard curve object from create_standard_curve()
#' @param volume_ml Sample volume in mL
#' @param digits Number of decimal places
#' @param title Custom table title
#' @return List with gt table and data frame
#' @export
calculate_protein_yield <- function(file_path,
                                   std_curve,
                                   volume_ml,
                                   digits = 2,
                                   title = "BCA Assay Protein Yield Summary",
                                   manual_concentration = NULL) {
  
  # Validate inputs
  if (missing(file_path) || !file.exists(file_path)) {
    stop("ERROR: Valid file_path is required.")
  }
  
  if (missing(std_curve) || is.null(std_curve$r2)) {
    stop("ERROR: Valid std_curve object is required from create_standard_curve().")
  }
  
  if (volume_ml <= 0) {
    stop("ERROR: volume_ml must be > 0")
  }
  
  # Get concentration - either from manual input or from file
  if (!is.null(manual_concentration)) {
    # Manual mode - use provided concentration
    conc_mg_ml <- manual_concentration
    cat("   Using manually entered concentration: ", conc_mg_ml, " mg/mL\n")
  } else {
    # Automatic mode - extract from file
    conc_mg_ml <- get_mean_adjusted_result(file_path)
    
    if (is.na(conc_mg_ml)) {
      stop(
        "ERROR: Could not extract protein concentration from file.\n",
        "  File: ", basename(file_path), "\n\n",
        "  Troubleshooting:\n",
        "  - Ensure SoftMax has calculated the 'Mean Adjusted Result'\n",
        "  - Check that the file contains sample data (not just standards)\n",
        "  - Verify the file export is complete\n",
        "  - Or use MANUAL mode to enter concentration directly"
      )
    }
  }
  
  # Calculate yield
  total_yield_mg <- conc_mg_ml * volume_ml
  
  # Create data frame
  results_df <- tibble(
    `Concentration (mg/mL)` = conc_mg_ml,
    `Volume (mL)` = volume_ml,
    `Total Yield (mg)` = total_yield_mg,
    `R²` = std_curve$r2
  )
  
  # Return results data frame
  list(
    data = results_df,
    gt = NULL  # gt table not created (package not required)
  )
}

################################################################################
#                    CONVENIENCE WRAPPER FUNCTION
################################################################################

#' Complete BCA Assay Analysis (One Function)
#' 
#' Performs complete analysis: reads data, fits curve, calculates yield
#' 
#' @param file Path to SoftMax .xls file
#' @param volume_ml Sample volume in mL
#' @param digits Decimal places for results
#' @param title Custom table title
#' @param save_outputs Automatically save plot and table
#' @return List with standard curve, results, and outputs
#' @export
analyze_bca_assay <- function(file,
                              volume_ml = 1.0,
                              digits = 2,
                              title = "BCA Assay Protein Yield Summary",
                              save_outputs = TRUE,
                              manual_concentration = NULL) {
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("             BCA ASSAY ANALYSIS\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("\n")
  cat("File:", basename(file), "\n")
  cat("Volume:", volume_ml, "mL\n")
  cat("\n")
  
  # Read data
  cat("📊 Reading data...\n")
  data_out <- read_softmax_bca(file)
  cat("   ✓ Complete\n\n")
  
  # Create standard curve
  cat("📈 Creating standard curve...\n")
  std_curve <- create_standard_curve(data_out)
  cat(sprintf("   ✓ R² = %.4f\n\n", std_curve$r2))
  
  # Calculate yield
  cat("🧬 Calculating yield...\n")
  results <- calculate_protein_yield(file, std_curve, volume_ml, digits, title, manual_concentration)
  cat("   ✓ Complete\n\n")
  
  # Save outputs
  if (save_outputs) {
    cat("💾 Saving outputs...\n")
    base_name <- tools::file_path_sans_ext(basename(file))
    output_dir <- dirname(file)
    
    # Save plot
    plot_file <- file.path(output_dir, paste0(base_name, "_StandardCurve.png"))
    ggsave(plot_file, std_curve$plot, width = 8, height = 6, dpi = 300, bg = "white")
    cat("   ✓ Plot:", basename(plot_file), "\n")
    
    # Save table (only if gt table was generated)
    if (!is.null(results$gt)) {
      if (requireNamespace("webshot2", quietly = TRUE) && requireNamespace("gt", quietly = TRUE)) {
        table_file <- file.path(output_dir, paste0(base_name, "_Results.png"))
        gt::gtsave(results$gt, table_file, vwidth = 1200, vheight = 600, zoom = 2)
        cat("   ✓ Table:", basename(table_file), "\n")
      } else if (requireNamespace("gt", quietly = TRUE)) {
        table_file <- file.path(output_dir, paste0(base_name, "_Results.html"))
        gt::gtsave(results$gt, table_file)
        cat("   ✓ Table:", basename(table_file), "\n")
      }
    }
    cat("\n")
  }
  
  cat("✓ Analysis complete!\n\n")
  
  # Return comprehensive results
  list(
    standard_curve = std_curve,
    results = results,
    data = data_out
  )
}

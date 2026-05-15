################################################################################
#  utils_theme.R  --  Shared themes, UI primitives, and helper-load diagnostics
################################################################################

# ---- App palette (mirrors CSS custom properties in www/custom.css) ----------
CG_PALETTE <- list(
  bg_deep      = "#080C14",
  bg_card      = "#0F1623",
  bg_card2     = "#111927",
  border       = "#1E2D45",
  border_light = "#243652",
  accent       = "#00C2FF",
  accent_green = "#00E5A0",
  accent_warm  = "#FF7B47",
  accent_purple= "#A78BFA",
  danger       = "#FF5C5C",
  txt          = "#E8F0FE",
  muted        = "#7A8FAD"
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- ggplot themes ----------------------------------------------------------
theme_cg_dark <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
      panel.background = ggplot2::element_rect(fill = CG_PALETTE$bg_card, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = CG_PALETTE$border, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_line(colour = "#161E2E",        linewidth = 0.2),
      text             = ggplot2::element_text(colour = CG_PALETTE$txt),
      axis.text        = ggplot2::element_text(colour = CG_PALETTE$muted),
      plot.title       = ggplot2::element_text(face = "bold", colour = CG_PALETTE$txt)
    )
}

theme_cg_publication <- function() {
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    text             = ggplot2::element_text(colour = "black"),
    axis.text        = ggplot2::element_text(colour = "black"),
    panel.grid.major = ggplot2::element_line(colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(colour = "grey95"),
    axis.line        = ggplot2::element_line(colour = "black"),
    plot.title       = ggplot2::element_text(colour = "black")
  )
}

# ---- Small UI primitives ----------------------------------------------------
status_pill <- function(state = c("ready", "error", "warning"), text) {
  state <- match.arg(state)
  shiny::div(class = paste("status-pill", state),
             shiny::div(class = "dot"),
             text)
}

info_box <- function(...) {
  shiny::div(class = "info-box",
             shiny::tags$span("\u2139", class = "info-icon"),
             ...)
}

step_title <- function(n, label) {
  shiny::div(class = "lab-card-title",
             shiny::span(class = "step-number", as.character(n)),
             label)
}

lab_card <- function(...) shiny::div(class = "lab-card", ...)

plot_placeholder <- function(icon = "\U0001f4c8", text) {
  shiny::div(class = "plot-placeholder",
             shiny::div(class = "icon", icon),
             text)
}

result_badge <- function(label, value, tone = c("default", "green", "orange")) {
  tone <- match.arg(tone)
  value_class <- switch(tone,
                        default = "result-value",
                        green   = "result-value green",
                        orange  = "result-value orange")
  shiny::div(class = "result-badge",
             shiny::div(class = "result-label", label),
             shiny::div(class = value_class, value))
}

ts_filename <- function(prefix, ext) {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
}

# Inline colour picker: a native HTML5 <input type="color"> wired up via
# Shiny.setInputValue() to a normal Shiny text input that holds the hex
# string. The text input is visible so users can read or paste hex codes.
# text_width controls the width of the hex-value box (e.g. "70px" for
# tight layouts, "90px" for the default).
colour_picker_inline <- function(text_id, default_hex, text_width = "90px") {
  picker_id <- paste0(text_id, "_picker")
  shiny::div(style = "display:flex;align-items:center;gap:0.4rem;",
    shiny::tags$input(
      id    = picker_id,
      type  = "color",
      value = default_hex,
      style = paste("width:2.2rem;height:2.2rem;border:1px solid #1E2D45;",
                    "border-radius:6px;background:transparent;cursor:pointer;",
                    "padding:0.1rem;flex-shrink:0;"),
      onchange = sprintf("Shiny.setInputValue('%s', this.value);", text_id),
      oninput  = sprintf("Shiny.setInputValue('%s', this.value);", text_id)),
    shiny::textInput(text_id, NULL, value = default_hex, width = text_width)
  )
}

# ---- Helper-load diagnostics ------------------------------------------------
#
# Every analytic helper file (softmax_bca_improved.R, tm_analysis_functions.R,
# plot_akta_improved.R) defines functions that the corresponding module needs.
# Two functions below handle the "is the helper actually loaded?" problem in a
# robust, self-debugging way:
#
#   ensure_helper_loaded()    - tries to make the helper's functions available
#   missing_helper_warning()  - renders a detailed diagnostic if it can't
#
# Both do their work at UI render time, so they don't depend on whether
# global.R succeeded earlier. If a package was missing, a path was wrong, or
# anything else went sideways, the warning card will say so explicitly.

# Candidate locations to look for an analytic helper file
.helper_candidate_paths <- function(filename) {
  app_dir_guess <- if (exists("app_dir", envir = globalenv())) {
    get("app_dir", envir = globalenv())
  } else getwd()

  unique(c(
    file.path(app_dir_guess, "inst", "analytics", filename),
    file.path(getwd(),       "inst", "analytics", filename),
    file.path("inst", "analytics", filename)
  ))
}

#' Try to make sure the analytic helper is loaded.
#'
#' Idempotent: if the functions are already in globalenv() it does nothing.
#' Otherwise it walks the candidate paths and attempts to source the first
#' one it finds.
#'
#' @return TRUE if every expected function is defined afterwards.
ensure_helper_loaded <- function(filename, expected_fns) {
  all_loaded <- function() {
    all(vapply(expected_fns, exists, logical(1),
               envir = globalenv(), mode = "function", inherits = TRUE))
  }
  if (all_loaded()) return(TRUE)

  for (p in .helper_candidate_paths(filename)) {
    if (file.exists(p)) {
      tryCatch(source(p, local = FALSE), error = function(e) NULL)
      if (all_loaded()) return(TRUE)
    }
  }
  FALSE
}

#' Render a self-diagnosing "helper not loaded" card.
#'
#' Looks at the live state of the R session AT RENDER TIME:
#'   1. Which tidyverse packages are missing?
#'   2. Where is the helper file (which candidate paths exist?)
#'   3. If found, can we source it?
#'   4. After sourcing, are the expected functions defined?
#'
#' The card shows whichever of these is the actual blocker. No reliance on
#' global state set up during global.R execution.
missing_helper_warning <- function(filename, expected_fns = character()) {

  # 1. Package availability
  helper_pkgs <- c("readr", "stringr", "dplyr", "tidyr", "purrr", "tibble",
                   "ggplot2", "scales", "gridExtra")
  missing_pkgs <- helper_pkgs[!vapply(helper_pkgs, requireNamespace, logical(1),
                                      quietly = TRUE)]

  # 2. File location
  candidates  <- .helper_candidate_paths(filename)
  existing    <- candidates[vapply(candidates, file.exists, logical(1))]
  found_at    <- if (length(existing)) existing[1] else NULL

  # 3. Try to source (only worth attempting if packages are present)
  source_err  <- NULL
  source_ok   <- FALSE
  if (!is.null(found_at) && length(missing_pkgs) == 0) {
    source_ok <- tryCatch({
      source(found_at, local = FALSE)
      TRUE
    }, error = function(e) {
      source_err <<- conditionMessage(e); FALSE
    })
  }

  # 4. Final function-availability check
  fn_present <- if (length(expected_fns)) {
    vapply(expected_fns, exists, logical(1),
           envir = globalenv(), mode = "function", inherits = TRUE)
  } else logical(0)

  # ---- Success path: file loaded just now -------------------------------
  if (length(expected_fns) > 0 && all(fn_present)) {
    return(shiny::div(class = "lab-card",
      style = "border-color: #00E5A0; margin: 2rem; max-width: 920px;",
      shiny::h4(paste0("\u2713 ", filename, " is loaded"),
                style = "color:#00E5A0; font-family:'Syne', sans-serif;"),
      shiny::p(paste0("Loaded from: ", found_at %||% "(already in memory)"),
               style = "color:#7A8FAD; font-family:monospace; font-size:0.82rem;"),
      shiny::p("Please refresh this page (F5 or Ctrl-R) to use the tool.",
               style = "color:#7A8FAD;")
    ))
  }

  # ---- Failure path: render specific diagnostic --------------------------
  lines <- character()

  if (length(missing_pkgs) > 0) {
    lines <- c(lines,
      "MISSING R PACKAGES",
      "",
      paste0("These packages are required by the helper but not installed:"),
      paste0("  ", paste(missing_pkgs, collapse = ", ")),
      "",
      "Run this in your R console, then restart the app:",
      "",
      paste0("  install.packages(c(",
             paste(paste0("\"", missing_pkgs, "\""), collapse = ", "),
             "))")
    )
  } else if (is.null(found_at)) {
    lines <- c(lines,
      paste0("File not found: ", filename),
      paste0("Working directory: ", getwd()),
      paste0("Resolved app dir:  ",
             if (exists("app_dir", envir = globalenv()))
               get("app_dir", envir = globalenv()) else "(not set)"),
      "",
      "Paths checked:"
    )
    for (p in candidates) {
      marker <- if (file.exists(p)) "  [FOUND]   " else "  [missing] "
      lines <- c(lines, paste0(marker, p))
    }
    lines <- c(lines, "",
      "Verify that inst/analytics/ contains this file in the same folder",
      "as app.R and global.R.")
  } else if (!is.null(source_err)) {
    lines <- c(lines,
      paste0("Found at: ", found_at),
      "",
      "But source() failed with this error:",
      "",
      paste0("  ", source_err),
      "",
      "If the message mentions 'no package called X':",
      "  install.packages(\"X\") in R, then restart the app."
    )
  } else {
    # Sourced OK but some expected functions still aren't in globalenv()
    missing_fns <- expected_fns[!fn_present]
    lines <- c(lines,
      paste0("Found and sourced: ", found_at),
      "",
      "But these functions are still not defined globally:",
      paste0("  ", paste(missing_fns, collapse = ", ")),
      "",
      "This usually means the helper file defines its functions inside",
      "a local() {} block or other non-global environment."
    )
  }

  shiny::div(class = "lab-card",
    style = "border-color: #FF5C5C; margin: 2rem; max-width: 960px;",
    shiny::h4(paste0("\u26a0\ufe0f Helper not loaded: ", filename),
              style = "color:#FF5C5C; font-family:'Syne', sans-serif;
                       margin-bottom: 1rem;"),
    shiny::tags$pre(
      paste(lines, collapse = "\n"),
      style = "color:#E8F0FE; background:#0A0F18;
               border:1px solid #1E2D45; padding:1rem;
               border-radius:6px; font-size:0.82rem;
               white-space:pre-wrap; word-break:break-word;
               font-family:'JetBrains Mono', monospace; line-height:1.6;"),
    shiny::tags$p(
      style = "color:#7A8FAD; font-size:0.82rem; margin-top:1rem;",
      "After fixing the issue, close this browser tab and click ",
      shiny::tags$b("Run App"), " again in RStudio.")
  )
}

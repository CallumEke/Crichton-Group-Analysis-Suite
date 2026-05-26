################################################################################
#  global.R  --  Package loading + module sourcing
################################################################################
#
#  Shiny sources this once at startup. Anything used by multiple modules
#  (packages, analytic helpers, theme utilities) loads here.
#
#  If the analytic helpers fail to load here, the per-tool UIs in R/mod_*.R
#  will try again at UI build time AND render a self-diagnosing card showing
#  exactly what went wrong (missing package, missing file, source error...).
#  See utils_theme.R: missing_helper_warning() / ensure_helper_loaded().
#
################################################################################

# -- Packages -----------------------------------------------------------------
# This list MUST be a superset of every library() call in any file we source,
# including the analytic helpers in inst/analytics/.
required <- c(
  # Shiny stack
  "shiny", "bslib", "shinyjs",
  # Plotting & I/O
  "ggplot2", "scales", "gridExtra",
  # Tidyverse pieces used by softmax_bca_improved.R + tm_analysis_functions.R
  "readr", "dplyr", "tidyr", "stringr", "purrr", "tibble",
  # Base utilities
  "tools",
  # Export bundle + Excel + image handling
  "zip", "openxlsx", "magick"
)

# Set a CRAN mirror so install.packages() doesn't try to prompt interactively
# (it can't, when run via the Run App button, and would fail silently).
if (is.null(getOption("repos")) ||
    !nzchar(getOption("repos")[["CRAN"]] %||% "") ||
    identical(getOption("repos")[["CRAN"]], "@CRAN@")) {
  options(repos = c(CRAN = "https://cran.r-project.org"))
}
`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

missing_pkgs <- required[!vapply(required, requireNamespace, logical(1),
                                 quietly = TRUE)]
if (length(missing_pkgs)) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  tryCatch(install.packages(missing_pkgs),
           error = function(e)
             message("install.packages() failed: ", conditionMessage(e),
                     "\nThe app will still run but tools requiring missing ",
                     "packages will show diagnostic cards."))
}

library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(scales)
library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(magick)

# -- Shiny upload size limit --------------------------------------------------
<<<<<<< HEAD
# Default is 5 MB; raise to 20 MB so larger UNICORN exports (long runs,
# fine-grained datapoints) fit comfortably AND multi-file overlays don't
# bump up against the limit. This is the upper bound for the TOTAL
# upload payload - when AKTA users select multiple files at once for
# overlay plots, the combined size of all selected files must fit
# under this limit, not each file individually.
#
# The same option is also set inside the server function in app.R as a
# belt-and-braces measure - some Shiny hosting environments don't pick
# up options() set here in global.R.
options(shiny.maxRequestSize = 20 * 1024^2)
=======
# Default is 5 MB; raise to 10 MB so larger UNICORN exports (long runs,
# fine-grained datapoints) fit. This is the upper bound for the TOTAL
# upload payload - when AKTA users select multiple files at once for
# overlay plots, the combined size of all selected files must fit
# under this limit, not each file individually.
options(shiny.maxRequestSize = 10 * 1024^2)
>>>>>>> 40d215cebc00d1a662419b518ddfa2f02a025593

# -- App directory ------------------------------------------------------------
# RStudio's Run App button sets wd to the app folder; this guards against
# edge cases where global.R is sourced from elsewhere.
app_dir <- tryCatch({
  if (file.exists("global.R")) getwd()
  else if (file.exists(file.path(getwd(), "global.R"))) getwd()
  else dirname(sys.frame(1)$ofile)
}, error = function(e) getwd())

# -- Shared utilities ---------------------------------------------------------
source(file.path(app_dir, "R", "utils_theme.R"))

# -- Analytic helpers ---------------------------------------------------------
# Sourced into globalenv() so functions like read_softmax_bca() are visible
# everywhere. If any of these fails to source (e.g. missing package), the
# per-tool UI in R/mod_*.R will retry the load and render a diagnostic card.
analytics_dir <- file.path(app_dir, "inst", "analytics")

for (f in c("softmax_bca_improved.R",
            "tm_analysis_functions.R",
            "plot_akta_improved.R")) {
  fp <- file.path(analytics_dir, f)
  if (!file.exists(fp)) {
    message("[helper] NOT FOUND: ", fp)
    next
  }
  ok <- tryCatch({
    source(fp, local = FALSE); TRUE
  }, error = function(e) {
    message("[helper] FAILED to source ", fp, ":\n  ", conditionMessage(e))
    FALSE
  })
  if (ok) message("[helper] OK: ", fp)
}

# -- Modules ------------------------------------------------------------------
for (m in c("mod_home", "mod_bca", "mod_cpm", "mod_akta", "mod_gel",
            "mod_cpm_qc", "mod_cpm_contour", "mod_ucp1")) {
  source(file.path(app_dir, "R", paste0(m, ".R")))
}

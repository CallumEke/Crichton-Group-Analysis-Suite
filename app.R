################################################################################
#              CRICHTON GROUP ANALYSIS SUITE  --  app.R  v3.0
################################################################################
#
#  Tools:
#    *  BCA Protein Quantification  (SoftMax Pro exports)
#    *  CPM Thermostability Tm      (RotorGene Q exports)
#    *  AKTA Chromatography         (UNICORN 7 exports)
#
#  New in v3:
#    - CPM Batch Analysis (all samples in one click)
#    - Delta-Tm Calculator (automatic -ligand vs +ligand pairing)
#    - Results History (session log per tool)
#    - CPM FWHM reported in results
#    - AKTA Peak Integration (click-defined regions, area + % purity)
#    - Drag-and-drop file upload styling
#    - Custom sample name editor
#    - Export Report (zip bundle of all results)
#    - Settings panel (algorithm parameters)
#    - Improved file format tolerance
#
################################################################################

# -- 0. Auto-install required packages ----------------------------------------
# CRITICAL: readr, dplyr, tidyr are REQUIRED for BCA tool file processing
# Do NOT remove these packages - softmax_bca_improved.R depends on them!
required <- c("shiny", "bslib", "shinyjs", "ggplot2", "scales",
              "DT", "readr", "dplyr", "tidyr", "tools", "zip", "gridExtra", "openxlsx", "magick")

for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}

library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(scales)
library(DT)
library(readr)   # Required by BCA tool (softmax_bca_improved.R)
library(dplyr)   # Required by BCA tool (softmax_bca_improved.R)
library(tidyr)   # Required by BCA tool (softmax_bca_improved.R)
library(gridExtra)
library(magick)  # For gel annotator - TIFF support

# -- 1. Source analysis functions ----------------------------------------------
app_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

bca_functions_ok  <- file.exists(file.path(app_dir, "softmax_bca_improved.R"))
cpm_functions_ok  <- file.exists(file.path(app_dir, "tm_analysis_functions.R"))
akta_functions_ok <- file.exists(file.path(app_dir, "plot_akta_improved.R"))

if (bca_functions_ok)  source(file.path(app_dir, "softmax_bca_improved.R"))
if (cpm_functions_ok)  source(file.path(app_dir, "tm_analysis_functions.R"))
if (akta_functions_ok) source(file.path(app_dir, "plot_akta_improved.R"))

# -- 2. Custom CSS -------------------------------------------------------------
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Syne:wght@400;600;700;800&family=DM+Sans:wght@300;400;500&family=JetBrains+Mono:wght@400;500&display=swap');

  :root {
    --bg-deep:      #080C14;
    --bg-card:      #0F1623;
    --bg-card2:     #111927;
    --border:       #1E2D45;
    --border-light: #243652;
    --accent:       #00C2FF;
    --accent-green: #00E5A0;
    --accent-warm:  #FF7B47;
    --accent-purple:#A78BFA;
    --danger:       #FF5C5C;
    --txt:          #E8F0FE;
    --muted:        #7A8FAD;
    --font-head:    'Syne', sans-serif;
    --font-body:    'DM Sans', sans-serif;
    --font-mono:    'JetBrains Mono', monospace;
  }

  body, .navbar { background: var(--bg-deep) !important; }
  * { box-sizing: border-box; }

  /* ---- Navbar ---- */
  .navbar {
    border-bottom: 1px solid var(--border);
    padding: 0 1.5rem;
  }
  .navbar-brand {
    font-family: var(--font-head);
    font-size: 1rem;
    font-weight: 700;
    letter-spacing: -0.01em;
    color: var(--txt) !important;
  }
  .navbar-brand span { color: var(--accent); }
  .nav-link { font-family: var(--font-body); font-size: 0.85rem !important; }

  /* ---- Cards ---- */
  .lab-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 1.1rem 1.2rem;
    margin-bottom: 0.9rem;
  }
  .lab-card-title {
    font-family: var(--font-head);
    font-size: 0.72rem;
    font-weight: 700;
    letter-spacing: 0.1em;
    text-transform: uppercase;
    color: var(--muted);
    margin-bottom: 0.9rem;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  .lab-card-title::after {
    content: '';
    flex: 1;
    height: 1px;
    background: var(--border);
  }
  
  /* NUCLEAR spacing fix - negative margins to pull sections together */
  #ucp1_content_section > .row {
    margin-bottom: 0 !important;
    margin-top: 0 !important;
  }
  #ucp1_content_section > .row + .row {
    margin-top: 0 !important;  /* Reset - no negative margin */
  }
  /* Minimal card margins */
  #ucp1_content_section .lab-card {
    margin-bottom: 0.2rem !important;
  }
  /* Zero all padding */
  #ucp1_content_section > .row > [class*='col-'] {
    padding-top: 0 !important;
    padding-bottom: 0 !important;
  }
  /* Hide br tags */
  #ucp1_content_section .lab-card > br {
    display: none !important;
  }
  /* Reduce spacing around plots */
  #ucp1_content_section .shiny-plot-output {
    margin-bottom: 0 !important;
    margin-top: 0 !important;
  }
  /* Ultra-compact plot cards */
  #ucp1_content_section .lab-card {
    padding: 0.6rem !important;
  }
  /* Reduce spacing in plot download areas */
  #ucp1_content_section .lab-card-title {
    margin-bottom: 0.5rem !important;
  }
  /* Reduce spacing between cards in plot columns */
  #ucp1_content_section > .row > [class*='col-8'] .lab-card + .lab-card {
    margin-top: -0.5rem !important;
  }
  /* Reduce padding in calibration curve outputs */
  #ucp1_calibcurve_ui, #ucp1_cap_calibcurve_ui {
    margin-top: -1rem !important;
    margin-bottom: 0 !important;
  }
  /* Reduce spacing in calibration curve containers */
  #ucp1_content_section .shiny-html-output {
    margin-top: 0 !important;
    margin-bottom: 0 !important;
  }

  /* ---- Step numbers ---- */
  .step-number {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 1.4rem;
    height: 1.4rem;
    border-radius: 50%;
    background: var(--accent);
    color: var(--bg-deep);
    font-family: var(--font-head);
    font-size: 0.72rem;
    font-weight: 800;
    flex-shrink: 0;
  }

  /* ---- Buttons ---- */
  .btn-run {
    width: 100%;
    padding: 0.7rem 1rem;
    background: linear-gradient(135deg, var(--accent) 0%, #0099CC 100%);
    color: var(--bg-deep);
    border: none;
    border-radius: 8px;
    font-family: var(--font-head);
    font-size: 0.8rem;
    font-weight: 700;
    letter-spacing: 0.05em;
    cursor: pointer;
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    box-shadow: 0 4px 15px rgba(0,194,255,0.25);
  }
  .btn-run:hover { transform: translateY(-1px); box-shadow: 0 6px 20px rgba(0,194,255,0.35); }
  .btn-run:active { transform: translateY(0); }
  .btn-run:disabled { opacity: 0.4; cursor: not-allowed; transform: none; box-shadow: none; }

  .btn-batch {
    width: 100%;
    padding: 0.7rem 1rem;
    background: linear-gradient(135deg, var(--accent-green) 0%, #00B87A 100%);
    color: var(--bg-deep);
    border: none;
    border-radius: 8px;
    font-family: var(--font-head);
    font-size: 0.8rem;
    font-weight: 700;
    letter-spacing: 0.05em;
    cursor: pointer;
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    box-shadow: 0 4px 15px rgba(0,229,160,0.25);
  }
  .btn-batch:hover { transform: translateY(-1px); box-shadow: 0 6px 20px rgba(0,229,160,0.35); }

  .btn-download {
    display: inline-flex;
    align-items: center;
    gap: 0.3rem;
    padding: 0.4rem 0.85rem;
    background: transparent;
    border: 1px solid var(--border-light);
    border-radius: 6px;
    color: var(--muted);
    font-family: var(--font-mono);
    font-size: 0.72rem;
    cursor: pointer;
    transition: all 0.15s cubic-bezier(0.4, 0, 0.2, 1);
    margin-top: 0.4rem;
    text-decoration: none;
  }
  .btn-download:hover { border-color: var(--accent); color: var(--accent); background: rgba(0,194,255,0.05); }

  .dl-row { display: flex; flex-wrap: wrap; gap: 0.4rem; margin-top: 0.5rem; }

  /* ---- Clear data button ---- */
  .btn-clear {
    background: linear-gradient(135deg, #EF4444 0%, #DC2626 100%);
    color: white;
    border: none;
    padding: 0.6rem 1.2rem;
    border-radius: 8px;
    font-weight: 600;
    font-size: 0.9rem;
    cursor: pointer;
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
    box-shadow: 0 2px 4px rgba(239, 68, 68, 0.3);
    display: inline-flex;
    align-items: center;
    gap: 0.5rem;
  }
  .btn-clear:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 12px rgba(239, 68, 68, 0.5);
    filter: brightness(1.1);
  }
  .btn-clear:active {
    transform: translateY(0px);
  }
  
  .clear-button-container {
    margin-bottom: 1.5rem;
    padding: 1rem;
    background: rgba(239, 68, 68, 0.05);
    border: 1px solid rgba(239, 68, 68, 0.2);
    border-radius: 8px;
    text-align: right;
  }
  
  /* ---- Clearing overlay ---- */
  .clearing-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(8, 12, 20, 0.95);
    z-index: 9999;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-direction: column;
  }
  .clearing-spinner {
    width: 60px;
    height: 60px;
    border: 4px solid rgba(0, 194, 255, 0.2);
    border-top-color: #00C2FF;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }
  @keyframes spin {
    to { transform: rotate(360deg); }
  }
  .clearing-text {
    margin-top: 1.5rem;
    color: #00C2FF;
    font-size: 1.2rem;
    font-weight: 600;
    font-family: var(--font-title);
  }

  /* ---- Plot placeholder ---- */
  .plot-placeholder {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    min-height: 200px;
    color: var(--muted);
    font-family: var(--font-body);
    font-size: 0.85rem;
  }
  .plot-placeholder .icon { font-size: 2.5rem; margin-bottom: 0.5rem; opacity: 0.3; }

  /* ---- Result badges ---- */
  .result-badge {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 0.9rem 1rem;
    margin-bottom: 0.9rem;
    text-align: center;
  }
  .result-label {
    font-family: var(--font-head);
    font-size: 0.65rem;
    font-weight: 700;
    letter-spacing: 0.1em;
    text-transform: uppercase;
    color: var(--muted);
    margin-bottom: 0.35rem;
  }
  .result-value {
    font-family: var(--font-mono);
    font-size: 1.6rem;
    font-weight: 500;
    color: var(--accent);
  }
  .result-value.green  { color: var(--accent-green); }
  .result-value.orange { color: var(--accent-warm); }
  .result-value.purple { color: var(--accent-purple); }

  /* ---- Status pills ---- */
  .status-pill {
    display: inline-flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.25rem 0.65rem;
    border-radius: 999px;
    font-family: var(--font-mono);
    font-size: 0.7rem;
    margin-top: 0.35rem;
  }
  .status-pill.ready    { background: rgba(0,229,160,0.1);  color: var(--accent-green); }
  .status-pill.waiting  { background: rgba(0,194,255,0.08); color: var(--accent); }
  .status-pill.error    { background: rgba(255,92,92,0.1);  color: var(--danger); }
  
  /* Fix upload status display */
  .shiny-file-input-progress {
    min-height: 2rem !important;
    margin-top: 0.5rem !important;
  }
  
  .status-pill {
    display: inline-flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.5rem 1rem;
    border-radius: 20px;
    font-size: 0.9em;
    font-weight: 500;
    margin: 0.25rem 0.5rem 0.25rem 0;
    min-height: 2rem;
    white-space: normal;
    word-break: break-word;
    line-height: 1.2;
  }
  .status-pill .dot {
    width: 6px; height: 6px; border-radius: 50%;
    background: currentColor;
    animation: pulse-dot 1.8s ease-in-out infinite;
  }
  .status-pill.ready .dot, .status-pill.error .dot { animation: none; }
  @keyframes pulse-dot { 0%,100%{opacity:1} 50%{opacity:0.3} }

  /* ---- Mode pills (radio) ---- */
  .mode-pills .form-check { display: inline-block; margin: 0; padding: 0; }
  .mode-pills { display: flex; gap: 0.4rem; flex-wrap: wrap; }
  .mode-pills input[type=radio] { display: none; }
  .mode-pills label {
    display: inline-block;
    padding: 0.3rem 0.75rem;
    border: 1px solid var(--border-light);
    border-radius: 6px;
    font-family: var(--font-body);
    font-size: 0.78rem;
    color: var(--muted);
    cursor: pointer;
    transition: all 0.15s cubic-bezier(0.4, 0, 0.2, 1);
    white-space: nowrap;
  }
  .mode-pills label:hover { border-color: var(--accent); color: var(--txt); }
  .mode-pills input[type=radio]:checked + label {
    border-color: var(--accent);
    background: rgba(0,194,255,0.1);
    color: var(--accent);
    font-weight: 500;
  }

  /* ---- Info box ---- */
  .info-box {
    background: rgba(0,194,255,0.05);
    border: 1px solid rgba(0,194,255,0.15);
    border-radius: 7px;
    padding: 0.55rem 0.75rem;
    font-size: 0.78rem;
    color: var(--muted);
    line-height: 1.5;
    margin-bottom: 0.75rem;
    display: flex;
    align-items: flex-start;
    gap: 0.4rem;
  }
  .info-box .info-icon { color: var(--accent); margin-right: 0.35rem; flex-shrink: 0; }

  /* ---- Warning box ---- */
  .warn-box {
    background: rgba(255,123,71,0.07);
    border: 1px solid rgba(255,123,71,0.25);
    border-radius: 7px;
    padding: 0.55rem 0.75rem;
    font-size: 0.78rem;
    color: #FFB085;
    line-height: 1.5;
    margin-bottom: 0.75rem;
  }

  /* ---- Home hero ---- */
  .home-hero {
    text-align: center;
    padding: 3rem 1rem 2rem;
  }
  .home-hero h1 {
    font-family: var(--font-head);
    font-size: 2.2rem;
    font-weight: 800;
    background: linear-gradient(135deg, var(--txt) 30%, var(--accent) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    margin-bottom: 0.6rem;
  }
  .home-hero p { color: var(--muted); font-size: 0.92rem; max-width: 620px; margin: 0 auto; }

  /* ---- Tool cards (home) ---- */
  .tool-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 12px;
    padding: 1.5rem;
    cursor: pointer;
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    position: relative;
    overflow: hidden;
    margin-bottom: 1rem;
    --accent-col: var(--accent);
  }
  .tool-card::before {
    content: '';
    position: absolute;
    top: 0; left: 0; right: 0;
    height: 3px;
    background: var(--accent-col);
    transform: scaleX(0);
    transform-origin: left;
    transition: transform 0.25s ease;
  }
  .tool-card.green::before { background: var(--accent-green); }
  .tool-card:hover { border-color: var(--border-light); transform: translateY(-2px); box-shadow: 0 12px 40px rgba(0,0,0,0.4); }
  .tool-card:hover::before { transform: scaleX(1); }
  .tool-icon { font-size: 2rem; display: block; margin-bottom: 0.6rem; color: var(--accent-col, var(--accent)); }
  .tool-icon svg { width: 2rem; height: 2rem; }
  .tool-title {
    font-family: var(--font-head);
    font-size: 1rem;
    font-weight: 700;
    color: var(--txt);
    margin-bottom: 0.4rem;
  }
  .tool-desc { font-size: 0.8rem; color: var(--muted); line-height: 1.5; margin-bottom: 0.75rem; }
  .tool-tag {
    display: inline-block;
    padding: 0.15rem 0.5rem;
    background: rgba(255,255,255,0.04);
    border: 1px solid var(--border);
    border-radius: 4px;
    font-family: var(--font-mono);
    font-size: 0.65rem;
    color: var(--muted);
    margin-right: 0.3rem;
  }

  /* ---- Inputs ---- */
  .form-control, .form-select, .selectize-input {
    background: var(--bg-deep) !important;
    border: 1px solid var(--border) !important;
    color: var(--txt) !important;
    border-radius: 6px !important;
    font-family: var(--font-body) !important;
    font-size: 0.83rem !important;
  }
  .form-control:focus { border-color: var(--accent) !important; box-shadow: 0 0 0 2px rgba(0,194,255,0.15) !important; }
  label { color: var(--muted) !important; font-size: 0.78rem !important; }

  /* ---- Drag-drop file upload ---- */
  .shiny-input-container .input-group {
    border: 2px dashed var(--border-light);
    border-radius: 8px;
    padding: 1.2rem;
    background: rgba(0,194,255,0.02);
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    text-align: center;
  }
  .shiny-input-container .input-group:hover,
  .shiny-input-container .input-group.drag-over {
    border-color: var(--accent);
    background: rgba(0,194,255,0.06);
  }
  .shiny-input-container .input-group .btn {
    background: var(--accent);
    color: var(--bg-deep);
    border: none;
    font-family: var(--font-head);
    font-size: 0.78rem;
    font-weight: 700;
    border-radius: 5px;
    padding: 0.35rem 0.8rem;
  }
  .shiny-input-container .form-control[type=text] {
    background: transparent !important;
    border: none !important;
    font-size: 0.75rem !important;
    color: var(--muted) !important;
  }

  /* ---- History / session log ---- */
  .history-row {
    display: grid;
    gap: 0.3rem;
    padding: 0.55rem 0;
    border-bottom: 1px solid var(--border);
    font-size: 0.79rem;
  }
  .history-row:last-child { border-bottom: none; }
  .history-time {
    font-family: var(--font-mono);
    font-size: 0.68rem;
    color: var(--muted);
  }
  .history-val {
    font-family: var(--font-mono);
    color: var(--accent);
    font-weight: 500;
  }

  /* ---- Settings panel ---- */
  .settings-group {
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 0.8rem 1rem;
    margin-bottom: 0.6rem;
  }
  .settings-group-title {
    font-family: var(--font-head);
    font-size: 0.68rem;
    font-weight: 700;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    color: var(--accent);
    margin-bottom: 0.65rem;
  }

  /* ---- dTm bar chart ---- */
  .dtm-positive { fill: var(--accent-green) !important; }
  .dtm-negative { fill: var(--danger) !important; }

  /* ---- QC mode toggle ---- */
  .qc-mode-toggle {
    display: inline-flex;
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    border-radius: 8px;
    padding: 3px;
    gap: 3px;
    margin-bottom: 1rem;
  }
  .qc-mode-btn {
    padding: 0.35rem 1.1rem;
    border: none;
    border-radius: 6px;
    font-family: var(--font-head);
    font-size: 0.78rem;
    font-weight: 700;
    letter-spacing: 0.03em;
    cursor: pointer;
    transition: all 0.15s ease;
    background: transparent;
    color: var(--muted);
  }
  .qc-mode-btn.active {
    background: var(--accent);
    color: var(--bg-deep);
    box-shadow: 0 2px 8px rgba(0,194,255,0.3);
  }
  .qc-mode-btn:not(.active):hover { color: var(--txt); }
  .integration-hint {
    font-family: var(--font-mono);
    font-size: 0.72rem;
    color: var(--accent-green);
    padding: 0.3rem 0;
  }

  /* ---- Tables ---- */
  .dataTable { background: var(--bg-card) !important; color: var(--txt) !important; font-family: var(--font-body) !important; }
  .dataTables_wrapper { color: var(--muted) !important; }
  table.dataTable thead th { background: #161E2E !important; color: var(--muted) !important; border-bottom: 1px solid var(--border) !important; font-family: var(--font-head) !important; font-size: 0.7rem !important; letter-spacing: 0.06em !important; text-transform: uppercase !important; }
  table.dataTable tbody tr { background: var(--bg-card) !important; }
  table.dataTable tbody tr:hover td { background: rgba(0,194,255,0.04) !important; }
  table.dataTable tbody td { border-bottom: 1px solid var(--border) !important; color: var(--txt) !important; font-size: 0.82rem !important; }

  /* ---- Scrollbar ---- */
  ::-webkit-scrollbar { width: 5px; height: 5px; }
  ::-webkit-scrollbar-track { background: var(--bg-deep); }
  ::-webkit-scrollbar-thumb { background: var(--border-light); border-radius: 3px; }

  /* ---- lbl helper ---- */
  .lbl { font-size: 0.76rem; color: var(--muted); margin-bottom: 0.2rem; margin-top: 0.5rem; }

  /* ---- Collapsible advanced panel ---- */
  .adv-toggle {
    background: none;
    border: 1px solid var(--border);
    border-radius: 6px;
    color: var(--muted);
    font-family: var(--font-mono);
    font-size: 0.72rem;
    padding: 0.3rem 0.7rem;
    cursor: pointer;
    margin-bottom: 0.5rem;
    transition: all 0.15s;
    width: 100%;
    text-align: left;
  }
  .adv-toggle:hover { border-color: var(--accent); color: var(--accent); }

  /* ---- Progress bar ---- */
  .progress { height: 3px; background: var(--border); border-radius: 99px; margin-top: 0.4rem; }
  .progress-bar { background: var(--accent); border-radius: 99px; transition: width 0.3s ease; }
"

# -- JS for drag-and-drop visual feedback and settings toggles ----------------
custom_js <- "
$(document).on('dragover', '.shiny-input-container .input-group', function(e) {
  e.preventDefault(); $(this).addClass('drag-over');
});
$(document).on('dragleave drop', '.shiny-input-container .input-group', function(e) {
  $(this).removeClass('drag-over');
});
"

# -- 3. UI --------------------------------------------------------------------
ui <- page_navbar(
  title   = tags$span(tags$span("Crichton Group", style = "color: #00C2FF"), " Analysis Suite"),
  id      = "nav",
  theme   = bs_theme(
    version   = 5,
    bg        = "#080C14",
    fg        = "#E8F0FE",
    primary   = "#00C2FF",
    secondary = "#1E2D45",
    success   = "#00E5A0",
    font_scale = 0.92
  ),
  header  = tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML(custom_js)),
    useShinyjs()
  ),
  footer  = NULL,
  collapsible = TRUE,

  # -- HOME ------------------------------------------------------------------
  nav_panel(
    "Home", icon = icon("house"),

    div(class = "home-hero",
      h1("Crichton Group Analysis Suite"),
      p("Automated analysis tools for the Crichton Group. Upload your data, adjust parameters, and get publication-ready results — no coding required.")
    ),

    fluidRow(
      column(3,
        div(class = "tool-card", style = "--accent-col: #FF7B47;",
            onclick = "Shiny.setInputValue('nav', 'AKTA')",
          div(class = "tool-icon", icon("chart-line")),
          div(class = "tool-title", "ÄKTA Chromatography"),
          div(class = "tool-desc", "Plot UNICORN 7 SEC/IEX/HIC exports. Overlay multiple runs, highlight fractions, and integrate peaks."),
          tags$span(".csv", class = "tool-tag"), tags$span("UNICORN 7", class = "tool-tag"), tags$span("Peak Integration", class = "tool-tag")
        )
      ),
      column(3,
        div(class = "tool-card", onclick = "Shiny.setInputValue('nav', 'BCA')",
          div(class = "tool-icon", icon("flask")),
          div(class = "tool-title", "BCA Protein Quantification"),
          div(class = "tool-desc", "Analyse SoftMax Pro BCA assay exports. Fits a standard curve, calculates concentration, and reports total protein yield."),
          tags$span(".xls", class = "tool-tag"), tags$span("SoftMax Pro", class = "tool-tag"), tags$span("Standard Curve", class = "tool-tag")
        )
      ),
      column(3,
        div(class = "tool-card", style = "--accent-col: #00E5A0;",
            onclick = "Shiny.setInputValue('nav', 'CPMCONTOUR')",
          div(class = "tool-icon", icon("border-all")),
          div(class = "tool-title", "CPM Contour Plotting"),
          div(class = "tool-desc", "Average replicate dF/dT traces across conditions and visualise as a heatmap. Plot Tm vs. concentration on a log scale."),
          tags$span(".csv", class = "tool-tag"), tags$span("Prism Export", class = "tool-tag"), tags$span("Titre", class = "tool-tag")
        )
      ),
      column(3,
        div(class = "tool-card green", onclick = "Shiny.setInputValue('nav', 'CPM')",
          div(class = "tool-icon", icon("temperature-half")),
          div(class = "tool-title", "CPM Peak Picker"),
          div(class = "tool-desc", "Calculate Tm from RotorGene Q CPM exports via manual range or automatic peak detection."),
          tags$span(".csv", class = "tool-tag"), tags$span("RotorGene Q", class = "tool-tag"), tags$span("Tm", class = "tool-tag")
        )
      )
    ),
    fluidRow(
      column(3,
        div(class = "tool-card", style = "--accent-col: #A78BFA;",
            onclick = "Shiny.setInputValue('nav', 'CPMQC')",
          div(class = "tool-icon", icon("magnifying-glass-chart")),
          div(class = "tool-title", "CPM QC"),
          div(class = "tool-desc", "Compare two samples: raw fluorescence, dF/dT traces and Tm bar chart. Designed for -GDP vs +GDP QC runs."),
          tags$span(".csv", class = "tool-tag"), tags$span("RotorGene Q", class = "tool-tag"), tags$span("QC", class = "tool-tag")
        )
      ),
      column(3,
        div(class = "tool-card", style = "--accent-col: #8B5CF6;",
            onclick = "Shiny.setInputValue('nav', 'GEL')",
          div(class = "tool-icon", icon("image")),
          div(class = "tool-title", "Gel Annotator"),
          div(class = "tool-desc", "Label SDS-PAGE and Western blot gels with publication-ready formatting. Supports TIFF, PNG, and JPEG files."),
          tags$span(".tif", class = "tool-tag"), tags$span("Western Blot", class = "tool-tag"), tags$span("Publication", class = "tool-tag")
        )
      ),
      column(3,
        div(class = "tool-card", style = "--accent-col: #22C55E;",
            onclick = "Shiny.setInputValue('nav', 'UCP1')",
          div(class = "tool-icon", icon("vial")),
          div(class = "tool-title", "UCP1 Proton Conductance"),
          div(class = "tool-desc", "Automated pre-processing of fluorimetric proton conductance assay data. Upload raw intensity traces for calibration and analysis."),
          tags$span(".csv", class = "tool-tag"), tags$span("Fluorimetry", class = "tool-tag"), tags$span("UCP1", class = "tool-tag")
        )
      )
    )
  ),

  # -- BCA -------------------------------------------------------------------
  nav_panel(
    "ÄKTA Chromatography", icon = icon("chart-line"), value = "AKTA",

    if (!akta_functions_ok) {
      div(class = "lab-card", style = "border-color: #FF5C5C; margin: 2rem;",
        h4("⚠️ Missing: plot_akta_improved.R", style = "color:#FF5C5C;font-family:'Syne',sans-serif;"),
        p("Place plot_akta_improved.R in the same folder as app.R and restart.", style = "color:#7A8FAD;"))
    } else tagList(

    # Clear data button
    div(class = "clear-button-container",
      actionButton("akta_clear", "🔄  Clear All Data", class = "btn-clear")
    ),

    fluidRow(
      # Left controls
      column(4,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Data File(s)"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Upload one file for a single run, or multiple to overlay traces."),
          fileInput("akta_files", NULL, accept = ".csv", multiple = TRUE,
            buttonLabel = "Browse…", placeholder = "UNICORN 7 CSV export(s)"),
          uiOutput("akta_file_status")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Custom Sample Names"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Override filenames in the legend. One per line, same order as files."),
          textAreaInput("akta_custom_names", NULL, rows = 3,
            placeholder = "e.g.\nWT protein\nMutant R151A")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Volume Range"),
          fluidRow(
            column(6, numericInput("akta_vol_min", "From (mL)", value = NA, min = 0, step = 1)),
            column(6, numericInput("akta_vol_max", "To (mL)",   value = NA, min = 0, step = 1))
          )
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Display Options"),
          checkboxInput("akta_show_fractions", "Show fraction markers", value = TRUE),
          checkboxInput("akta_show_cond",      "Show conductance trace", value = FALSE),
          checkboxInput("akta_show_uv260",     "Show UV 260 nm trace",   value = FALSE),
          checkboxInput("akta_show_pctb",      "Show % Buffer B trace",  value = FALSE),
          br(),
          div(class = "lbl", "Highlight fractions (e.g. B8,B9,B10)"),
          textInput("akta_highlight", NULL, placeholder = "e.g. B8,B9,B10")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Peak Integration"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Enter volume boundaries to integrate the UV trace. Reports area, purity, centroid elution volume, and estimated MW."),
          fluidRow(
            column(6, numericInput("akta_int_start", "From (mL)", value = NA, min = 0, step = 0.1)),
            column(6, numericInput("akta_int_end",   "To (mL)",   value = NA, min = 0, step = 0.1))
          ),
          tags$button("\u2699  Column Calibration", class = "adv-toggle",
            onclick = "$('#akta_calib_panel').slideToggle(200)"),
          div(id = "akta_calib_panel", style = "display:none;",
            div(class = "settings-group",
              div(class = "settings-group-title", "SEC Column Calibration"),
              div(style = "font-size:0.75rem;color:var(--muted);margin-bottom:0.5rem;",
                "Defaults calibrated for your column. Update if using a different column."),
              fluidRow(
                column(6, numericInput("akta_void_vol",  "Void vol. (mL)",  value = 8.23,  min = 0, step = 0.01)),
                column(6, numericInput("akta_total_vol", "Total vol. (mL)", value = 24.00, min = 0, step = 0.01))
              ),
              div(style = "font-size:0.72rem;color:var(--muted);",
                "MW = 10^(\u22123.2245 \u00d7 (Ve\u2212Void)/(Total\u2212Void) + 5.9275)")
            )
          ),
          br(),
          uiOutput("akta_integration_result"),
          br(),
          uiOutput("akta_integration_actions")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "6"), "Plot Style"),
          selectInput("akta_theme", "Theme",
            choices = c("Publication" = "publication", "Presentation" = "presentation", "Minimal" = "minimal"),
            selected = "publication"),
          numericInput("akta_linewidth", "Line width", value = 1.2, min = 0.5, max = 4, step = 0.2)
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "7"), "Plot"),
          actionButton("akta_run", "▶  Generate Plot", class = "btn-run"), br(), br(),
          uiOutput("akta_download_buttons")
        )
      ),

      # Right panel
      column(8,
        div(class = "lab-card",
          div(class = "lab-card-title", "📈  Chromatography Profile"),
          uiOutput("akta_plot_placeholder"),
          plotOutput("akta_plot", height = "500px")
        ),
        uiOutput("akta_file_list_ui"),

        # History
        div(class = "lab-card",
          div(class = "lab-card-title", "🕑  Session History"),
          uiOutput("akta_history_ui")
        )
      )
    )
    )  # Close tagList
  ),

  # -- GEL ANNOTATOR ---------------------------------------------------------
  nav_panel(
    "BCA", icon = icon("flask"), value = "BCA",

    if (!bca_functions_ok) {
      div(class = "lab-card", style = "border-color: #FF5C5C; margin: 2rem;",
        h4("⚠️ Missing: softmax_bca_improved.R", style = "color:#FF5C5C;"),
        p("Place softmax_bca_improved.R in the same folder as app.R and restart.", style = "color:#7A8FAD;"))
    } else tagList(

    # Clear data button
    div(class = "clear-button-container",
      actionButton("bca_clear", "🔄  Clear All Data", class = "btn-clear")
    ),

    fluidRow(
      column(4,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Data File"),
          fileInput("bca_file", NULL, accept = c(".xls",".xlsx",".csv",".txt"),
            buttonLabel = "Browse…", placeholder = "SoftMax Pro export"),
          uiOutput("bca_file_status")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Concentration Source"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Use Manual if multiple samples share one plate."),
          div(class = "mode-pills",
            radioButtons("bca_mode", NULL,
              choices = c("Automatic (from file)" = "auto", "Manual entry" = "manual"),
              selected = "auto", inline = TRUE)
          ),
          conditionalPanel("input.bca_mode == 'manual'", br(),
            numericInput("bca_manual_conc", "Protein concentration (mg/mL)", value = NULL, min = 0, step = 0.01))
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Parameters"),
          numericInput("bca_volume", "Sample volume (mL)", value = 1.0, min = 0.001, step = 0.1),
          textInput("bca_title", "Result table title", value = "BCA Assay Protein Yield Summary"),
          numericInput("bca_digits", "Decimal places", value = 2, min = 1, max = 4)
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Analyse"),
          actionButton("bca_run", "▶  Run Analysis", class = "btn-run"), br(), br(),
          uiOutput("bca_download_buttons")
        )
      ),
      column(8,
        uiOutput("bca_result_badges"),
        div(class = "lab-card",
          div(class = "lab-card-title", "📈  Standard Curve"),
          uiOutput("bca_curve_placeholder"),
          plotOutput("bca_curve_plot", height = "360px")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "📊  Results"),
          DTOutput("bca_results_table")
        ),
        # History
        div(class = "lab-card",
          div(class = "lab-card-title", "🕑  Session History"),
          uiOutput("bca_history_ui"),
          uiOutput("bca_history_dl")
        )
      )
    )
    )  # Close tagList
  ),

  # -- CPM -------------------------------------------------------------------
  nav_panel(
    "CPM Contour Plotting", icon = icon("border-all"), value = "CPMCONTOUR",
    
    # Clear button
    div(class = "clear-button-container",
      actionButton("contour_clear", "🔄  Clear All Data", class = "btn-clear")
    ),
    
    fluidRow(
      # Left sidebar - File Uploads + Pairing
      column(3,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload dF/dT Data"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Upload CSV file exported from GraphPad Prism containing dF/dT data with replicate samples."),
          fileInput("contour_dfdt_file", NULL, accept = c(".csv"),
            buttonLabel = "Browse…", placeholder = "No dF/dT file selected"),
          uiOutput("contour_dfdt_status")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Upload Tm Data"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Upload CSV file containing Tm scatter data for the same samples."),
          fileInput("contour_tm_file", NULL, accept = c(".csv"),
            buttonLabel = "Browse…", placeholder = "No Tm file selected"),
          uiOutput("contour_tm_status")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Match Tm to Samples"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Match each Tm concentration to its corresponding dF/dT sample."),
          uiOutput("contour_tm_pairing_ui")
        )
      ),
      
      # Middle - Results & Visualization
      column(6,
        div(class = "lab-card",
          div(class = "lab-card-title", "📈  dF/dT Heatmap"),
          plotOutput("contour_heatmap", height = "450px")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "🌡️  Tm vs. Concentration"),
          plotOutput("contour_tm_plot", height = "340px")
        )
      ),
      
      # Right sidebar - Statistics + Process + Settings + Export
      column(3,
        div(class = "lab-card",
          div(class = "lab-card-title", "📊  Sample Statistics"),
          div(style = "max-height: 200px; overflow-y: auto; padding-right: 4px;",
            uiOutput("contour_stats_ui")
          )
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "🌡️  Tm Summary"),
          div(style = "max-height: 200px; overflow-y: auto; padding-right: 4px;",
            uiOutput("contour_tm_summary")
          )
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Process Data"),
          actionButton("contour_process", "🔬 Calculate Mean & SEM", class = "btn-run"),
          br(), br(),
          uiOutput("contour_sample_count")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "🎨  Plot Settings"),
          tags$label("dF/dT colour palette", style = "color:var(--muted);font-size:0.78rem;"),
          selectInput("contour_palette", NULL,
            choices = c(
              "Grayscale (white → black)"  = "grayscale",
              "Inferno (dark → yellow)"    = "inferno",
              "Magma (dark → white)"       = "magma",
              "Viridis (dark → yellow)"    = "viridis",
              "Plasma (dark → pink)"       = "plasma",
              "Blue → White → Red"         = "RdBu",
              "Yellow → Orange → Red"      = "YlOrRd"
            ),
            selected = "grayscale",
            width = "100%"
          ),
          br(),
          tags$label("Intensity threshold", style = "color:var(--muted);font-size:0.78rem;"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Values below this threshold are shown as background colour. Remaining values are rescaled to use the full palette."),
          sliderInput("contour_threshold", NULL,
            min = 0, max = 0.95, value = 0, step = 0.05,
            width = "100%"
          ),
          br(),
          tags$button("\u2705  Temperature Range", class = "adv-toggle",
            onclick = "$('#contour_yrange_panel').slideToggle(200)"),
          div(id = "contour_yrange_panel", style = "display:none;",
            div(class = "settings-group",
              div(class = "settings-group-title", "Tm Plot Y-Axis Range (\u00b0C)"),
              div(style = "font-size:0.75rem;color:var(--muted);margin-bottom:0.5rem;",
                "Full data range shown by default. Adjust to zoom in on a specific region."),
              fluidRow(
                column(6, numericInput("contour_ymin", "Min", value = 25.5, step = 1, width = "100%")),
                column(6, numericInput("contour_ymax", "Max", value = 89.5, step = 1, width = "100%"))
              )
            )
          )
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "📥  Export"),
          downloadButton("contour_export_stats",   "⬇ Statistics CSV",  class = "btn-download"),
          br(), br(),
          downloadButton("contour_export_heatmap", "⬇ Heatmap PNG",     class = "btn-download"),
          br(), br(),
          downloadButton("contour_export_tmplot",  "⬇ Tm Plot PNG",     class = "btn-download")
        )
      )
    )
  ),

  # -- UCP1 Proton Conductance -----------------------------------------------
  nav_panel(
    "CPM Peak Picker", icon = icon("temperature-half"), value = "CPM",

    if (!cpm_functions_ok) {
      div(class = "lab-card", style = "border-color: #FF5C5C; margin: 2rem;",
        h4("⚠️ Missing: tm_analysis_functions.R", style = "color:#FF5C5C;"),
        p("Place tm_analysis_functions.R in the same folder as app.R and restart.", style = "color:#7A8FAD;"))
    } else tagList(

    # Clear data button
    div(class = "clear-button-container",
      actionButton("cpm_clear", "🔄  Clear All Data", class = "btn-clear")
    ),

    fluidRow(
      # Left controls
      column(4,

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Data File"),
          fileInput("cpm_file", NULL, accept = ".csv",
            buttonLabel = "Browse…", placeholder = "RotorGene Q CSV export"),
          uiOutput("cpm_file_status")
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Select Sample"),
          uiOutput("cpm_sample_ui")
        ),


        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Analysis Mode"),
          div(class = "mode-pills",
            radioButtons("cpm_mode", NULL,
              choices = c("Manual range" = "manual", "Auto peak detect" = "auto"),
              selected = "manual", inline = TRUE)
          ), br(),
          conditionalPanel("input.cpm_mode == 'manual'",
            div(class = "info-box", tags$span("ℹ", class = "info-icon"),
              "Check the Preview tab, then enter the temperature range around your peak."),
            fluidRow(
              column(6, numericInput("cpm_tlow",  "Lower T (°C)", value = 45, step = 0.5)),
              column(6, numericInput("cpm_thigh", "Upper T (°C)", value = 65, step = 0.5))
            )
          ),
          conditionalPanel("input.cpm_mode == 'auto'",
            div(class = "info-box", tags$span("ℹ", class = "info-icon"),
              "Peaks outside the region of interest are ignored for analysis but still plotted."),
            fluidRow(
              column(6, numericInput("cpm_tmin", "Ignore below (°C)", value = 30, step = 1)),
              column(6, numericInput("cpm_tmax", "Ignore above (°C)", value = 80, step = 1))
            ),
            numericInput("cpm_prominence", "Min peak prominence (0-1)",
              value = 0.10, min = 0.01, max = 0.9, step = 0.01)
          )
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Custom Sample Name"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Override the auto-generated name for cleaner plots and exports."),
          textInput("cpm_custom_name", NULL, placeholder = "Leave blank to use original name")
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Analyse"),
          actionButton("cpm_run", "▶  Run Analysis", class = "btn-run"), br(), br(),
          uiOutput("cpm_download_buttons"),

          # Advanced settings
          br(),
          tags$button("⚙  Advanced Settings", class = "adv-toggle",
            onclick = "$('#cpm_adv').slideToggle(200)"),
          div(id = "cpm_adv", style = "display:none;",
            div(class = "settings-group",
              div(class = "settings-group-title", "Peak Detection"),
              numericInput("cpm_smooth_sigma", "Smoothing sigma (°C)",
                value = 3, min = 0.5, max = 10, step = 0.5),
              numericInput("cpm_min_sep",      "Min peak separation (°C)",
                value = 8, min = 1, max = 20, step = 1),
              numericInput("cpm_boundary_thresh", "Boundary threshold (0-1)",
                value = 0.10, min = 0.01, max = 0.5, step = 0.01)
            )
          )
        )
      ),

      # Right panel
      column(8,
        navset_card_underline(
          id = "cpm_tabs",

          nav_panel("📡  Preview",
            div(style = "padding:1rem 0;",
              div(class = "info-box", tags$span("ℹ", class = "info-icon"),
                "Full dF/dT trace. Use to choose your integration range."),
              plotOutput("cpm_preview_plot", height = "380px")
            )
          ),

          nav_panel("📈  Results",
            div(style = "padding:1rem 0;",
              uiOutput("cpm_result_badges"),
              plotOutput("cpm_result_plot", height = "400px"),
              br(),
              uiOutput("cpm_results_table")
            )
          ),



          nav_panel("🕑  History",
            div(style = "padding:1rem 0;",
              uiOutput("cpm_history_ui"),
              uiOutput("cpm_history_dl")
            )
          )
        )
      )
    )
    )  # Close tagList
  ),


  # -- CPM QC ----------------------------------------------------------------
  nav_panel(
    "CPM QC", icon = icon("magnifying-glass-chart"), value = "CPMQC",

    if (!cpm_functions_ok) {
      div(class = "lab-card", style = "border-color: #FF5C5C; margin: 2rem;",
        h4("\u26a0\ufe0f Missing: tm_analysis_functions.R", style = "color:#FF5C5C;"),
        p("Place tm_analysis_functions.R in the same folder as app.R and restart.", style = "color:#7A8FAD;"))
    } else tagList(

    div(class = "clear-button-container",
      actionButton("qc_clear", "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    # ---- Mode toggle --------------------------------------------------------
    div(style = "padding: 0 1rem 0.5rem;",
      div(class = "qc-mode-toggle",
        tags$button("Simple (2 samples)", id = "qc_btn_simple", class = "qc-mode-btn active",
          onclick = "Shiny.setInputValue('qc_mode', 'simple', {priority: 'event'});
                     document.getElementById('qc_btn_simple').classList.add('active');
                     document.getElementById('qc_btn_multi').classList.remove('active');"),
        tags$button("Multi-Sample (3\u201310)", id = "qc_btn_multi", class = "qc-mode-btn",
          onclick = "Shiny.setInputValue('qc_mode', 'multi', {priority: 'event'});
                     document.getElementById('qc_btn_multi').classList.add('active');
                     document.getElementById('qc_btn_simple').classList.remove('active');")
      )
    ),

    # =========================================================================
    # SIMPLE MODE
    # =========================================================================
    conditionalPanel("input.qc_mode == 'simple' || input.qc_mode == null || input.qc_mode == undefined",
    fluidRow(
      column(4,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Data File"),
          fileInput("qc_file", NULL, accept = ".csv",
            buttonLabel = "Browse\u2026", placeholder = "RotorGene Q CSV export"),
          uiOutput("qc_file_status")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Select Samples"),
          div(class = "info-box", tags$span("\u2139", class = "info-icon"),
            "Select two samples to compare (e.g. -GDP and +GDP)."),
          uiOutput("qc_sample_a_ui"),
          uiOutput("qc_sample_b_ui")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Sample Labels & Colours"),
          fluidRow(
            column(8, textInput("qc_label_a", "Label A", value = "-GDP")),
            column(4, div(class = "lbl", "Colour A"),
              div(style = "display:flex;align-items:center;gap:0.4rem;",
                tags$input(id = "qc_col_a_picker", type = "color", value = "#E41A1C",
                  style = "width:2.2rem;height:2.2rem;border:1px solid #1E2D45;border-radius:6px;background:transparent;cursor:pointer;padding:0.1rem;flex-shrink:0;",
                  onchange = "Shiny.setInputValue('qc_col_a', this.value);",
                  oninput  = "Shiny.setInputValue('qc_col_a', this.value);"),
                textInput("qc_col_a", NULL, value = "#E41A1C", width = "90px")))
          ),
          fluidRow(
            column(8, textInput("qc_label_b", "Label B", value = "+GDP")),
            column(4, div(class = "lbl", "Colour B"),
              div(style = "display:flex;align-items:center;gap:0.4rem;",
                tags$input(id = "qc_col_b_picker", type = "color", value = "#377EB8",
                  style = "width:2.2rem;height:2.2rem;border:1px solid #1E2D45;border-radius:6px;background:transparent;cursor:pointer;padding:0.1rem;flex-shrink:0;",
                  onchange = "Shiny.setInputValue('qc_col_b', this.value);",
                  oninput  = "Shiny.setInputValue('qc_col_b', this.value);"),
                textInput("qc_col_b", NULL, value = "#377EB8", width = "90px")))
          )
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Tm Values"),
          div(class = "info-box", tags$span("\u2139", class = "info-icon"),
            "Auto-detected as the maximum of the dF/dT peak. Override manually if needed."),
          fluidRow(
            column(6, numericInput("qc_tm_a", "Tm A (\u00b0C)", value = NULL, step = 0.1)),
            column(6, numericInput("qc_tm_b", "Tm B (\u00b0C)", value = NULL, step = 0.1))
          ),
          uiOutput("qc_tm_auto_status"),
          br(),
          checkboxInput("qc_show_tm_labels", "Show Tm values above bars", value = FALSE),
          checkboxInput("qc_show_dtm", "Show \u0394T\u2098 annotation", value = FALSE)
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Plot Title"),
          textInput("qc_title", NULL, placeholder = "e.g. NaOaUCP1 CPM QC"),
          div(class = "lab-card-title", "Plot Style"),
          numericInput("qc_linewidth", "Line width", value = 1.5, min = 0.5, max = 4, step = 0.25),
          br(),
          tags$button("\u2699  Advanced Plot Settings", class = "adv-toggle",
            onclick = "$('#qc_adv_panel').slideToggle(200)"),
          div(id = "qc_adv_panel", style = "display:none;",
            div(class = "settings-group",
              div(class = "settings-group-title", "Legend Position"),
              selectInput("qc_legend_pos", NULL,
                choices = c(
                  "Top right"    = "topright",
                  "Top left"     = "topleft",
                  "Bottom right" = "bottomright",
                  "Bottom left"  = "bottomleft"
                ),
                selected = "topright",
                width = "100%"
              )
            )
          )
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "6"), "Generate"),
          actionButton("qc_run", "\u25b6  Generate QC Plots", class = "btn-run"), br(), br(),
          uiOutput("qc_download_buttons")
        )
      ),
      column(8,
        uiOutput("qc_badges"),
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f321\ufe0f  Raw Fluorescence"),
          uiOutput("qc_raw_placeholder"),
          plotOutput("qc_raw_plot", height = "300px")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f4c8  dF/dT (unnormalised)"),
          plotOutput("qc_dfdt_plot", height = "300px")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f4ca  Tm Comparison"),
          plotOutput("qc_tm_plot", height = "320px")
        )
      )
    )
    ), # end conditionalPanel simple

    # =========================================================================
    # MULTI-SAMPLE MODE
    # =========================================================================
    conditionalPanel("input.qc_mode == 'multi'",
    fluidRow(
      column(4,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Data File"),
          div(class = "info-box", tags$span("\u2139", class = "info-icon"),
            "Same file format as Simple mode. Re-use an already uploaded file by switching modes without re-uploading."),
          fileInput("mqc_file", NULL, accept = ".csv",
            buttonLabel = "Browse\u2026", placeholder = "RotorGene Q CSV export"),
          uiOutput("mqc_file_status")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Select Samples"),
          div(class = "info-box", tags$span("\u2139", class = "info-icon"),
            "Select 3\u201310 samples. Colours are assigned automatically from a qualitative palette."),
          uiOutput("mqc_sample_select_ui")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Sample Labels"),
          div(class = "info-box", tags$span("\u2139", class = "info-icon"),
            "Optionally override the default sample names for cleaner plots."),
          uiOutput("mqc_labels_ui")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Plot Settings"),
          textInput("mqc_title", "Plot title", placeholder = "e.g. HsUCP1 Multi-Ligand QC"),
          numericInput("mqc_linewidth", "Line width", value = 1.5, min = 0.5, max = 4, step = 0.25)
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Generate"),
          actionButton("mqc_run", "\u25b6  Generate QC Plots", class = "btn-run"), br(), br(),
          uiOutput("mqc_download_buttons")
        )
      ),
      column(8,
        uiOutput("mqc_badges"),
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f321\ufe0f  Raw Fluorescence"),
          uiOutput("mqc_raw_placeholder"),
          plotOutput("mqc_raw_plot", height = "300px")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f4c8  dF/dT (unnormalised)"),
          plotOutput("mqc_dfdt_plot", height = "300px")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f4ca  Tm Comparison"),
          plotOutput("mqc_tm_plot", height = "320px")
        )
      )
    )
    ) # end conditionalPanel multi

    )  # Close tagList
  ),

  # -- CPM CONTOUR PLOTTING --------------------------------------------------
  nav_panel(
    "Gel Annotator", icon = icon("image"), value = "GEL",
    
    # Clear button
    div(class = "clear-button-container",
      actionButton("gel_clear", "🔄  Clear All Data", class = "btn-clear")
    ),
    
    div(style = "padding: 0 1rem 0.5rem;",
      div(class = "qc-mode-toggle",
        tags$button("Gel", id = "gel_btn_gel", class = "qc-mode-btn active",
          onclick = "Shiny.setInputValue('gel_tab_mode', 'gel', {priority: 'event'});
                     document.getElementById('gel_btn_gel').classList.add('active');
                     document.getElementById('gel_btn_western').classList.remove('active');"),
        tags$button("Western", id = "gel_btn_western", class = "qc-mode-btn",
          onclick = "Shiny.setInputValue('gel_tab_mode', 'western', {priority: 'event'});
                     document.getElementById('gel_btn_western').classList.add('active');
                     document.getElementById('gel_btn_gel').classList.remove('active');")
      )
    ),
    conditionalPanel("input.gel_tab_mode == 'gel' || input.gel_tab_mode == null || input.gel_tab_mode == undefined",
    fluidRow(
      # Left sidebar - Controls
      column(3,
        # Step 1: Upload
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Gel Image"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Upload TIFF, PNG, or JPEG gel images. TIFF files are fully supported."),
          fileInput("gel_image", NULL, accept = c(".tif", ".tiff", ".png", ".jpg", ".jpeg"),
            buttonLabel = "Browse…", placeholder = "No file selected")
        ),
        
        # Step 2: Crop
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Crop (Optional)"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Drag on the gel image to select crop area. Release, then click Apply to confirm."),
          fluidRow(
            column(6, actionButton("gel_apply_crop", "✓ Apply Crop",
                                   class="btn-run", style="width:100%;")),
            column(6, actionButton("gel_revert_crop", "↩ Revert Original",
                                   class="btn-secondary", style="width:100%;"))
          ),
          uiOutput("gel_crop_status")
        ),
        
        # Step 3: Mode selection
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Mark Features"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Click on the image to mark ladder bands or sample wells."),
          selectInput("gel_mode", "Marking Mode",
            choices = c("Crop" = "crop", "Ladder Bands" = "ladder", "Sample Wells" = "wells"),
            selected = "crop")
        ),
        
        # Step 4: Ladder preset
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Ladder Type"),
          selectInput("gel_ladder_type", NULL,
            choices = c("PageRuler Plus" = "pageruler",
                       "Precision Plus" = "precision",
                       "Color Prestained" = "prestained",
                       "Broad Range" = "broad",
                       "Custom" = "custom"),
            selected = "precision")
        ),
        
        # Step 5: Label settings
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Label Settings"),
          fluidRow(
            column(6, numericInput("gel_fontsize", "Font Size", value = 20, min = 10, max = 32, step = 2)),
            column(6, checkboxInput("gel_bold", "Bold Text", value = FALSE))
          ),
          fluidRow(
            column(6, numericInput("gel_ladder_offset", "Ladder Offset", value = 60, min = 40, max = 120, step = 10)),
            column(6, numericInput("gel_well_offset", "Well Offset", value = 40, min = 20, max = 80, step = 10))
          ),
          selectInput("gel_text_angle", "Well Label Orientation",
            choices = c("Horizontal" = "0", "Diagonal" = "45", "Vertical" = "90"),
            selected = "45")
        ),
        
        # Step 6: Preview and Export
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "6"), "Export"),
          checkboxInput("gel_preview", "Preview Mode (hide markers)", value = FALSE),
          selectInput("gel_bg_color", "Export Background",
            choices = c("Transparent" = "transparent", "White" = "white"),
            selected = "transparent"),
          br(),
          downloadButton("gel_export",      "\u2193 Download PNG",  class = "btn-download"),
          " ",
          downloadButton("gel_export_tiff", "\u2193 Download TIFF", class = "btn-download"),
          br(), br(),
          actionButton("gel_clear_markers", "🗑️ Clear All Markers", class = "btn-secondary")
        )
      ),
      
      # Center - Image display
      column(7,
        div(class = "lab-card",
          div(class = "lab-card-title", "🔬  Gel Image"),
          conditionalPanel(
            condition = "output.gel_image_loaded",
            div(style = "text-align: center;",
              plotOutput("gel_plot", height = "600px",
                click = "gel_click",
                brush = brushOpts(id="gel_crop_brush", fill="#00C2FF",
                                   stroke="#00C2FF", opacity=0.2, resetOnNew=TRUE))
            )
          ),
          conditionalPanel(
            condition = "!output.gel_image_loaded",
            div(style = "text-align: center; padding: 100px 20px; color: #7A8FAD;",
              icon("image", style = "font-size: 64px; margin-bottom: 20px;"),
              h4("No image loaded", style = "color: #7A8FAD;"),
              p("Upload a gel image to begin")
            )
          )
        )
      ),
      
      # Right sidebar - Markers list
      column(2,
        div(class = "lab-card",
          div(class = "lab-card-title", "🎯 Ladder Bands"),
          uiOutput("gel_ladder_list")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "🧪 Sample Wells"),
          uiOutput("gel_wells_list")
        ),
        div(class = "lab-card",
          div(class = "lab-card-title", "📊 Statistics"),
          uiOutput("gel_stats")
        )
      )
    )

    ),


        conditionalPanel("input.gel_tab_mode == 'western'",
    fluidRow(
      # Left sidebar — western controls
      column(3,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Western Image"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Upload your western blot image. Use the preview panel to crop and label it."),
          fileInput("gel_western", NULL,
            accept = c(".tif", ".tiff", ".png", ".jpg", ".jpeg"),
            buttonLabel = "Browse\u2026", placeholder = "No file selected"),
          uiOutput("gel_western_status")
        ),

    "output.gel_western_loaded",
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Crop (Optional)"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Switch to Crop mode, then drag on the preview. Release, then click Apply."),
          fluidRow(
            column(6, actionButton("gel_western_crop_apply", "✓ Apply Crop",
                                   class="btn-run", style="width:100%;")),
            column(6, actionButton("gel_western_revert_crop", "↩ Revert Original",
                                   class="btn-secondary", style="width:100%;"))
          )
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Mark Ladder Bands"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Click a band on the preview, enter its MW, then click Add."),
          selectInput("gel_western_mode", "Preview Click Mode",
            choices = c("Crop" = "crop", "Mark Ladder Bands" = "ladder"),
            selected = "crop", width = "100%"),
          conditionalPanel("input.gel_western_mode == 'ladder'",
            br(),
            uiOutput("gel_western_ladder_entry"),
            br(),
            actionButton("gel_western_clear_bands", "\U1f5d1  Clear All Bands",
              class = "btn-secondary",
              style = "font-size:0.78rem;padding:0.35rem 0.7rem;width:100%;")
          )
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Antibody Label"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Text shown to the right of the western in the stitched export."),
          textInput("gel_western_antibody", NULL, placeholder = "e.g. anti-HsUCP1")
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Enhance Contrast"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "0 = no adjustment. Higher values darken bands and lighten background."),
          sliderInput("gel_western_contrast", NULL,
            min = 0, max = 15, value = 0, step = 1, width = "100%")
        ),

        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "6"), "Stitch & Export"),
          div(class = "info-box", tags$span("ℹ", class = "info-icon"),
            "Combines the annotated gel (from the Gel tab) with the western below it."),
          numericInput("gel_gap_px", "Gap (px)", value = 10, min = 0, max = 200, step = 5),
          selectInput("gel_gap_color", "Gap colour",
            choices = c("Transparent" = "none", "Black" = "black", "White" = "white"),
            selected = "none", width = "100%"),
          br(),
          downloadButton("gel_stitch_png",      "\u2193 PNG (Gel + Western)",  class = "btn-download"), br(), br(),
          downloadButton("gel_stitch_tiff", "\u2193 TIFF (Gel + Western)", class = "btn-download"), br(), br(),
          actionButton("gel_western_clear", "\u2715  Remove Western", class = "btn-secondary")
        )

      ),

      # Centre — western preview
      column(7,
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f9ec  Western Blot Preview"),
          conditionalPanel("output.gel_western_loaded",
            div(class = "info-box", tags$span("ℹ", class = "info-icon"),
              "Select 'Mark Ladder Bands' mode in Step 3 to click bands. Select 'Crop' mode to crop."),
            div(style = "text-align:center;",
              plotOutput("gel_western_preview", height = "420px",
                         click = "gel_western_click",
                         brush = brushOpts(id="gel_western_crop_brush", fill="#00C2FF",
                                           stroke="#00C2FF", opacity=0.2, resetOnNew=TRUE)))
          ),
          conditionalPanel("!output.gel_western_loaded",
            div(style = "text-align:center;padding:80px 20px;color:#7A8FAD;",
              icon("image", style = "font-size:64px;margin-bottom:20px;"),
              h4("No western image loaded", style = "color:#7A8FAD;"),
              p("Upload a western blot image to begin"))
          )
        )
      ),

      # Right — western ladder list
      column(2,
        div(class = "lab-card",
          div(class = "lab-card-title", "\U0001f3af Ladder Bands"),
          uiOutput("gel_western_ladder_list")
        )
      )
    )
    ) # end western tab
  ),
  nav_panel(
    "UCP1 Proton Conductance", icon = icon("vial"), value = "UCP1",

    # Clear data button
    div(class = "clear-button-container",
      actionButton("ucp1_clear", "🔄  Clear All Data", class = "btn-clear")
    ),
    
    # Clearing overlay (shown during clear operation)
    uiOutput("ucp1_clearing_overlay"),

    div(id = "ucp1_content_section",
    fluidRow(
      column(4,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "1"), "Upload Proton Calibration"),
          fileInput("ucp1_cal_file", NULL, accept = ".csv",
            buttonLabel = "Browse…", placeholder = "Proton calibration CSV"),
          uiOutput("ucp1_cal_status")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "2"), "Upload Capacity Calibration"),
          fileInput("ucp1_cap_file", NULL, accept = ".csv",
            buttonLabel = "Browse…", placeholder = "Capacity calibration CSV"),
          uiOutput("ucp1_cap_status")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "3"), "Plot Settings"),
          numericInput("ucp1_line_width", "Line width", value = 1, min = 0.5, max = 3, step = 0.25),
          checkboxInput("ucp1_show_titles", "Show plot titles", value = TRUE),
          conditionalPanel(
            condition = "input.ucp1_show_titles == true",
            textInput("ucp1_proton_trace_title", "Proton calibration trace title", 
                      placeholder = "Default: Intensity (a.u.)"),
            textInput("ucp1_proton_curve_title", "Proton calibration curve title", 
                      placeholder = "Default: FU/Proton Calibration Curve"),
            textInput("ucp1_capacity_trace_title", "Capacity calibration trace title", 
                      placeholder = "Default: Intensity (a.u.)"),
            textInput("ucp1_capacity_curve_title", "Capacity calibration curve title", 
                      placeholder = "Default: Internal Volume Calibration")
          )
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "📊  Detected Plateaus"),
          uiOutput("ucp1_plateau_info"),
          br(),
          div(style = "max-height: 300px; overflow-y: auto;",
            tableOutput("ucp1_plateau_table")
          ),
          br(),
          uiOutput("ucp1_plateau_selector")
        )
      ),

      column(8,
        div(class = "lab-card",
          div(class = "lab-card-title", "📈  Proton Calibration Trace"),
          uiOutput("ucp1_cal_placeholder"),
          plotOutput("ucp1_cal_plot", height = "320px"),
          br(),
          uiOutput("ucp1_cal_download")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "📉  FU/Proton Calibration Curve"),
          uiOutput("ucp1_calibcurve_ui")
        )
      )
    ),
    fluidRow(
      column(4,
        div(class = "lab-card",
          div(class = "lab-card-title", "📊  Detected Plateaus (Capacity)"),
          uiOutput("ucp1_cap_plateau_info"),
          br(),
          div(style = "max-height: 300px; overflow-y: auto;",
            tableOutput("ucp1_cap_plateau_table")
          ),
          br(),
          uiOutput("ucp1_cap_plateau_selector")
        )
      ),
      
      column(8,
        div(class = "lab-card",
          div(class = "lab-card-title", "📈  Capacity Calibration Trace"),
          uiOutput("ucp1_cap_placeholder"),
          plotOutput("ucp1_cap_plot", height = "320px"),
          br(),
          uiOutput("ucp1_cap_download")
        ),
        
        div(class = "lab-card",
          div(class = "lab-card-title", "📉  Internal Volume Calibration Curve"),
          uiOutput("ucp1_cap_calibcurve_ui")
        )
      )
    ),
    
    # -- Raw Sample Data --------------------------------------------------------
    fluidRow(style = "margin-top: 0 !important;",
      column(12,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "4"), "Upload Raw Sample Data"),
          fileInput("ucp1_raw_files", NULL, 
                    accept = ".csv",
                    multiple = TRUE,
                    buttonLabel = "Browse…", 
                    placeholder = "Select 12-18 raw trace CSV files"),
          uiOutput("ucp1_raw_status"),
          br(),
          div(style = "max-height: 400px; overflow-y: auto; overflow-x: auto;",
            tableOutput("ucp1_raw_table")
          ),
          br(),
          uiOutput("ucp1_raw_download")
        )
      )
    ),
    
    # -- Processed Data ---------------------------------------------------------
    fluidRow(style = "margin-top: 0 !important;",
      column(12,
        div(class = "lab-card",
          div(class = "lab-card-title", span(class = "step-number", "5"), "Processed Data ([H+] in mM)"),
          uiOutput("ucp1_processed_status"),
          br(),
          div(style = "max-height: 400px; overflow-y: auto; overflow-x: auto;",
            tableOutput("ucp1_processed_table")
          ),
          br(),
          uiOutput("ucp1_processed_download")
        )
      )
    ),
    
    # -- Export Complete Analysis -----------------------------------------------
    fluidRow(
      column(12,
        div(class = "lab-card",
          div(class = "lab-card-title", "📦  Export Complete Analysis"),
          uiOutput("ucp1_export_status"),
          br(),
          uiOutput("ucp1_export_button")
        )
      )
    )
    )  # Close ucp1_content_section div
  ),

  # -- AKTA ------------------------------------------------------------------
  # -- Export Report nav item
  nav_item(
    downloadButton("export_report",
      "↓  Export Report (.zip)",
      style = "background:transparent;border:1px solid #1E2D45;color:#7A8FAD;font-family:'JetBrains Mono',monospace;font-size:0.72rem;padding:0.3rem 0.75rem;border-radius:6px;margin:0.5rem;")
  )
) # end page_navbar


# -- 4. Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # ==========================================================================
  # GEL ANNOTATOR SERVER LOGIC - FIXED VERSION
  # ==========================================================================
  
  # Initialize reactive values for gel annotator
  gel_data <- reactiveValues(
    image = NULL,
    image_width = NULL,
    image_height = NULL,
    ladder_markers = data.frame(x = numeric(), y = numeric(), mw = numeric(), id = character(), stringsAsFactors = FALSE),
    well_markers = data.frame(x = numeric(), y = numeric(), label = character(), id = character(), stringsAsFactors = FALSE),
    crop_corners = list(),
    crop_active = FALSE,
    cropped = FALSE,
    marker_observers = list(),
    plot_trigger = 0,
    western_image=NULL, western_raw=NULL,
    western_crop_active=FALSE, western_crop_corners=list(),
    western_ladder_markers=data.frame(x=numeric(),y=numeric(),mw=numeric(),stringsAsFactors=FALSE),
    western_pending_click=NULL, western_plot_trigger=0,
    image_original=NULL, western_original=NULL,
    gel_crop_pending=NULL, western_crop_pending=NULL
  )
  
  # Ladder presets
  gel_ladder_presets <- list(
    pageruler = c(250, 130, 100, 70, 55, 35, 25, 15, 10),
    precision = c(250, 150, 100, 75, 50, 37, 25, 20, 15, 10),
    prestained = c(245, 180, 135, 100, 75, 63, 48, 35, 25, 20, 17, 11),
    broad = c(200, 116, 97, 66, 45, 31, 21, 14, 6),
    custom = c()
  )
  
  # Load image
  observeEvent(input$gel_image, {
    req(input$gel_image)
    
    tryCatch({
      img <- image_read(input$gel_image$datapath)
      img_info <- image_info(img)
      gel_data$image_original <- img
      
      gel_data$image <- img
      gel_data$image_width <- img_info$width
      gel_data$image_height <- img_info$height
      gel_data$cropped <- FALSE
      
      # Reset markers
      gel_data$ladder_markers <- data.frame(x = numeric(), y = numeric(), mw = numeric(), id = character(), stringsAsFactors = FALSE)
      gel_data$well_markers <- data.frame(x = numeric(), y = numeric(), label = character(), id = character(), stringsAsFactors = FALSE)
      gel_data$crop_corners <- list()
      gel_data$crop_active <- FALSE
      
      showNotification("✓ Image loaded successfully", type = "message", duration = 2)
    }, error = function(e) {
      showNotification(paste("Error loading image:", e$message), type = "error", duration = 5)
    })
  })
  
  # Clear all data
  observeEvent(input$gel_clear, {
    gel_data$image <- NULL
    gel_data$image_width <- NULL
    gel_data$image_height <- NULL
    gel_data$ladder_markers <- data.frame(x = numeric(), y = numeric(), mw = numeric(), id = character(), stringsAsFactors = FALSE)
    gel_data$well_markers <- data.frame(x = numeric(), y = numeric(), label = character(), id = character(), stringsAsFactors = FALSE)
    gel_data$crop_corners <- list()
    gel_data$crop_active <- FALSE
    gel_data$cropped <- FALSE
    
    showNotification("✓ All data cleared", type = "message", duration = 2)
  })
  
  # Image loaded indicator
  output$gel_image_loaded <- reactive({
    !is.null(gel_data$image)
  })
  outputOptions(output, "gel_image_loaded", suspendWhenHidden = FALSE)
  
  # Crop mode

  

  

  
  # Apply crop

  
  # Handle clicks on image
  observeEvent(input$gel_click, {
    req(gel_data$image)
    
    # Ignore clicks when in crop mode — brush handles crop
    if (!is.null(input$gel_mode) && input$gel_mode == "crop") return()
    
    click <- input$gel_click
    
    # Handle crop mode
    if (gel_data$crop_active && length(gel_data$crop_corners) < 2) {
      gel_data$crop_corners <- append(gel_data$crop_corners, list(list(x = click$x, y = click$y)))
      
      if (length(gel_data$crop_corners) == 2) {
        showNotification("✓ Crop area defined. Click 'Apply Crop' to crop.", type = "message", duration = 3)
      }
      return()
    }
    
    if (gel_data$crop_active) {
      return()
    }
    
    # Handle marker placement
    if (input$gel_mode == "ladder") {
      # Get current ladder type and presets
      ladder_type <- isolate(input$gel_ladder_type)
      preset_mws <- gel_ladder_presets[[ladder_type]]
      
      # Calculate next MW value
      current_count <- nrow(isolate(gel_data$ladder_markers))
      next_mw <- if (length(preset_mws) > current_count) {
        preset_mws[current_count + 1]
      } else {
        0
      }
      
      new_marker <- data.frame(
        x = round(click$x, 1),
        y = round(click$y, 1),
        mw = next_mw,
        id = paste0("m", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6"))),
        stringsAsFactors = FALSE
      )
      
      gel_data$ladder_markers <- rbind(isolate(gel_data$ladder_markers), new_marker)
      gel_data$plot_trigger <- isolate(gel_data$plot_trigger) + 1  # Trigger plot redraw
      
    } else if (input$gel_mode == "wells") {
      current_count <- nrow(isolate(gel_data$well_markers))
      
      new_marker <- data.frame(
        x = round(click$x, 1),
        y = round(click$y, 1),
        label = paste0("Sample ", current_count + 1),
        id = paste0("w", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6"))),
        stringsAsFactors = FALSE
      )
      
      gel_data$well_markers <- rbind(isolate(gel_data$well_markers), new_marker)
      gel_data$plot_trigger <- isolate(gel_data$plot_trigger) + 1  # Trigger plot redraw
    }
  })
  
  # Clear markers
  observeEvent(input$gel_clear_markers, {
    gel_data$ladder_markers <- data.frame(x = numeric(), y = numeric(), mw = numeric(), id = character(), stringsAsFactors = FALSE)
    gel_data$well_markers <- data.frame(x = numeric(), y = numeric(), label = character(), id = character(), stringsAsFactors = FALSE)
    gel_data$plot_trigger <- isolate(gel_data$plot_trigger) + 1  # Trigger plot redraw
    showNotification("✓ All markers cleared", type = "message", duration = 2)
  })
  
  # Render image with markers
  output$gel_plot <- renderPlot({
    req(gel_data$image)
    
    # Depend on plot_trigger to control when plot redraws
    gel_data$plot_trigger
    
    img_raster <- as.raster(gel_data$image)
    
    preview <- input$gel_preview
    ladder_offset <- input$gel_ladder_offset
    well_offset <- input$gel_well_offset
    
    # Use isolate() to read marker data without creating dependencies
    ladder_markers <- isolate(gel_data$ladder_markers)
    well_markers <- isolate(gel_data$well_markers)
    
    if (preview && (nrow(ladder_markers) > 0 || nrow(well_markers) > 0)) {
      # Preview mode with scaled image
      par(mar = c(0, 0, 0, 0))
      
      # Scale image to 75% to leave more room for labels
      scale_factor <- 0.75
      img_width <- gel_data$image_width * scale_factor
      img_height <- gel_data$image_height * scale_factor
      
      left_pad <- if (nrow(ladder_markers) > 0) max(ladder_offset + 20, round(input$gel_fontsize * 9)) else 0
      
      # Calculate top padding dynamically based on text angle and label length
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        font_size <- input$gel_fontsize
        
        # Find longest label
        max_label_len <- max(nchar(well_markers$label))
        
        # Calculate extra space needed based on angle
        if (text_angle == 45) {
          # Diagonal: text extends upward at 45°
          # Approximate char width in pixels at given font size
          char_width <- font_size * 0.6
          diagonal_extension <- max_label_len * char_width * sin(pi/4)  # sin(45°) ≈ 0.707
          top_pad <- well_offset + 20 + diagonal_extension
        } else if (text_angle == 90) {
          # Vertical: text goes straight up, needs char width space
          char_width <- font_size * 0.6
          top_pad <- well_offset + 20 + (max_label_len * char_width * 0.5)
        } else {
          # Horizontal: standard padding
          top_pad <- well_offset + 20
        }
      } else {
        top_pad <- 0
      }
      
      total_width <- img_width + left_pad
      total_height <- img_height + top_pad
      
      plot(1, type = "n", xlim = c(0, total_width), ylim = c(0, total_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      rect(0, 0, total_width, total_height, col = "white", border = NA)
      
      rasterImage(img_raster, left_pad, 0, left_pad + img_width, img_height)
      
      # Draw ladder labels
      if (nrow(ladder_markers) > 0) {
        for (i in 1:nrow(ladder_markers)) {
          marker <- ladder_markers[i, ]
          y_pos <- marker$y * scale_factor
          x_label <- left_pad - 10
          
          text(x_label, y_pos, paste0(marker$mw, " kDa"),
               pos = 2, cex = input$gel_fontsize / 12,
               font = if (input$gel_bold) 2 else 1)
          
          segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
        }
      }
      
      # Draw well labels with rotation
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        
        for (i in 1:nrow(well_markers)) {
          marker <- well_markers[i, ]
          x_pos <- marker$x * scale_factor + left_pad
          y_label <- img_height + 10
          
          # Adjust positioning based on angle
          if (text_angle == 0) {
            # Horizontal
            text(x_pos, y_label, marker$label,
                 pos = 3, cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 0)
          } else if (text_angle == 90) {
            # Vertical
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0.5), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 90)
          } else {
            # Diagonal (45 degrees)
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 45)
          }
          
          segments(x_pos, y_label - 5, x_pos, img_height, lwd = 1)
        }
      }
      
    } else {
      # Editing mode
      par(mar = c(0, 0, 0, 0))
      plot(1, type = "n", xlim = c(0, gel_data$image_width), ylim = c(0, gel_data$image_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      rasterImage(img_raster, 0, 0, gel_data$image_width, gel_data$image_height)
      
      # Draw crop rectangle
      if (gel_data$crop_active && length(gel_data$crop_corners) > 0) {
        for (corner in gel_data$crop_corners) {
          points(corner$x, corner$y, pch = 19, col = "#10b981", cex = 2)
        }
        if (length(gel_data$crop_corners) == 2) {
          x_vals <- c(gel_data$crop_corners[[1]]$x, gel_data$crop_corners[[2]]$x)
          y_vals <- c(gel_data$crop_corners[[1]]$y, gel_data$crop_corners[[2]]$y)
          rect(min(x_vals), min(y_vals), max(x_vals), max(y_vals),
               border = "#10b981", lwd = 3, lty = 2)
        }
      }
      
      # Draw ladder markers (red)
      if (nrow(ladder_markers) > 0) {
        points(ladder_markers$x, ladder_markers$y,
               pch = 19, col = "#dc2626", cex = 2)
        text(ladder_markers$x, ladder_markers$y,
             paste0(ladder_markers$mw, " kDa"),
             pos = 4, col = "white", font = 2, cex = 0.9)
      }
      
      # Draw well markers (blue)
      if (nrow(well_markers) > 0) {
        points(well_markers$x, well_markers$y,
               pch = 19, col = "#2563eb", cex = 2)
        text(well_markers$x, well_markers$y,
             well_markers$label,
             pos = 3, col = "white", font = 2, cex = 0.9)
      }
    }
  }, bg = "white")
  
  # Ladder markers list
  output$gel_ladder_list <- renderUI({
    # Depend on plot_trigger - only re-render when markers added/deleted
    gel_data$plot_trigger
    
    # Use isolate to read marker data without creating dependency
    ladder_markers <- isolate(gel_data$ladder_markers)
    
    if (nrow(ladder_markers) == 0) {
      return(p("No ladder bands marked", style = "color: #7A8FAD; font-size: 0.9rem; padding: 0.5rem;"))
    }
    
    marker_ui <- lapply(1:nrow(ladder_markers), function(i) {
      marker <- ladder_markers[i, ]
      marker_id <- marker$id
      
      div(style = "background: #1E2D45; padding: 8px; border-radius: 4px; margin-bottom: 8px;",
        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
          span(paste("Band", i), style = "color: #7A8FAD; font-size: 0.85rem;"),
          actionButton(paste0("gel_del_ladder_", marker_id), "✕", 
            style = "background: #FF5C5C; color: white; border: none; padding: 2px 8px; font-size: 0.8rem; border-radius: 3px; cursor: pointer;")
        ),
        numericInput(paste0("gel_mw_", marker_id), NULL, value = marker$mw, min = 0, step = 1,
          width = "100%")
      )
    })
    
    div(style = "max-height: 300px; overflow-y: auto;", tagList(marker_ui))
  })
  
  # Well markers list
  output$gel_wells_list <- renderUI({
    # Depend on plot_trigger - only re-render when markers added/deleted
    gel_data$plot_trigger
    
    # Use isolate to read marker data without creating dependency
    well_markers <- isolate(gel_data$well_markers)
    
    if (nrow(well_markers) == 0) {
      return(p("No wells marked", style = "color: #7A8FAD; font-size: 0.9rem; padding: 0.5rem;"))
    }
    
    marker_ui <- lapply(1:nrow(well_markers), function(i) {
      marker <- well_markers[i, ]
      marker_id <- marker$id
      
      div(style = "background: #1E2D45; padding: 8px; border-radius: 4px; margin-bottom: 8px;",
        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
          span(paste("Well", i), style = "color: #7A8FAD; font-size: 0.85rem;"),
          actionButton(paste0("gel_del_well_", marker_id), "✕",
            style = "background: #FF5C5C; color: white; border: none; padding: 2px 8px; font-size: 0.8rem; border-radius: 3px; cursor: pointer;")
        ),
        textInput(paste0("gel_label_", marker_id), NULL, value = marker$label,
          width = "100%", placeholder = "Sample name")
      )
    })
    
    div(style = "max-height: 300px; overflow-y: auto;", tagList(marker_ui))
  })
  
  # FIXED: Update marker values - single observer that watches all inputs
  observe({
    # For each ladder marker, watch its input
    if (nrow(gel_data$ladder_markers) > 0) {
      for (i in 1:nrow(gel_data$ladder_markers)) {
        marker_id <- gel_data$ladder_markers$id[i]
        input_id <- paste0("gel_mw_", marker_id)
        
        # Get the current value from input
        new_val <- input[[input_id]]
        
        # Update if changed and valid
        if (!is.null(new_val) && !is.na(new_val)) {
          current_val <- gel_data$ladder_markers$mw[gel_data$ladder_markers$id == marker_id]
          if (length(current_val) > 0 && new_val != current_val) {
            gel_data$ladder_markers$mw[gel_data$ladder_markers$id == marker_id] <- new_val
          }
        }
      }
    }
    
    # For each well marker, watch its input
    if (nrow(gel_data$well_markers) > 0) {
      for (i in 1:nrow(gel_data$well_markers)) {
        marker_id <- gel_data$well_markers$id[i]
        input_id <- paste0("gel_label_", marker_id)
        
        new_val <- input[[input_id]]
        
        if (!is.null(new_val)) {
          current_val <- gel_data$well_markers$label[gel_data$well_markers$id == marker_id]
          if (length(current_val) > 0 && new_val != current_val) {
            gel_data$well_markers$label[gel_data$well_markers$id == marker_id] <- new_val
          }
        }
      }
    }
  })
  
  # FIXED: Delete markers - observe all delete buttons dynamically
  observe({
    # Check all possible ladder delete buttons
    if (nrow(gel_data$ladder_markers) > 0) {
      for (i in 1:nrow(gel_data$ladder_markers)) {
        marker_id <- gel_data$ladder_markers$id[i]
        btn_id <- paste0("gel_del_ladder_", marker_id)
        
        # Check if this button was clicked
        if (!is.null(input[[btn_id]]) && input[[btn_id]] > 0) {
          # Use isolate to prevent reactive loop
          isolate({
            gel_data$ladder_markers <- gel_data$ladder_markers[gel_data$ladder_markers$id != marker_id, ]
            gel_data$plot_trigger <- gel_data$plot_trigger + 1  # Trigger plot redraw
          })
        }
      }
    }
    
    # Check all possible well delete buttons
    if (nrow(gel_data$well_markers) > 0) {
      for (i in 1:nrow(gel_data$well_markers)) {
        marker_id <- gel_data$well_markers$id[i]
        btn_id <- paste0("gel_del_well_", marker_id)
        
        if (!is.null(input[[btn_id]]) && input[[btn_id]] > 0) {
          isolate({
            gel_data$well_markers <- gel_data$well_markers[gel_data$well_markers$id != marker_id, ]
            gel_data$plot_trigger <- gel_data$plot_trigger + 1  # Trigger plot redraw
          })
        }
      }
    }
  })
  
  # Statistics
  output$gel_stats <- renderUI({
    # Depend on plot_trigger to control when stats update
    gel_data$plot_trigger
    
    # Use isolate to read marker data
    ladder_markers <- isolate(gel_data$ladder_markers)
    well_markers <- isolate(gel_data$well_markers)
    
    n_ladder <- nrow(ladder_markers)
    n_wells <- nrow(well_markers)
    
    mw_range <- if (n_ladder >= 2) {
      paste0(min(ladder_markers$mw), "-", max(ladder_markers$mw), " kDa")
    } else {
      "N/A"
    }
    
    div(style = "padding: 0.5rem;",
      div(style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
        span("Ladder Bands:", style = "color: #7A8FAD;"),
        span(n_ladder, style = "color: #00C2FF; font-weight: bold;")
      ),
      div(style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
        span("Sample Wells:", style = "color: #7A8FAD;"),
        span(n_wells, style = "color: #00C2FF; font-weight: bold;")
      ),
      div(style = "display: flex; justify-content: space-between;",
        span("MW Range:", style = "color: #7A8FAD;"),
        span(mw_range, style = "color: #00C2FF; font-weight: bold;")
      )
    )
  })
  
  # Export function
  output$gel_export <- downloadHandler(
    filename = function() {
      paste0("gel_labeled_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(gel_data$image)
      
      # Use isolate to read markers
      ladder_markers <- isolate(gel_data$ladder_markers)
      well_markers <- isolate(gel_data$well_markers)
      
      # Use SAME scaling as preview (75%)
      scale_factor <- 0.75
      img_width <- gel_data$image_width * scale_factor
      img_height <- gel_data$image_height * scale_factor
      
      ladder_offset <- input$gel_ladder_offset
      well_offset <- input$gel_well_offset
      
      left_pad <- if (nrow(ladder_markers) > 0) max(ladder_offset + 20, round(input$gel_fontsize * 6)) else 0
      
      # Calculate top padding dynamically - SAME as preview
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        font_size <- input$gel_fontsize
        
        # Find longest label
        max_label_len <- max(nchar(well_markers$label))
        
        # Calculate extra space needed based on angle
        if (text_angle == 45) {
          # Diagonal: text extends upward at 45°
          char_width <- font_size * 0.6
          diagonal_extension <- max_label_len * char_width * sin(pi/4)
          top_pad <- well_offset + 20 + diagonal_extension
        } else if (text_angle == 90) {
          # Vertical: text goes straight up
          char_width <- font_size * 0.6
          top_pad <- well_offset + 20 + (max_label_len * char_width * 0.5)
        } else {
          # Horizontal: standard padding
          top_pad <- well_offset + 20
        }
      } else {
        top_pad <- 0
      }
      
      total_width <- img_width + left_pad
      total_height <- img_height + top_pad
      
      # Get background color preference
      bg_color <- if (input$gel_bg_color == "white") "white" else NA
      
      # Transparent or white background
      png(file, width = total_width, height = total_height,
          units = "px", bg = bg_color, res = 96)
      
      img_raster <- as.raster(gel_data$image)
      
      par(mar = c(0, 0, 0, 0))
      
      plot(1, type = "n", xlim = c(0, total_width), ylim = c(0, total_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      
      # Draw white rectangle if white background selected
      if (input$gel_bg_color == "white") {
        rect(0, 0, total_width, total_height, col = "white", border = NA)
      }
      
      rasterImage(img_raster, left_pad, 0, left_pad + img_width, img_height)
      
      # Ladder labels (match preview exactly)
      if (nrow(ladder_markers) > 0) {
        for (i in 1:nrow(ladder_markers)) {
          marker <- ladder_markers[i, ]
          y_pos <- marker$y * scale_factor  # Scale position!
          x_label <- left_pad - 10
          
          text(x_label, y_pos, paste0(marker$mw, " kDa"),
               pos = 2, cex = input$gel_fontsize / 12,
               font = if (input$gel_bold) 2 else 1)
          
          segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
        }
      }
      
      # Well labels with rotation (match preview exactly)
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        
        for (i in 1:nrow(well_markers)) {
          marker <- well_markers[i, ]
          x_pos <- marker$x * scale_factor + left_pad  # Scale position!
          y_label <- img_height + 10
          
          # Adjust positioning based on angle
          if (text_angle == 0) {
            # Horizontal
            text(x_pos, y_label, marker$label,
                 pos = 3, cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 0)
          } else if (text_angle == 90) {
            # Vertical
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0.5), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 90)
          } else {
            # Diagonal (45 degrees)
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 45)
          }
          
          segments(x_pos, y_label - 5, x_pos, img_height, lwd = 1)
        }
      }
      
      dev.off()
      
      showNotification("✓ Image exported successfully!", type = "message", duration = 3)
    }
  )

  # Gel TIFF export — identical rendering, tiff() device instead of png()
  output$gel_export_tiff <- downloadHandler(
    filename = function() {
      paste0("gel_labeled_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")
    },
    content = function(file) {
      req(gel_data$image)
      
      ladder_markers <- isolate(gel_data$ladder_markers)
      well_markers   <- isolate(gel_data$well_markers)
      
      scale_factor <- 0.75
      img_width    <- gel_data$image_width  * scale_factor
      img_height   <- gel_data$image_height * scale_factor
      
      ladder_offset <- input$gel_ladder_offset
      well_offset   <- input$gel_well_offset
      left_pad      <- if (nrow(ladder_markers) > 0) ladder_offset + 20 else 0
      
      if (nrow(well_markers) > 0) {
        text_angle    <- as.numeric(input$gel_text_angle)
        font_size     <- input$gel_fontsize
        max_label_len <- max(nchar(well_markers$label))
        if (text_angle == 45) {
          char_width <- font_size * 0.6
          top_pad    <- well_offset + 20 + max_label_len * char_width * sin(pi / 4)
        } else if (text_angle == 90) {
          char_width <- font_size * 0.6
          top_pad    <- well_offset + 20 + (max_label_len * char_width * 0.5)
        } else {
          top_pad <- well_offset + 20
        }
      } else {
        top_pad <- 0
      }
      
      total_width  <- img_width  + left_pad
      total_height <- img_height + top_pad
      bg_color     <- if (input$gel_bg_color == "white") "white" else "transparent"
      
      # TIFF at 300 dpi for publication quality
      tiff(file,
           width      = total_width,
           height     = total_height,
           units      = "px",
           bg         = bg_color,
           res        = 300,
           compression = "lzw")
      
      img_raster <- as.raster(gel_data$image)
      par(mar = c(0, 0, 0, 0))
      plot(1, type = "n", xlim = c(0, total_width), ylim = c(0, total_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      
      if (input$gel_bg_color == "white")
        rect(0, 0, total_width, total_height, col = "white", border = NA)
      
      rasterImage(img_raster, left_pad, 0, left_pad + img_width, img_height)
      
      if (nrow(ladder_markers) > 0) {
        for (i in seq_len(nrow(ladder_markers))) {
          marker <- ladder_markers[i, ]
          y_pos  <- marker$y * scale_factor
          x_label <- left_pad - 10
          text(x_label, y_pos, paste0(marker$mw, " kDa"),
               pos = 2, cex = input$gel_fontsize / 12,
               font = if (input$gel_bold) 2 else 1)
          segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
        }
      }
      
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        for (i in seq_len(nrow(well_markers))) {
          marker  <- well_markers[i, ]
          x_pos   <- marker$x * scale_factor + left_pad
          y_label <- img_height + 10
          if (text_angle == 0) {
            text(x_pos, y_label, marker$label,
                 pos = 3, cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 0)
          } else if (text_angle == 90) {
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0.5), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 90)
          } else {
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 45)
          }
          segments(x_pos, y_label - 5, x_pos, img_height, lwd = 1)
        }
      }
      
      dev.off()
      showNotification("\u2713 TIFF exported successfully!", type = "message", duration = 3)
    }
  )
  
  # ===========================================================================
  # CPM CONTOUR PLOTTING SERVER LOGIC
  # ===========================================================================
  
  # Reactive values for contour data
  contour_data <- reactiveValues(
    dfdt_raw = NULL,        # Raw dF/dT data
    tm_raw = NULL,          # Raw Tm data
    dfdt_processed = NULL,  # Processed: mean & SEM
    tm_processed = NULL,    # Processed Tm data with pairing
    sample_names = NULL,    # Unique sample names
    temperatures = NULL,    # Temperature points
    tm_pairing = NULL       # Maps Tm rows to dFdT samples
  )
  
  # File upload: dF/dT data
  observeEvent(input$contour_dfdt_file, {
    req(input$contour_dfdt_file)
    
    tryCatch({
      # Read CSV - fileEncoding handles UTF-8 BOM from Prism exports
      # check.names = FALSE preserves spaces and special chars in sample names
      raw_data <- read.csv(input$contour_dfdt_file$datapath,
                           stringsAsFactors = FALSE,
                           check.names = FALSE,
                           fileEncoding = "UTF-8-BOM")
      
      # First column is Temperature - read.csv puts header as colnames,
      # so raw_data[,1] contains all temperature values directly
      temperatures <- as.numeric(raw_data[, 1])
      temperatures <- temperatures[!is.na(temperatures)]
      
      # Column names (excluding first "Temp" column) are the sample names
      # Prism exports repeat each sample name once per replicate
      sample_names <- colnames(raw_data)[-1]
      
      # dF/dT values: all rows, all columns except temperature
      dfdt_values <- as.data.frame(lapply(raw_data[, -1, drop = FALSE], as.numeric))
      dfdt_values <- dfdt_values[seq_len(length(temperatures)), , drop = FALSE]
      colnames(dfdt_values) <- sample_names
      
      # Store raw data
      contour_data$dfdt_raw <- list(
        temperatures = temperatures,
        sample_names = sample_names,
        values = dfdt_values
      )
      
      n_unique <- length(unique(sample_names))
      n_reps   <- table(sample_names)[1]
      showNotification(
        sprintf("✓ dF/dT loaded: %d samples × %d replicates, %d temps",
                n_unique, n_reps, length(temperatures)),
        type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error reading dF/dT file:", e$message), type = "error", duration = 5)
    })
  })
  
  observeEvent(input$contour_tm_file, {
    req(input$contour_tm_file)
    
    tryCatch({
      # Read CSV - fileEncoding handles UTF-8 BOM, check.names preserves labels
      raw_data <- read.csv(input$contour_tm_file$datapath,
                           stringsAsFactors = FALSE,
                           check.names = FALSE,
                           fileEncoding = "UTF-8-BOM")
      
      # First column is concentration/condition labels (header already consumed by read.csv)
      concentrations <- as.character(raw_data[, 1])
      n_rows <- nrow(raw_data)
      
      # Extract Tm replicates (all columns except first)
      tm_values <- list()
      tm_means  <- numeric(n_rows)
      tm_sems   <- numeric(n_rows)
      
      for (i in seq_len(n_rows)) {
        values <- as.numeric(raw_data[i, -1])
        values <- values[!is.na(values)]
        
        tm_values[[i]] <- values
        tm_means[i]    <- if (length(values) > 0) mean(values) else NA
        tm_sems[i]     <- if (length(values) > 1) sd(values) / sqrt(length(values)) else 0
      }
      
      # Store raw Tm data with calculated statistics
      contour_data$tm_raw <- list(
        concentrations = concentrations,
        values         = tm_values,
        means          = tm_means,
        sems           = tm_sems,
        n_rows         = n_rows
      )
      
      # Reset pairing (user must re-assign after reload)
      contour_data$tm_pairing <- NULL
      
      showNotification(
        sprintf("✓ Tm loaded: %d conditions", n_rows),
        type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error reading Tm file:", e$message), type = "error", duration = 5)
    })
  })
  
  # Tm pairing UI - show after both files loaded and dFdT processed
  output$contour_tm_pairing_ui <- renderUI({
    req(contour_data$tm_raw, contour_data$dfdt_raw)
    
    tm_raw <- contour_data$tm_raw
    # Derive unique sample names directly from the raw file — no need to wait for Process
    unique_samples <- unique(contour_data$dfdt_raw$sample_names)
    
    pairing_inputs <- lapply(1:tm_raw$n_rows, function(i) {
      conc_label <- tm_raw$concentrations[i]
      tm_mean    <- tm_raw$means[i]
      
      div(style = "margin-bottom: 10px; background: #1E2D45; padding: 8px; border-radius: 4px;",
        div(style = "color: #7A8FAD; font-size: 0.85rem; margin-bottom: 4px;",
          sprintf("Row %d: %s (Tm: %.1f°C)", i, conc_label, tm_mean)
        ),
        selectInput(paste0("contour_tm_pair_", i), NULL,
          choices = c("-- Select sample --", unique_samples),
          selected = if (!is.null(contour_data$tm_pairing) && i <= length(contour_data$tm_pairing)) {
            contour_data$tm_pairing[i]
          } else {
            "-- Select sample --"
          },
          width = "100%"
        )
      )
    })
    
    tagList(pairing_inputs)
  })
  
  # Process button: Calculate mean and SEM
  observeEvent(input$contour_process, {
    req(contour_data$dfdt_raw)
    
    tryCatch({
      # Get raw data
      temperatures <- contour_data$dfdt_raw$temperatures
      sample_names <- contour_data$dfdt_raw$sample_names
      dfdt_values <- contour_data$dfdt_raw$values
      
      # Get unique sample names
      unique_samples <- unique(sample_names)
      contour_data$sample_names <- unique_samples
      contour_data$temperatures <- temperatures
      
      # Initialize results
      n_temps <- length(temperatures)
      n_samples <- length(unique_samples)
      
      mean_matrix <- matrix(NA, nrow = n_temps, ncol = n_samples)
      sem_matrix <- matrix(NA, nrow = n_temps, ncol = n_samples)
      n_matrix <- matrix(NA, nrow = n_temps, ncol = n_samples)
      
      colnames(mean_matrix) <- unique_samples
      colnames(sem_matrix) <- unique_samples
      colnames(n_matrix) <- unique_samples
      
      # Calculate mean and SEM for each unique sample
      for (i in seq_along(unique_samples)) {
        sample <- unique_samples[i]
        
        # Find all columns with this sample name
        sample_cols <- which(sample_names == sample)
        
        # Extract data for this sample (all replicates)
        sample_data <- dfdt_values[, sample_cols, drop = FALSE]
        
        # Calculate mean and SEM for each temperature
        for (j in 1:n_temps) {
          values <- as.numeric(sample_data[j, ])
          values <- values[!is.na(values)]  # Remove NAs
          
          if (length(values) > 0) {
            mean_matrix[j, i] <- mean(values)
            n_matrix[j, i] <- length(values)
            
            if (length(values) > 1) {
              # SEM = SD / sqrt(n)
              sem_matrix[j, i] <- sd(values) / sqrt(length(values))
            } else {
              sem_matrix[j, i] <- 0
            }
          }
        }
      }
      
      # Store processed data
      # Normalise each sample's mean dF/dT so max = 1 (same as CPM Peak Picker)
      norm_matrix <- apply(mean_matrix, 2, function(col) {
        mn  <- min(col, na.rm = TRUE)
        mx  <- max(col, na.rm = TRUE)
        rng <- mx - mn
        if (is.na(rng) || rng == 0) rep(0, length(col)) else (col - mn) / rng
      })
      colnames(norm_matrix) <- unique_samples
      
      contour_data$dfdt_processed <- list(
        temperatures = temperatures,
        sample_names = unique_samples,
        mean = mean_matrix,
        norm = norm_matrix,
        sem  = sem_matrix,
        n    = n_matrix
      )
      
      # Collect Tm pairing if Tm data is loaded
      if (!is.null(contour_data$tm_raw)) {
        tm_raw <- contour_data$tm_raw
        pairing <- character(tm_raw$n_rows)
        
        for (i in 1:tm_raw$n_rows) {
          input_val <- input[[paste0("contour_tm_pair_", i)]]
          if (!is.null(input_val) && input_val != "-- Select sample --") {
            pairing[i] <- input_val
          } else {
            pairing[i] <- NA
          }
        }
        
        contour_data$tm_pairing <- pairing
        
        # Create processed Tm data with pairing
        valid_pairs <- !is.na(pairing)
        
        if (any(valid_pairs)) {
          contour_data$tm_processed <- list(
            concentrations = tm_raw$concentrations[valid_pairs],
            sample_names = pairing[valid_pairs],
            means = tm_raw$means[valid_pairs],
            sems = tm_raw$sems[valid_pairs]
          )
        }
      }
      
      showNotification("✓ Mean and SEM calculated successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error", duration = 5)
    })
  })
  
  # Clear button
  observeEvent(input$contour_clear, {
    contour_data$dfdt_raw <- NULL
    contour_data$tm_raw <- NULL
    contour_data$dfdt_processed <- NULL
    contour_data$tm_processed <- NULL
    contour_data$sample_names <- NULL
    contour_data$temperatures <- NULL
    contour_data$tm_pairing <- NULL
    
    showNotification("✓ All data cleared", type = "message", duration = 2)
  })
  
  # Status indicators
  output$contour_dfdt_status <- renderUI({
    if (!is.null(contour_data$dfdt_raw)) {
      n_samples <- length(contour_data$dfdt_raw$sample_names)
      n_temps <- length(contour_data$dfdt_raw$temperatures)
      div(class = "status-pill ready",
        span(class = "dot"),
        sprintf("✓ Loaded: %d samples, %d temps", n_samples, n_temps)
      )
    }
  })
  
  output$contour_tm_status <- renderUI({
    if (!is.null(contour_data$tm_raw)) {
      n_conc <- length(contour_data$tm_raw$concentrations)
      div(class = "status-pill ready",
        span(class = "dot"),
        sprintf("✓ Loaded: %d concentrations", n_conc)
      )
    }
  })
  
  output$contour_sample_count <- renderUI({
    if (!is.null(contour_data$dfdt_processed)) {
      n_unique <- length(contour_data$sample_names)
      div(class = "status-pill ready",
        span(class = "dot"),
        sprintf("✓ Processed: %d unique samples", n_unique)
      )
    }
  })
  
  # Tm summary display
  output$contour_tm_summary <- renderUI({
    req(contour_data$tm_processed)
    
    tm <- contour_data$tm_processed
    
    summary_html <- lapply(seq_along(tm$sample_names), function(i) {
      div(style = "background: #1E2D45; padding: 10px; border-radius: 6px; margin-bottom: 10px;",
        div(style = "color: #00C2FF; font-weight: bold; margin-bottom: 5px;", 
          tm$sample_names[i]),
        div(style = "color: #7A8FAD; font-size: 0.85rem;",
          sprintf("Condition: %s", tm$concentrations[i])
        ),
        div(style = "color: #7A8FAD; font-size: 0.85rem;",
          sprintf("Tm: %.2f ± %.2f °C", tm$means[i], tm$sems[i])
        )
      )
    })
    
    tagList(summary_html)
  })
  
  # Statistics display
  output$contour_stats_ui <- renderUI({
    req(contour_data$dfdt_processed)
    
    processed <- contour_data$dfdt_processed
    
    stats_html <- lapply(seq_along(processed$sample_names), function(i) {
      sample <- processed$sample_names[i]
      n_reps <- processed$n[1, i]  # Number of replicates (same for all temps)
      
      # Find peak mean value
      peak_idx <- which.max(processed$mean[, i])
      peak_temp <- processed$temperatures[peak_idx]
      peak_value <- processed$mean[peak_idx, i]
      
      div(style = "background: #1E2D45; padding: 10px; border-radius: 6px; margin-bottom: 10px;",
        div(style = "color: #00C2FF; font-weight: bold; margin-bottom: 5px;", sample),
        div(style = "color: #7A8FAD; font-size: 0.85rem;",
          sprintf("Replicates: %d", n_reps)
        ),
        div(style = "color: #7A8FAD; font-size: 0.85rem;",
          sprintf("Peak: %.2f°C (%.4f)", peak_temp, peak_value)
        )
      )
    })
    
    tagList(stats_html)
  })
  
  # Heatmap visualization
  output$contour_heatmap <- renderPlot({
    req(contour_data$dfdt_processed)
    
    processed <- contour_data$dfdt_processed
    
    # Reshape normalized values to long format, applying threshold + rescale
    thr <- input$contour_threshold
    df_long <- data.frame(
      Temperature = rep(processed$temperatures, times = length(processed$sample_names)),
      Sample      = rep(processed$sample_names, each  = length(processed$temperatures)),
      Norm_dFdT   = .contour_thresh(as.vector(processed$norm), thr)
    )
    df_long$Sample <- factor(df_long$Sample, levels = processed$sample_names)
    
    # Colour palette (use inferno as default for heatmap regardless of Tm plot setting)
    pal_fn <- switch(input$contour_palette,
      grayscale = colorRampPalette(c("#FFFFFF", "#000000"))(100),
      inferno  = scales::viridis_pal(option = "inferno")(100),
      magma    = scales::viridis_pal(option = "magma")(100),
      viridis  = scales::viridis_pal(option = "viridis")(100),
      plasma   = scales::viridis_pal(option = "plasma")(100),
      RdBu     = rev(hcl.colors(100, "RdBu")),
      YlOrRd   = hcl.colors(100, "YlOrRd"),
      scales::viridis_pal(option = "inferno")(100)  # default
    )
    
    ggplot(df_long, aes(x = Sample, y = Temperature, fill = Norm_dFdT)) +
      geom_tile() +
      scale_fill_gradientn(
        colours = pal_fn,
        limits  = c(0, 1),
        name    = "Norm.\ndF/dT"
      ) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(
        x     = NULL,
        y     = "Temperature (\u00b0C)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#0B1623", colour = NA),
        panel.background = element_rect(fill = "#0B1623", colour = NA),
        plot.title       = element_text(colour = "#E8F0FE", face = "bold", size = 13),
        axis.text.x      = element_text(colour = "#7A8FAD", angle = 40, hjust = 1, size = 9),
        axis.text.y      = element_text(colour = "#7A8FAD", size = 9),
        axis.title.y     = element_text(colour = "#7A8FAD", size = 10),
        legend.background = element_rect(fill = "#0B1623", colour = NA),
        legend.text      = element_text(colour = "#7A8FAD", size = 8),
        legend.title     = element_text(colour = "#7A8FAD", size = 9),
        panel.grid       = element_blank(),
        panel.border     = element_rect(colour = "#7A8FAD", fill = NA, linewidth = 0.6)
      )
    
  }, bg = "#0B1623")
  
  # Combined dF/dT fingerprint + Tm scatter/line plot
  output$contour_tm_plot <- renderPlot({
    req(contour_data$tm_processed, contour_data$dfdt_processed)
    
    tm        <- contour_data$tm_processed
    processed <- contour_data$dfdt_processed
    
    # ---- X-axis positions ----
    conc_num <- suppressWarnings(as.numeric(tm$concentrations))
    conc_log <- ifelse(is.na(conc_num) | conc_num == 0, 0.001, conc_num)
    log_x    <- log10(conc_log)
    
    # Tile width: fill the space between points (85% of min gap, or 0.4 if single point)
    tile_w <- if (length(unique(log_x)) > 1) {
      min(diff(sort(unique(log_x)))) * 0.85
    } else { 0.4 }
    
    # ---- Build background tile data (full temperature range) ----
    y_lo      <- if (!is.na(input$contour_ymin)) input$contour_ymin else min(processed$temperatures)
    y_hi      <- if (!is.na(input$contour_ymax)) input$contour_ymax else max(processed$temperatures)
    temp_mask <- processed$temperatures >= y_lo & processed$temperatures <= y_hi
    temps_sub <- processed$temperatures[temp_mask]
    temp_step <- if (length(temps_sub) > 1) mean(diff(temps_sub)) else 1.0
    
    tile_rows <- lapply(seq_along(tm$concentrations), function(i) {
      sname <- tm$sample_names[i]
      sidx  <- which(processed$sample_names == sname)
      if (length(sidx) == 0) return(NULL)
      
      nv <- .contour_thresh(processed$norm[temp_mask, sidx[1]], input$contour_threshold)
      data.frame(
        log_x       = log_x[i],
        Temperature = temps_sub,
        norm_dFdT   = nv
      )
    })
    tile_df <- do.call(rbind, Filter(Negate(is.null), tile_rows))
    
    # ---- Colour palette for tiles ----
    pal_cols <- switch(input$contour_palette,
      grayscale = colorRampPalette(c("#0B1623", "#E8F0FE"))(256),
      inferno  = scales::viridis_pal(option = "inferno")(256),
      magma    = scales::viridis_pal(option = "magma")(256),
      viridis  = scales::viridis_pal(option = "viridis")(256),
      plasma   = scales::viridis_pal(option = "plasma")(256),
      RdBu     = rev(hcl.colors(256, "RdBu")),
      YlOrRd   = hcl.colors(256, "YlOrRd"),
      colorRampPalette(c("#0B1623", "#E8F0FE"))(256)
    )
    
    # ---- Tm scatter data ----
    df <- data.frame(
      log_x = log_x,
      Mean  = tm$means,
      SEM   = tm$sems,
      stringsAsFactors = FALSE
    )
    
    # Individual replicates
    rep_df <- NULL
    if (!is.null(contour_data$tm_raw)) {
      tm_raw  <- contour_data$tm_raw
      pairing <- contour_data$tm_pairing
      valid   <- !is.na(pairing) & pairing != ""
      raw_conc <- tm_raw$concentrations[valid]
      raw_vals <- tm_raw$values[valid]
      rows <- lapply(seq_along(raw_conc), function(i) {
        cn <- suppressWarnings(as.numeric(raw_conc[i]))
        cx <- if (is.na(cn) || cn == 0) 0.001 else cn
        data.frame(log_x = log10(cx), Tm = raw_vals[[i]])
      })
      rep_df <- do.call(rbind, rows)
    }
    
    # ---- x-axis breaks ----
    x_breaks <- seq(floor(min(log_x)), ceiling(max(log_x)))
    
    # ---- Build plot ----
    p <- ggplot()
    
    # Layer 1: dF/dT fingerprint columns (semi-transparent background)
    if (!is.null(tile_df) && nrow(tile_df) > 0) {
      p <- p + geom_tile(
        data = tile_df,
        aes(x = log_x, y = Temperature, fill = norm_dFdT),
        width  = tile_w,
        height = temp_step,
        alpha  = 0.65
      ) +
      scale_fill_gradientn(
        colours = pal_cols,
        limits  = c(0, 1),
        name    = "Norm.\ndF/dT"
      )
    }
    
    # Layer 2: individual replicate Tm points (faint)
    if (!is.null(rep_df) && nrow(rep_df) > 0) {
      p <- p + geom_point(
        data = rep_df,
        aes(x = log_x, y = Tm),
        colour = "#AECBFA", size = 1.8, alpha = 0.55, shape = 16,
        inherit.aes = FALSE
      )
    }
    
    # Layer 3: connecting line through means
    p <- p + geom_line(
      data = df,
      aes(x = log_x, y = Mean),
      colour = "#FFFFFF", linewidth = 0.9,
      inherit.aes = FALSE
    )
    
    # Layer 4: SEM error bars
    p <- p + geom_errorbar(
      data = df,
      aes(x = log_x, ymin = Mean - SEM, ymax = Mean + SEM),
      width = 0.06, colour = "#FFFFFF", linewidth = 0.9,
      inherit.aes = FALSE
    )
    
    # Layer 5: mean Tm points (open circles on top)
    p <- p + geom_point(
      data = df,
      aes(x = log_x, y = Mean),
      colour = "#FFFFFF", fill = "#0B1623",
      size = 3.8, shape = 21, stroke = 1.8,
      inherit.aes = FALSE
    )
    
    p <- p +
      scale_x_continuous(
        breaks = x_breaks,
        labels = as.character(x_breaks),
        expand = expansion(mult = 0.06)
      ) +
      scale_y_continuous(
        limits = c(y_lo, y_hi),
        breaks = pretty(c(y_lo, y_hi), n = 8),
        expand = c(0, 0)
      ) +
      labs(
        x     = "Log [Concentration (\u03bcM)]",
        y     = "Temperature (\u00b0C)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background   = element_rect(fill = "#0B1623", colour = NA),
        panel.background  = element_rect(fill = "#0B1623", colour = NA),
        plot.title        = element_text(colour = "#E8F0FE", face = "bold", size = 13),
        axis.title        = element_text(colour = "#7A8FAD", size = 11),
        axis.text         = element_text(colour = "#7A8FAD", size = 10),
        axis.line         = element_line(colour = "#3A4D63", linewidth = 0.6),
        panel.grid        = element_blank(),
        legend.background = element_rect(fill = "#0B1623", colour = NA),
        legend.text       = element_text(colour = "#7A8FAD", size = 8),
        legend.title      = element_text(colour = "#7A8FAD", size = 9)
      )
    
    p
    
  }, bg = "#0B1623")
  
  # Export statistics CSV
  output$contour_export_stats <- downloadHandler(
    filename = function() {
      paste0("cpm_contour_statistics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(contour_data$dfdt_processed)
      
      processed <- contour_data$dfdt_processed
      
      # --- dF/dT mean & SEM table ---
      export_df <- data.frame(Temperature = processed$temperatures)
      for (i in seq_along(processed$sample_names)) {
        sample <- processed$sample_names[i]
        export_df[[paste0(sample, "_Mean")]] <- processed$mean[, i]
        export_df[[paste0(sample, "_SEM")]]  <- processed$sem[,  i]
      }
      
      # Write dF/dT block
      write.csv(export_df, file, row.names = FALSE)
      
      # Append Tm data if available (separator + second table)
      if (!is.null(contour_data$tm_processed)) {
        tm    <- contour_data$tm_processed
        tm_df <- data.frame(
          Condition = tm$concentrations,
          Sample    = tm$sample_names,
          Tm_Mean   = round(tm$means, 3),
          Tm_SEM    = round(tm$sems,  3)
        )
        # Append a blank row then the Tm table
        write("", file, append = TRUE)
        write("# Tm Summary", file, append = TRUE)
        write.table(tm_df, file, sep = ",", row.names = FALSE, append = TRUE)
      }
      
      showNotification("✓ Statistics exported!", type = "message", duration = 3)
    }
  )
  
  # Helper: build palette colours from current selection
  .contour_pal <- function(n = 256) {
    switch(isolate(input$contour_palette),
      grayscale = colorRampPalette(c("#FFFFFF", "#000000"))(n),
      inferno   = scales::viridis_pal(option = "inferno")(n),
      magma     = scales::viridis_pal(option = "magma")(n),
      viridis   = scales::viridis_pal(option = "viridis")(n),
      plasma    = scales::viridis_pal(option = "plasma")(n),
      RdBu      = rev(hcl.colors(n, "RdBu")),
      YlOrRd    = hcl.colors(n, "YlOrRd"),
      colorRampPalette(c("#FFFFFF", "#000000"))(n)  # default
    )
  }

  # Helper: floor values below threshold to 0, then rescale [thr,1] → [0,1]
  # so the full palette is always used for above-threshold intensities.
  .contour_thresh <- function(v, thr = 0) {
    if (is.null(thr) || thr <= 0) return(v)
    ifelse(v < thr, 0, (v - thr) / (1 - thr))
  }

  # Export heatmap PNG
  output$contour_export_heatmap <- downloadHandler(
    filename = function() paste0("cpm_heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(contour_data$dfdt_processed)
      processed <- contour_data$dfdt_processed
      pal <- .contour_pal()
      thr <- isolate(input$contour_threshold)

      df_long <- data.frame(
        Temperature = rep(processed$temperatures, times = length(processed$sample_names)),
        Sample      = rep(processed$sample_names, each  = length(processed$temperatures)),
        Norm_dFdT   = .contour_thresh(as.vector(processed$norm), thr)
      )
      df_long$Sample <- factor(df_long$Sample, levels = processed$sample_names)

      p <- ggplot(df_long, aes(x = Sample, y = Temperature, fill = Norm_dFdT)) +
        geom_tile() +
        scale_fill_gradientn(colours = pal, limits = c(0, 1), name = "Norm.\ndF/dT") +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = NULL, y = "Temperature (\u00b0C)") +
        theme_minimal(base_size = 12) +
        theme(
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title       = element_text(face = "bold", size = 14),
          axis.text.x      = element_text(angle = 40, hjust = 1, size = 10),
          axis.text.y      = element_text(size = 10),
          axis.title.y     = element_text(size = 11),
          panel.grid       = element_blank(),
          panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6)
        )

      ggsave(file, p, width = 10, height = 7, dpi = 300, bg = "white")
      showNotification("\u2713 Heatmap exported!", type = "message", duration = 3)
    }
  )

  # Export Tm plot PNG
  output$contour_export_tmplot <- downloadHandler(
    filename = function() paste0("cpm_tmplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(contour_data$tm_processed, contour_data$dfdt_processed)
      processed <- contour_data$dfdt_processed
      tm        <- contour_data$tm_processed
      pal       <- .contour_pal()

      conc_num  <- suppressWarnings(as.numeric(tm$concentrations))
      conc_log  <- ifelse(is.na(conc_num) | conc_num == 0, 0.001, conc_num)
      log_x     <- log10(conc_log)
      x_breaks  <- seq(floor(min(log_x)), ceiling(max(log_x)))
      tile_w    <- if (length(unique(log_x)) > 1) min(diff(sort(unique(log_x)))) * 0.85 else 0.4

      y_lo      <- if (!is.na(isolate(input$contour_ymin))) isolate(input$contour_ymin) else min(processed$temperatures)
      y_hi      <- if (!is.na(isolate(input$contour_ymax))) isolate(input$contour_ymax) else max(processed$temperatures)
      temp_mask <- processed$temperatures >= y_lo & processed$temperatures <= y_hi
      temps_sub <- processed$temperatures[temp_mask]
      temp_step <- if (length(temps_sub) > 1) mean(diff(temps_sub)) else 1.0

      tile_rows <- lapply(seq_along(tm$concentrations), function(i) {
        sname <- tm$sample_names[i]
        sidx  <- which(processed$sample_names == sname)
        if (length(sidx) == 0) return(NULL)
        nv <- .contour_thresh(processed$norm[temp_mask, sidx[1]], isolate(input$contour_threshold))
        data.frame(log_x = log_x[i], Temperature = temps_sub, norm_dFdT = nv)
      })
      tile_df <- do.call(rbind, Filter(Negate(is.null), tile_rows))
      df_tm   <- data.frame(log_x = log_x, Mean = tm$means, SEM = tm$sems)

      p <- ggplot() +
        geom_tile(data = tile_df,
                  aes(x = log_x, y = Temperature, fill = norm_dFdT),
                  width = tile_w, height = temp_step) +
        scale_fill_gradientn(colours = pal, limits = c(0, 1), name = "Norm.\ndF/dT") +
        geom_line(data = df_tm, aes(x = log_x, y = Mean),
                  colour = "black", linewidth = 0.9, inherit.aes = FALSE) +
        geom_errorbar(data = df_tm, aes(x = log_x, ymin = Mean - SEM, ymax = Mean + SEM),
                      width = 0.06, colour = "black", linewidth = 0.9, inherit.aes = FALSE) +
        geom_point(data = df_tm, aes(x = log_x, y = Mean),
                   colour = "black", fill = "white",
                   size = 3.8, shape = 21, stroke = 1.8, inherit.aes = FALSE) +
        scale_x_continuous(breaks = x_breaks, labels = as.character(x_breaks),
                           expand = expansion(mult = 0.06)) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = pretty(c(y_lo, y_hi), n = 8), expand = c(0, 0)) +
        labs(x = "Log [Concentration (\u03bcM)]",
             y = "Temperature (\u00b0C)") +
        theme_minimal(base_size = 12) +
        theme(
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          axis.title       = element_text(size = 11),
          axis.text        = element_text(size = 10),
          axis.line        = element_line(colour = "grey40", linewidth = 0.6),
          panel.grid       = element_blank(),
          legend.title     = element_text(size = 10),
          legend.text      = element_text(size = 9)
        )

      ggsave(file, p, width = 10, height = 7, dpi = 300, bg = "white")
      showNotification("\u2713 Tm plot exported!", type = "message", duration = 3)
    }
  )
  
  # END CPM CONTOUR PLOTTING SERVER LOGIC
  # ==========================================================================


  # ── WESTERN BLOT SERVER ──────────────────────────────────────────────────────

  output$gel_western_loaded <- reactive({ !is.null(gel_data$western_image) })
  outputOptions(output, "gel_western_loaded", suspendWhenHidden=FALSE)



  observeEvent(input$gel_western, {
    req(input$gel_western)
    tryCatch({
      img <- image_read(input$gel_western$datapath)
      gel_data$western_raw            <- img
      gel_data$western_image          <- img
      gel_data$western_original       <- img
      gel_data$western_crop_active    <- FALSE
      gel_data$western_crop_corners   <- list()
      gel_data$western_ladder_markers <- data.frame(x=numeric(),y=numeric(),
                                                     mw=numeric(),stringsAsFactors=FALSE)
      gel_data$western_pending_click  <- NULL
      gel_data$western_plot_trigger   <- 0
      showNotification("\u2713 Western blot loaded", type="message", duration=2)
    }, error=function(e)
      showNotification(paste("Error:", e$message), type="error"))
  })

  observeEvent(input$gel_western_clear, {
    gel_data$western_image          <- NULL
    gel_data$western_raw            <- NULL
    gel_data$western_crop_active    <- FALSE
    gel_data$western_crop_corners   <- list()
    gel_data$western_ladder_markers <- data.frame(x=numeric(),y=numeric(),
                                                   mw=numeric(),stringsAsFactors=FALSE)
    gel_data$western_pending_click  <- NULL
    gel_data$western_plot_trigger   <- 0
    shinyjs::reset("gel_western")
    showNotification("Western removed", type="message", duration=2)
  })

  output$gel_western_status <- renderUI({
    req(gel_data$western_image)
    info <- image_info(gel_data$western_image)
    div(class="status-pill ready", div(class="dot"),
        sprintf("\u2713 %d \u00d7 %d px", info$width, info$height))
  })



  observeEvent(input$gel_western_click, {
    req(gel_data$western_image)
    cl   <- input$gel_western_click
    mode <- if (!is.null(input$gel_western_mode)) input$gel_western_mode else "crop"
    if (mode == "crop" && isTRUE(gel_data$western_crop_active)) {
      corners <- gel_data$western_crop_corners
      corners[[length(corners)+1]] <- list(x=cl$x, y=cl$y)
      gel_data$western_crop_corners <- corners
      if (length(corners) >= 2) gel_data$western_crop_active <- FALSE
    } else if (mode == "ladder") {
      gel_data$western_ladder_markers <- rbind(gel_data$western_ladder_markers,
        data.frame(x=cl$x, y=cl$y, mw=0, stringsAsFactors=FALSE))
      gel_data$western_plot_trigger <- gel_data$western_plot_trigger + 1
    }
  })



  output$gel_western_ladder_entry <- renderUI({ NULL })

  output$gel_western_ladder_list <- renderUI({
    gel_data$western_plot_trigger
    markers <- isolate(gel_data$western_ladder_markers)
    if (nrow(markers) == 0)
      return(p("No bands marked yet.", style="color:#7A8FAD;font-size:0.9rem;"))
    rows <- lapply(seq_len(nrow(markers)), function(i) {
      div(style="background:#1E2D45;padding:8px;border-radius:4px;margin-bottom:6px;",
        div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;",
          span(paste("Band",i), style="color:#7A8FAD;font-size:0.85rem;"),
          tags$button("\u00d7",
            style="background:#FF5C5C;color:white;border:none;padding:2px 8px;border-radius:3px;cursor:pointer;",
            onclick=sprintf("Shiny.setInputValue('gel_western_rm',%d,{priority:'event'})",i))),
        numericInput(paste0("gel_wmw_",i), NULL, value=if(markers$mw[i]==0) NA else markers$mw[i],
                     min=0, step=1, width="100%")
      )
    })
    div(style="max-height:300px;overflow-y:auto;", tagList(rows))
  })

  observeEvent(input$gel_western_rm, {
    idx <- input$gel_western_rm
    m   <- gel_data$western_ladder_markers
    if (idx >= 1 && idx <= nrow(m)) {
      gel_data$western_ladder_markers <- m[-idx,,drop=FALSE]
      gel_data$western_plot_trigger   <- gel_data$western_plot_trigger + 1
    }
  })

  observeEvent(input$gel_western_clear_bands, {
    gel_data$western_ladder_markers <- data.frame(x=numeric(),y=numeric(),
                                                   mw=numeric(),stringsAsFactors=FALSE)
    gel_data$western_plot_trigger <- gel_data$western_plot_trigger + 1
    showNotification("Bands cleared", type="message", duration=2)
  })

  .get_western_processed <- function(for_export=FALSE) {
    img <- gel_data$western_image
    if (is.null(img)) return(NULL)
    cv  <- if (for_export) isolate(input$gel_western_contrast) else input$gel_western_contrast
    if (!is.null(cv) && !is.na(cv) && cv > 0) {
      img <- image_normalize(img)
      for (k in seq_len(min(as.integer(cv), 15)))
        img <- image_contrast(img, sharpen=FALSE)
    }
    img
  }

  output$gel_western_preview <- renderPlot({
    req(gel_data$western_image)
    gel_data$western_plot_trigger
    img  <- .get_western_processed()
    rast <- as.raster(img)
    info <- image_info(img)
    par(mar=c(0,0,0,0), bg="black")
    plot(NA, xlim=c(0,info$width), ylim=c(0,info$height),
         asp=1, xlab="", ylab="", axes=FALSE)
    rasterImage(rast, 0, 0, info$width, info$height)
    for (cr in gel_data$western_crop_corners)
      points(cr$x, cr$y, pch=3, col="#00C2FF", cex=2.5, lwd=2)
    m <- isolate(gel_data$western_ladder_markers)
    if (nrow(m) > 0) for (i in seq_len(nrow(m))) {
      points(m$x[i], m$y[i], pch=16, col="#FF7B47", cex=1.2)
      if (!is.na(m$mw[i]) && m$mw[i] > 0)
        text(m$x[i], m$y[i], paste0(m$mw[i]," kDa"),
             pos=3, col="#FF7B47", cex=0.75, font=2)
    }
    if (!is.null(gel_data$western_pending_click))
      points(gel_data$western_pending_click$x, gel_data$western_pending_click$y,
             pch=16, col="#00C2FF", cex=2)
  }, bg="black")

  .gel_stack_western <- function(gel_png_path) {
    west_src <- .get_western_processed(for_export = TRUE)
    if (is.null(west_src)) return(gel_png_path)

    gap_px  <- max(0L, as.integer(isolate(input$gel_gap_px)))
    gap_col <- isolate(input$gel_gap_color)
    bg      <- if (!is.null(gap_col) && gap_col != "none") gap_col else "none"

    gel_img <- image_read(gel_png_path)
    gel_w   <- as.integer(image_info(gel_img)$width)

    # Match gel's left padding so western content aligns with gel content
    gel_lad  <- isolate(gel_data$ladder_markers)
    lad_off  <- if (!is.null(input$gel_ladder_offset)) isolate(input$gel_ladder_offset) else 60L
    fsize    <- if (!is.null(input$gel_fontsize)) isolate(input$gel_fontsize) else 20L
    left_pad <- if (nrow(gel_lad) > 0) max(as.integer(lad_off + 20L), round(fsize * 6)) else 0L
    west_w   <- max(1L, gel_w - left_pad)   # western content width

    src_w <- as.integer(image_info(west_src)$width)
    src_h <- as.integer(image_info(west_src)$height)
    new_h <- max(1L, round(src_h * west_w / src_w))   # proportional height

    # Read current MW values directly from numericInputs
    w_marks <- isolate(gel_data$western_ladder_markers)
    if (nrow(w_marks) > 0) {
      for (i in seq_len(nrow(w_marks))) {
        v <- isolate(input[[paste0("gel_wmw_", i)]])
        if (!is.null(v) && !is.na(v) && v > 0) w_marks$mw[i] <- v
      }
    }

    # Antibody label column width (horizontal text needs room)
    ab       <- trimws(isolate(input$gel_western_antibody))
    ab_col_w <- if (nchar(ab) > 0) max(round(fsize * 4L), round(nchar(ab) * round(fsize * 0.65))) else 0L
    canvas_w <- gel_w + ab_col_w

    bg_col   <- if (!is.null(input$gel_bg_color) && input$gel_bg_color == "white") "white" else NA
    tmp_west <- tempfile(fileext = ".png")

    png(tmp_west, width = canvas_w, height = new_h,
        units = "px", bg = bg_col, res = 96)
    par(mar = c(0, 0, 0, 0))

    # Plot in canvas pixel space: x 0->canvas_w, y 0=bottom->new_h=top
    plot(1, type = "n", xlim = c(0, canvas_w), ylim = c(0, new_h),
         xlab = "", ylab = "", axes = FALSE)

    # Draw western image in content area (right of left_pad) — same layout as gel
    rasterImage(as.raster(west_src), left_pad, 0, left_pad + west_w, new_h)

    # Ladder labels — IDENTICAL style to gel export
    if (nrow(w_marks) > 0) {
      for (i in seq_len(nrow(w_marks))) {
        mw_val <- w_marks$mw[i]
        if (!is.na(mw_val) && mw_val > 0) {
          # Scale click y (0=bottom in src_h space) to export y (0=bottom in new_h space)
          y_pos   <- w_marks$y[i] / src_h * new_h
          x_label <- left_pad - 10
          text(x_label, y_pos, paste0(mw_val, " kDa"),
               pos = 2, cex = isolate(input$gel_fontsize) / 12,
               font = if (isTRUE(isolate(input$gel_bold))) 2 else 1)
          segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
        }
      }
    }

    # Antibody label — horizontal text centred in the right column
    if (nchar(ab) > 0) {
      text(gel_w + 8, new_h / 2,
           ab, adj = c(0, 0.5),
           cex = isolate(input$gel_fontsize) / 12,
           font = if (isTRUE(isolate(input$gel_bold))) 2 else 1,
           col = "black", srt = 0)
    }

    dev.off()

    # Pad gel image to same total width as western canvas (for clean stacking)
    west_img <- image_read(tmp_west)
    if (ab_col_w > 0) {
      pad     <- image_blank(ab_col_w, image_info(gel_img)$height, color = bg)
      gel_img <- image_append(c(gel_img, pad), stack = FALSE)
    }

    gap <- image_blank(canvas_w, max(1L, gap_px), color = bg)
    out <- image_append(c(gel_img, gap, west_img), stack = TRUE)
    image_write(out, path = gel_png_path, format = "png")
    gel_png_path
  }






  # Stitch download handlers (use unique IDs gel_stitch_png / gel_stitch_tiff)
  output$gel_stitch_png <- downloadHandler(
    filename = function() {
      paste0("gel_western_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(gel_data$image)
      
      # Use isolate to read markers
      ladder_markers <- isolate(gel_data$ladder_markers)
      well_markers <- isolate(gel_data$well_markers)
      
      # Use SAME scaling as preview (75%)
      scale_factor <- 0.75
      img_width <- gel_data$image_width * scale_factor
      img_height <- gel_data$image_height * scale_factor
      
      ladder_offset <- input$gel_ladder_offset
      well_offset <- input$gel_well_offset
      
      left_pad <- if (nrow(ladder_markers) > 0) max(ladder_offset + 20, round(input$gel_fontsize * 6)) else 0
      
      # Calculate top padding dynamically - SAME as preview
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        font_size <- input$gel_fontsize
        
        # Find longest label
        max_label_len <- max(nchar(well_markers$label))
        
        # Calculate extra space needed based on angle
        if (text_angle == 45) {
          # Diagonal: text extends upward at 45°
          char_width <- font_size * 0.6
          diagonal_extension <- max_label_len * char_width * sin(pi/4)
          top_pad <- well_offset + 20 + diagonal_extension
        } else if (text_angle == 90) {
          # Vertical: text goes straight up
          char_width <- font_size * 0.6
          top_pad <- well_offset + 20 + (max_label_len * char_width * 0.5)
        } else {
          # Horizontal: standard padding
          top_pad <- well_offset + 20
        }
      } else {
        top_pad <- 0
      }
      
      total_width <- img_width + left_pad
      total_height <- img_height + top_pad
      
      # Get background color preference
      bg_color <- if (input$gel_bg_color == "white") "white" else NA
      
      # Transparent or white background
      png(file, width = total_width, height = total_height,
          units = "px", bg = bg_color, res = 96)
      
      img_raster <- as.raster(gel_data$image)
      
      par(mar = c(0, 0, 0, 0))
      
      plot(1, type = "n", xlim = c(0, total_width), ylim = c(0, total_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      
      # Draw white rectangle if white background selected
      if (input$gel_bg_color == "white") {
        rect(0, 0, total_width, total_height, col = "white", border = NA)
      }
      
      rasterImage(img_raster, left_pad, 0, left_pad + img_width, img_height)
      
      # Ladder labels (match preview exactly)
      if (nrow(ladder_markers) > 0) {
        for (i in 1:nrow(ladder_markers)) {
          marker <- ladder_markers[i, ]
          y_pos <- marker$y * scale_factor  # Scale position!
          x_label <- left_pad - 10
          
          text(x_label, y_pos, paste0(marker$mw, " kDa"),
               pos = 2, cex = input$gel_fontsize / 12,
               font = if (input$gel_bold) 2 else 1)
          
          segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
        }
      }
      
      # Well labels with rotation (match preview exactly)
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        
        for (i in 1:nrow(well_markers)) {
          marker <- well_markers[i, ]
          x_pos <- marker$x * scale_factor + left_pad  # Scale position!
          y_label <- img_height + 10
          
          # Adjust positioning based on angle
          if (text_angle == 0) {
            # Horizontal
            text(x_pos, y_label, marker$label,
                 pos = 3, cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 0)
          } else if (text_angle == 90) {
            # Vertical
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0.5), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 90)
          } else {
            # Diagonal (45 degrees)
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 45)
          }
          
          segments(x_pos, y_label - 5, x_pos, img_height, lwd = 1)
        }
      }
      
      dev.off()
      
      tryCatch(
        .gel_stack_western(file),
        error = function(e) showNotification(
          paste("Export error:", e$message), type="error", duration=5)
      )
      showNotification("✓ Gel+Western exported!", type = "message", duration = 3)
    }
  )

    output$gel_stitch_tiff <- downloadHandler(
    filename = function() {
      paste0("gel_western_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")
    },
    content = function(file) {
      req(gel_data$image)
      
      ladder_markers <- isolate(gel_data$ladder_markers)
      well_markers   <- isolate(gel_data$well_markers)
      
      scale_factor <- 0.75
      img_width    <- gel_data$image_width  * scale_factor
      img_height   <- gel_data$image_height * scale_factor
      
      ladder_offset <- input$gel_ladder_offset
      well_offset   <- input$gel_well_offset
      left_pad      <- if (nrow(ladder_markers) > 0) ladder_offset + 20 else 0
      
      if (nrow(well_markers) > 0) {
        text_angle    <- as.numeric(input$gel_text_angle)
        font_size     <- input$gel_fontsize
        max_label_len <- max(nchar(well_markers$label))
        if (text_angle == 45) {
          char_width <- font_size * 0.6
          top_pad    <- well_offset + 20 + max_label_len * char_width * sin(pi / 4)
        } else if (text_angle == 90) {
          char_width <- font_size * 0.6
          top_pad    <- well_offset + 20 + (max_label_len * char_width * 0.5)
        } else {
          top_pad <- well_offset + 20
        }
      } else {
        top_pad <- 0
      }
      
      total_width  <- img_width  + left_pad
      total_height <- img_height + top_pad
      bg_color     <- if (input$gel_bg_color == "white") "white" else "transparent"
      
      # TIFF at 300 dpi for publication quality
      tiff(file,
           width      = total_width,
           height     = total_height,
           units      = "px",
           bg         = bg_color,
           res        = 300,
           compression = "lzw")
      
      img_raster <- as.raster(gel_data$image)
      par(mar = c(0, 0, 0, 0))
      plot(1, type = "n", xlim = c(0, total_width), ylim = c(0, total_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      
      if (input$gel_bg_color == "white")
        rect(0, 0, total_width, total_height, col = "white", border = NA)
      
      rasterImage(img_raster, left_pad, 0, left_pad + img_width, img_height)
      
      if (nrow(ladder_markers) > 0) {
        for (i in seq_len(nrow(ladder_markers))) {
          marker <- ladder_markers[i, ]
          y_pos  <- marker$y * scale_factor
          x_label <- left_pad - 10
          text(x_label, y_pos, paste0(marker$mw, " kDa"),
               pos = 2, cex = input$gel_fontsize / 12,
               font = if (input$gel_bold) 2 else 1)
          segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
        }
      }
      
      if (nrow(well_markers) > 0) {
        text_angle <- as.numeric(input$gel_text_angle)
        for (i in seq_len(nrow(well_markers))) {
          marker  <- well_markers[i, ]
          x_pos   <- marker$x * scale_factor + left_pad
          y_label <- img_height + 10
          if (text_angle == 0) {
            text(x_pos, y_label, marker$label,
                 pos = 3, cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 0)
          } else if (text_angle == 90) {
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0.5), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 90)
          } else {
            text(x_pos, y_label, marker$label,
                 adj = c(0, 0), cex = input$gel_fontsize / 12,
                 font = if (input$gel_bold) 2 else 1, srt = 45)
          }
          segments(x_pos, y_label - 5, x_pos, img_height, lwd = 1)
        }
      }
      
      dev.off()

      # Stack western blot below gel
      tryCatch({
        wt <- .get_western_processed(for_export=TRUE)
        if (!is.null(wt)) {
          gi  <- image_read(file)
          gw  <- as.integer(image_info(gi)$width)
          sw  <- as.integer(image_info(wt)$width)
          sh  <- as.integer(image_info(wt)$height)
          nh  <- max(1L, round(sh * gw / sw))
          wt  <- image_resize(wt, sprintf("%dx%d!", gw, nh))
          gc2 <- isolate(input$gel_gap_color)
          bg2 <- if (!is.null(gc2) && gc2!="none") gc2 else "none"
          gp  <- image_blank(gw, max(1L, as.integer(isolate(input$gel_gap_px))), color=bg2)
          image_write(image_append(c(gi, gp, wt), stack=TRUE), path=file,
                      format="tiff", options=c("compression=lzw"))
        }
      }, error=function(e)
        showNotification(paste("TIFF stitch error:", e$message), type="error", duration=5))
      showNotification("\u2713 TIFF exported successfully!", type = "message", duration = 3)
    }
  )

  # Gel crop status
  output$gel_crop_status <- renderUI({
    if (!is.null(gel_data$gel_crop_pending)) {
      b <- gel_data$gel_crop_pending
      div(style="color:#00C2FF;font-size:0.8rem;margin-top:0.5rem;",
          sprintf("~%d x %d px selected -- click Apply to confirm",
                  round(abs(b$xmax-b$xmin)), round(abs(b$ymax-b$ymin))))
    } else if (isTRUE(gel_data$cropped)) {
      div(style="color:#7A8FAD;font-size:0.8rem;margin-top:0.5rem;", "Image is cropped.")
    }
  })

  observeEvent(input$gel_crop_brush, {
    req(gel_data$image)
    gel_data$gel_crop_pending <- input$gel_crop_brush
  })

  observeEvent(input$gel_apply_crop, {
    req(gel_data$image, gel_data$gel_crop_pending)
    b  <- gel_data$gel_crop_pending
    iw <- gel_data$image_width; ih <- gel_data$image_height
    sf <- 0.75
    lad_off  <- if (!is.null(input$gel_ladder_offset)) input$gel_ladder_offset else 60
    left_pad <- if (nrow(gel_data$ladder_markers) > 0) lad_off + 20L else 0L
    x_min <- max(0L, round((b$xmin - left_pad) / sf))
    x_max <- min(iw,  round((b$xmax - left_pad) / sf))
    y_min_img <- max(0L, round(ih - b$ymax / sf))
    y_max_img <- min(ih,  round(ih - b$ymin / sf))
    cw <- max(0L, x_max - x_min); ch <- max(0L, y_max_img - y_min_img)
    if (cw < 10 || ch < 10) {
      showNotification("Selection too small.", type="warning")
      gel_data$gel_crop_pending <- NULL; return()
    }
    tryCatch({
      gel_data$image        <- image_crop(gel_data$image, geometry_area(cw, ch, x_min, y_min_img))
      gel_data$image_width  <- cw
      gel_data$image_height <- ch
      gel_data$cropped      <- TRUE
      gel_data$gel_crop_pending <- NULL
      gel_data$ladder_markers <- data.frame(x=numeric(),y=numeric(),mw=numeric(),
                                             id=character(),stringsAsFactors=FALSE)
      gel_data$well_markers   <- data.frame(x=numeric(),y=numeric(),label=character(),
                                             id=character(),stringsAsFactors=FALSE)
      showNotification(sprintf("Cropped to %d x %d px",cw,ch), type="message", duration=2)
    }, error=function(e) showNotification(paste("Crop error:",e$message), type="error"))
  })

  observeEvent(input$gel_revert_crop, {
    req(gel_data$image_original)
    info <- image_info(gel_data$image_original)
    gel_data$image        <- gel_data$image_original
    gel_data$image_width  <- info$width
    gel_data$image_height <- info$height
    gel_data$cropped      <- FALSE
    gel_data$gel_crop_pending <- NULL
    gel_data$ladder_markers <- data.frame(x=numeric(),y=numeric(),mw=numeric(),
                                           id=character(),stringsAsFactors=FALSE)
    gel_data$well_markers   <- data.frame(x=numeric(),y=numeric(),label=character(),
                                           id=character(),stringsAsFactors=FALSE)
    showNotification("Reverted to original image", type="message", duration=2)
  })

  # Western crop
  output$gel_western_crop_ready <- reactive({ FALSE })
  outputOptions(output, "gel_western_crop_ready", suspendWhenHidden=FALSE)

  observeEvent(input$gel_western_crop_brush, {
    req(gel_data$western_image)
    if (isTRUE(input$gel_western_mode == "crop"))
      gel_data$western_crop_pending <- input$gel_western_crop_brush
  })

  observeEvent(input$gel_western_crop_apply, {
    req(gel_data$western_image)
    b <- gel_data$western_crop_pending
    if (is.null(b)) {
      showNotification("Switch to Crop mode and drag a selection first.", type="warning"); return()
    }
    tryCatch({
      info <- image_info(gel_data$western_image)
      iw <- info$width; ih <- info$height
      x_min <- max(0L, round(min(b$xmin,b$xmax)))
      x_max <- min(iw,  round(max(b$xmin,b$xmax)))
      y_min_img <- max(0L, round(ih - max(b$ymin,b$ymax)))
      y_max_img <- min(ih,  round(ih - min(b$ymin,b$ymax)))
      cw <- x_max - x_min; ch <- y_max_img - y_min_img
      if (cw < 10 || ch < 10) { showNotification("Selection too small.", type="warning"); return() }
      gel_data$western_image        <- image_crop(gel_data$western_image,
                                                   geometry_area(cw, ch, x_min, y_min_img))
      gel_data$western_crop_pending <- NULL
      gel_data$western_ladder_markers <- data.frame(x=numeric(),y=numeric(),
                                                      mw=numeric(),stringsAsFactors=FALSE)
      gel_data$western_plot_trigger <- gel_data$western_plot_trigger + 1
      showNotification(sprintf("Cropped to %d x %d px",cw,ch), type="message", duration=2)
    }, error=function(e) showNotification(paste("Crop error:",e$message), type="error"))
  })

  observeEvent(input$gel_western_revert_crop, {
    req(gel_data$western_original)
    gel_data$western_image          <- gel_data$western_original
    gel_data$western_crop_pending   <- NULL
    gel_data$western_ladder_markers <- data.frame(x=numeric(),y=numeric(),
                                                   mw=numeric(),stringsAsFactors=FALSE)
    gel_data$western_plot_trigger <- gel_data$western_plot_trigger + 1
    showNotification("Reverted to original western", type="message", duration=2)
  })

  observeEvent(input$nav, {
    if (input$nav %in% c("BCA", "CPM", "CPMQC", "CPMCONTOUR", "UCP1", "AKTA", "GEL")) nav_select("nav", input$nav)
  })


  
  # -- Raw Sample Data --------------------------------------------------------
  ucp1_raw_data <- reactiveVal(NULL)
  
  observeEvent(input$ucp1_raw_files, ignoreNULL = TRUE, ignoreInit = TRUE, {
    req(input$ucp1_raw_files)
    req(!ucp1_clearing())  # Don't process during clear
    ucp1_raw_data(NULL)
    
    tryCatch({
      files <- input$ucp1_raw_files
      n_files <- nrow(files)
      
      # Parse all files
      all_data <- list()
      
      for (i in 1:n_files) {
        # Read CSV
        lines <- readLines(files$datapath[i], warn = FALSE)
        data_lines <- lines[3:length(lines)]
        
        parsed <- strsplit(data_lines, ",")
        time_vals <- sapply(parsed, function(x) as.numeric(x[1]))
        intensity_vals <- sapply(parsed, function(x) as.numeric(x[2]))
        
        df <- data.frame(Time = time_vals, Intensity = intensity_vals)
        df <- na.omit(df)
        
        # Hard cap to include last data point at 140.1000061 seconds
        df <- df[df$Time >= 0.1 & df$Time <= 140.11, ]
        
        # Extract sample name from filename
        # Text after final underscore, before .csv
        fname <- files$name[i]
        fname_no_ext <- sub("\\.csv$", "", fname, ignore.case = TRUE)
        
        # Split by underscore and take last part
        parts <- strsplit(fname_no_ext, "_")[[1]]
        sample_name <- parts[length(parts)]
        
        all_data[[i]] <- list(
          name = sample_name,
          data = df
        )
      }
      
      # Combine into wide format table
      # Start with time column from first file
      time_col <- all_data[[1]]$data$Time
      
      # Create combined data frame
      combined <- data.frame(Time = time_col)
      
      for (i in 1:length(all_data)) {
        sample_df <- all_data[[i]]$data
        sample_name <- all_data[[i]]$name
        
        # Match times (in case they differ slightly)
        matched_intensities <- rep(NA, length(time_col))
        for (j in 1:length(time_col)) {
          # Find closest time match
          time_diff <- abs(sample_df$Time - time_col[j])
          closest_idx <- which.min(time_diff)
          if (time_diff[closest_idx] < 0.05) {  # Within 0.05s tolerance
            matched_intensities[j] <- sample_df$Intensity[closest_idx]
          }
        }
        
        combined[[sample_name]] <- matched_intensities
      }
      
      ucp1_raw_data(combined)
      
    }, error = function(e) {
      ucp1_raw_data(list(error = conditionMessage(e)))
    })
  })
  
  output$ucp1_raw_status <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_raw_data())
    d <- ucp1_raw_data()
    
    if (!is.null(d$error)) {
      div(class = "status-pill error", div(class = "dot"), 
          paste("Error:", d$error))
    } else {
      n_samples <- ncol(d) - 1  # Minus time column
      n_points <- nrow(d)
      time_range <- range(d$Time, na.rm = TRUE)
      
      div(
        div(class = "status-pill ready", div(class = "dot"),
            sprintf("%d samples loaded", n_samples)),
        div(class = "status-pill ready", div(class = "dot"),
            sprintf("%d data points per sample (%.1f - %.1f s)", 
                    n_points, time_range[1], time_range[2]))
      )
    }
  })
  
  output$ucp1_raw_table <- renderTable({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_raw_data())
    d <- isolate(ucp1_raw_data())  # isolate to prevent excessive re-renders
    if (!is.null(d$error)) return(NULL)
    
    # Round for display
    d_display <- d
    d_display$Time <- round(d_display$Time, 1)
    for (col in names(d_display)[-1]) {
      d_display[[col]] <- round(d_display[[col]], 2)
    }
    
    d_display
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%", align = "c", digits = 2)
  
  output$ucp1_raw_download <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_raw_data())
    d <- ucp1_raw_data()
    if (!is.null(d$error)) return(NULL)
    
    downloadButton("ucp1_raw_dl_csv", "↓ CSV for GraphPad Prism", class = "btn-download")
  })
  
  output$ucp1_raw_dl_csv <- downloadHandler(
    filename = function() paste0("UCP1_raw_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(ucp1_raw_data())
      d <- ucp1_raw_data()
      d$Time <- round(d$Time, 1)
      write.csv(d, file, row.names = FALSE)
    }
  )
  
  # -- Processed Data (Raw → [H+] using proton calibration) ------------------
  ucp1_processed_data <- reactive({
    req(!ucp1_clearing())  # Skip if currently clearing
    req(ucp1_raw_data())
    req(ucp1_calibration())
    
    raw <- ucp1_raw_data()
    if (!is.null(raw$error)) return(NULL)
    
    calib <- ucp1_calibration()
    slope <- calib$slope
    intercept <- calib$intercept
    
    # Apply formula: ((1/raw_intensity) - intercept) / slope
    processed <- raw
    
    # Process all columns except Time
    for (col in names(raw)[-1]) {
      processed[[col]] <- ((1 / raw[[col]]) - intercept) / slope
    }
    
    processed
  })
  
  output$ucp1_processed_status <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    proc <- ucp1_processed_data()
    
    if (is.null(proc)) {
      div(style = "color: #7A8FAD; font-size: 0.9em; padding: 0.5rem;",
          "Upload raw data and complete proton calibration to generate processed data")
    } else {
      n_samples <- ncol(proc) - 1
      div(
        div(class = "status-pill ready", div(class = "dot"),
            sprintf("%d samples processed", n_samples)),
        div(class = "status-pill ready", div(class = "dot"),
            sprintf("Converted to [H+] (mM) using proton calibration"))
      )
    }
  })
  
  output$ucp1_processed_table <- renderTable({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_processed_data())
    proc <- isolate(ucp1_processed_data())  # isolate to prevent excessive re-renders
    
    # Round for display
    proc_display <- proc
    proc_display$Time <- round(proc_display$Time, 1)
    for (col in names(proc_display)[-1]) {
      proc_display[[col]] <- round(proc_display[[col]], 4)
    }
    
    proc_display
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%", align = "c", digits = 4)
  
  output$ucp1_processed_download <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_processed_data())
    downloadButton("ucp1_processed_dl_csv", "↓ CSV Processed Data", class = "btn-download")
  })
  
  output$ucp1_processed_dl_csv <- downloadHandler(
    filename = function() paste0("UCP1_processed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(ucp1_processed_data())
      proc <- ucp1_processed_data()
      proc$Time <- round(proc$Time, 1)
      write.csv(proc, file, row.names = FALSE)
    }
  )
  
  # -- Export Complete Excel Analysis -----------------------------------------
  ucp1_export_ready <- reactive({
    # Check if all required data is available
    list(
      proton_cal = !is.null(ucp1_calibration()),
      capacity_cal = !is.null(ucp1_cap_calibration()),
      raw_data = !is.null(ucp1_raw_data()),
      processed_data = !is.null(ucp1_processed_data())
    )
  })
  
  output$ucp1_export_status <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    ready <- ucp1_export_ready()
    
    all_ready <- all(unlist(ready))
    
    if (all_ready) {
      div(
        div(class = "status-pill ready", div(class = "dot"),
            "✓ All data available - ready to export"),
        div(style = "color: #7A8FAD; font-size: 0.9em; margin-top: 0.5rem;",
            "Complete Excel analysis file with all sheets populated")
      )
    } else {
      missing <- names(ready)[!unlist(ready)]
      div(
        div(class = "status-pill warning", div(class = "dot"),
            sprintf("Missing: %s", paste(gsub("_", " ", missing), collapse = ", "))),
        div(style = "color: #7A8FAD; font-size: 0.9em; margin-top: 0.5rem;",
            "Complete all steps above to export analysis file")
      )
    }
  })
  
  output$ucp1_export_button <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    ready <- ucp1_export_ready()
    if (!all(unlist(ready))) return(NULL)
    
    downloadButton("ucp1_export_xlsx", "↓ Download Complete Analysis (.xlsx)", 
                   class = "btn-download", 
                   style = "font-size: 1.1em; padding: 0.8rem 1.5rem;")
  })
  
  output$ucp1_export_xlsx <- downloadHandler(
    filename = function() {
      paste0("UCP1_Analysis_Complete_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(ucp1_cal_data(), ucp1_calibration(), 
          ucp1_cap_calibration(), ucp1_cap_summary(),
          ucp1_raw_data(), ucp1_processed_data())
      
      # Load required package - openxlsx for native Excel chart support
      if (!require(openxlsx, quietly = TRUE)) {
        install.packages("openxlsx", repos = "https://cloud.r-project.org/")
        library(openxlsx)
      }
      
      # Create new workbook
      wb <- createWorkbook()
      
      # Add sheets
      addWorksheet(wb, "Raw data")
      addWorksheet(wb, "Processed data")
      addWorksheet(wb, "Proton calibration")
      addWorksheet(wb, "Capacity calibration")
      addWorksheet(wb, "Rates")
      
      # Write raw data (format time to 1 decimal place)
      raw <- ucp1_raw_data()
      raw_export <- raw
      raw_export$Time <- round(raw_export$Time, 1)
      writeData(wb, "Raw data", raw_export, startRow = 1, startCol = 1)
      
      # Write processed data (format time to 1 decimal place)
      proc <- ucp1_processed_data()
      proc_export <- proc
      proc_export$Time <- round(proc_export$Time, 1)
      writeData(wb, "Processed data", proc_export, startRow = 1, startCol = 1)
      
      # Write proton calibration data - formatted like Excel template
      proton_cal <- ucp1_calibration()
      proton_plateaus <- ucp1_plateaus()$plateau_summary
      proton_raw <- ucp1_cal_data()
      
      # Columns A-B: Raw time and intensity data
      writeData(wb, "Proton calibration", "Time (s)", startRow = 1, startCol = 1)
      writeData(wb, "Proton calibration", "Intensity (a.u.)", startRow = 1, startCol = 2)
      writeData(wb, "Proton calibration", round(proton_raw$Time, 1), startRow = 2, startCol = 1)
      writeData(wb, "Proton calibration", round(proton_raw$Intensity, 4), startRow = 2, startCol = 2)
      
      # FU/proton table starting at column M
      # Row 5: "FU/proton" header
      writeData(wb, "Proton calibration", "FU/proton", startRow = 5, startCol = 13)
      
      # Row 6: "Relative change in conc (mM)"
      writeData(wb, "Proton calibration", "Relative change in conc (mM)", startRow = 6, startCol = 14)
      
      # Row 7: Table headers
      writeData(wb, "Proton calibration", "Addition", startRow = 7, startCol = 13)
      writeData(wb, "Proton calibration", "H+ stock", startRow = 7, startCol = 14)
      writeData(wb, "Proton calibration", "[H+]", startRow = 7, startCol = 15)
      writeData(wb, "Proton calibration", "FU", startRow = 7, startCol = 16)
      writeData(wb, "Proton calibration", "1/FU", startRow = 7, startCol = 17)
      
      # Row 7 units
      writeData(wb, "Proton calibration", "(µL)", startRow = 8, startCol = 13)
      writeData(wb, "Proton calibration", "(mM)", startRow = 8, startCol = 14)
      writeData(wb, "Proton calibration", "(mM)", startRow = 8, startCol = 15)
      
      # Data rows starting at row 9
      # Addition column: 0, 1, 1, 1, 1, 1...
      n_plateaus <- length(proton_cal$fu_values)
      addition_col <- c(0, rep(1, n_plateaus - 1))
      writeData(wb, "Proton calibration", addition_col, startRow = 9, startCol = 13)
      
      # H+ stock column: 0, 2000, 2000, 2000...
      hstock_col <- c(0, rep(2000, n_plateaus - 1))
      writeData(wb, "Proton calibration", hstock_col, startRow = 9, startCol = 14)
      
      # [H+] column: 0, 4, 8, 12, 16, 20...
      h_conc <- proton_cal$data$H_conc
      writeData(wb, "Proton calibration", h_conc, startRow = 9, startCol = 15)
      
      # FU column (O8 onwards): plateau median intensities
      fu_vals <- proton_cal$fu_values
      writeData(wb, "Proton calibration", fu_vals, startRow = 9, startCol = 16)
      
      # 1/FU column
      inv_fu <- proton_cal$data$inv_FU
      writeData(wb, "Proton calibration", inv_fu, startRow = 9, startCol = 17)
      
      # Addition note (after data rows)
      last_data_row <- 9 + n_plateaus
      writeData(wb, "Proton calibration", "Addition:", startRow = last_data_row + 2, startCol = 13)
      writeData(wb, "Proton calibration", "1µL nigericin (0.5mM, startRow = 1, startCol = 1)", startRow = last_data_row + 2, startCol = 14)
      
      writeData(wb, "Proton calibration", "H+ stock:", startRow = last_data_row + 3, startCol = 13)
      writeData(wb, "Proton calibration", "1M H2SO4", startRow = last_data_row + 3, startCol = 14)
      
      # Slope and intercept at N25 and O25
      writeData(wb, "Proton calibration", "a", startRow = 25, startCol = 14)
      writeData(wb, "Proton calibration", proton_cal$slope, startRow = 25, startCol = 15)
      
      writeData(wb, "Proton calibration", "b", startRow = 26, startCol = 14)
      writeData(wb, "Proton calibration", proton_cal$intercept, startRow = 26, startCol = 15)
      
      writeData(wb, "Proton calibration", "y=ax+b", startRow = 25, startCol = 16)
      writeData(wb, "Proton calibration", "x=(y-b)/a", startRow = 26, startCol = 16)
      
      # Write capacity calibration data - formatted like manual template
      cap_cal <- ucp1_cap_calibration()
      cap_plateaus <- ucp1_cap_plateaus()$plateau_summary
      cap_summary <- ucp1_cap_summary()
      cap_raw <- ucp1_cap_data()  # Get capacity trace data
      
      # ========================================================================
      # TIME AND INTENSITY TRACE DATA (Columns A-B)
      # ========================================================================
      
      # Column headers (matching template: A1 and B2)
      writeData(wb, "Capacity calibration", "Time (s)", startRow = 1, startCol = 1)
      writeData(wb, "Capacity calibration", "Intensity (a.u.)", startRow = 2, startCol = 2)
      
      # Trace data (starting Row 3)
      writeData(wb, "Capacity calibration", round(cap_raw$Time, 1), startRow = 3, startCol = 1)
      writeData(wb, "Capacity calibration", round(cap_raw$Intensity, 4), startRow = 3, startCol = 2)
      
      # ========================================================================
      # INTERNAL VOLUME TABLE (Starting at Column L)
      # ========================================================================
      
      # Row 2: "Internal volume" header
      writeData(wb, "Capacity calibration", "Internal volume", startRow = 2, startCol = 12)
      
      # Row 5: Column headers
      writeData(wb, "Capacity calibration", "Addition", startRow = 5, startCol = 12)
      writeData(wb, "Capacity calibration", "SPQ", startRow = 5, startCol = 13)
      writeData(wb, "Capacity calibration", "Total", startRow = 5, startCol = 14)
      writeData(wb, "Capacity calibration", "FU", startRow = 5, startCol = 15)
      
      # Row 6: Units
      writeData(wb, "Capacity calibration", "(µL)", startRow = 6, startCol = 12)
      writeData(wb, "Capacity calibration", "(µM)", startRow = 6, startCol = 13)
      writeData(wb, "Capacity calibration", "(µM)", startRow = 6, startCol = 14)
      
      # Row 8 onwards: Data rows
      # First row: baseline (0, 0, 0, initial_FU)
      n_plateaus_cap <- cap_cal$n_used
      fu_vals_cap <- cap_plateaus$median_intensity[1:n_plateaus_cap]
      
      writeData(wb, "Capacity calibration", 0, startRow = 8, startCol = 12)  # Addition
      writeData(wb, "Capacity calibration", 0, startRow = 8, startCol = 13)  # SPQ
      writeData(wb, "Capacity calibration", 0, startRow = 8, startCol = 14)  # Total
      writeData(wb, "Capacity calibration", fu_vals_cap[1], startRow = 8, startCol = 15)  # Initial FU
      
      # Subsequent rows: (1, 1, cumulative_total, FU)
      for (i in 2:n_plateaus_cap) {
        row_num <- 8 + i - 1
        writeData(wb, "Capacity calibration", 1, startRow = row_num, startCol = 12)  # Addition
        writeData(wb, "Capacity calibration", 1, startRow = row_num, startCol = 13)  # SPQ
        writeData(wb, "Capacity calibration", i - 1, startRow = row_num, startCol = 14)  # Total (cumulative)
        writeData(wb, "Capacity calibration", fu_vals_cap[i], startRow = row_num, startCol = 15)  # FU
      }
      
      # Row after data: Addition note (row 21 or dynamic based on data)
      note_row <- 8 + n_plateaus_cap + 2
      writeData(wb, "Capacity calibration", "Addition:", startRow = note_row, startCol = 12)
      writeData(wb, "Capacity calibration", "1µL SPQ (0.5mM)", startRow = note_row, startCol = 13)
      
      # ========================================================================
      # CALIBRATION PARAMETERS
      # ========================================================================
      
      # Row 24: Parameter labels (or dynamic based on data)
      param_row <- note_row + 3
      writeData(wb, "Capacity calibration", "a", startRow = param_row, startCol = 13)
      writeData(wb, "Capacity calibration", "b", startRow = param_row, startCol = 14)
      writeData(wb, "Capacity calibration", "y=ax+b", startRow = param_row, startCol = 15)
      
      # Row 25: Parameter values
      writeData(wb, "Capacity calibration", cap_cal$slope, startRow = param_row + 1, startCol = 13)
      writeData(wb, "Capacity calibration", cap_cal$intercept, startRow = param_row + 1, startCol = 14)
      writeData(wb, "Capacity calibration", "x=(y-b)/a", startRow = param_row + 1, startCol = 15)
      
      # Row 26: "internal vol" label
      writeData(wb, "Capacity calibration", "internal vol", startRow = param_row + 2, startCol = 15)
      
      # ========================================================================
      # RESULTS SECTION
      # ========================================================================
      
      # Row 27: Results headers
      results_header_row <- param_row + 3
      writeData(wb, "Capacity calibration", "FU/µM", startRow = results_header_row, startCol = 12)
      writeData(wb, "Capacity calibration", "Initial FU", startRow = results_header_row, startCol = 13)
      writeData(wb, "Capacity calibration", "µM", startRow = results_header_row, startCol = 14)
      writeData(wb, "Capacity calibration", "µL/75 µL sample", startRow = results_header_row, startCol = 15)
      
      # Row 29: Results values
      results_row <- results_header_row + 2
      
      # Extract values from cap_summary
      fu_per_um <- cap_summary$Value[cap_summary$Parameter == "FU/µM"]
      initial_fu <- fu_vals_cap[1]
      um_value <- cap_summary$Value[cap_summary$Parameter == "µM"]
      ul_per_75ul <- cap_summary$Value[cap_summary$Parameter == "µL/75 µL sample"]
      
      writeData(wb, "Capacity calibration", fu_per_um, startRow = results_row, startCol = 12)
      writeData(wb, "Capacity calibration", initial_fu, startRow = results_row, startCol = 13)
      writeData(wb, "Capacity calibration", um_value, startRow = results_row, startCol = 14)
      writeData(wb, "Capacity calibration", ul_per_75ul, startRow = results_row, startCol = 15)
      
      # Row 30: Reaction volume note
      writeData(wb, "Capacity calibration", "(reaction vol 500 µL)", startRow = results_row + 1, startCol = 15)
      
      # ========================================================================
      # RATES SHEET - CRITICAL FOR WORKFLOW
      # ========================================================================
      
      # Row 1: Sample header
      writeData(wb, "Rates", "Sample", startRow = 1, startCol = 1)
      
      # Row 2: Date header
      writeData(wb, "Rates", "Date", startRow = 2, startCol = 1)
      
      # Row 3: Internal volume (formula reference to Capacity calibration)
      writeData(wb, "Rates", "Internal volume", startRow = 3, startCol = 1)
      writeFormula(wb, "Rates", "='Capacity calibration'!O24", startRow = 3, startCol = 2)
      
      # Row 4: Assumed protein (formula)
      writeData(wb, "Rates", "Assumed protein (µg)", startRow = 4, startCol = 1)
      writeFormula(wb, "Rates", "=20/(1400/75)", startRow = 4, startCol = 2)
      
      # Row 6: Liposome additions header
      writeData(wb, "Rates", "Liposome additions", startRow = 6, startCol = 1)
      
      # Row 6: Sample names (formulas pulling from Raw data sheet row 1)
      # Get number of samples from raw data
      n_samples <- ncol(raw_export) - 1  # Subtract 1 for Time column
      
      for (i in 1:n_samples) {
        col_num <- i + 1  # B, C, D, etc.
        col_letter <- LETTERS[col_num]
        formula <- paste0("='Raw data'!", col_letter, "1")
        writeFormula(wb, "Rates", formula, startRow = 6, startCol = col_num)
      }
      
      # Row 7: Plateau (EMPTY - user will paste from Prism)
      writeData(wb, "Rates", "Plateau", startRow = 7, startCol = 1)
      
      # Row 8: Top (EMPTY - user will paste from Prism)
      writeData(wb, "Rates", "Top", startRow = 8, startCol = 1)
      
      # Row 9: K (EMPTY - user will paste from Prism)
      writeData(wb, "Rates", "K", startRow = 9, startCol = 1)
      
      # Row 10: Span (FORMULA: Top - Plateau)
      writeData(wb, "Rates", "Span", startRow = 10, startCol = 1)
      for (i in 1:n_samples) {
        col_num <- i + 1
        col_letter <- LETTERS[col_num]
        formula <- paste0("=", col_letter, "8-", col_letter, "7")
        writeFormula(wb, "Rates", formula, startRow = 10, startCol = col_num)
      }
      
      # Row 12: mM [H+]/s (FORMULA: K * Span) - THIS IS THE KEY OUTPUT
      writeData(wb, "Rates", "mM [H+]/s", startRow = 12, startCol = 1)
      for (i in 1:n_samples) {
        col_num <- i + 1
        col_letter <- LETTERS[col_num]
        formula <- paste0("=", col_letter, "9*", col_letter, "10")
        writeFormula(wb, "Rates", formula, startRow = 12, startCol = col_num)
      }
      
      # Row 13: µmol H+/s (FORMULA: mM[H+]/s * 1000 * (Internal vol / 1000000))
      writeData(wb, "Rates", "µmol H+/s", startRow = 13, startCol = 1)
      for (i in 1:n_samples) {
        col_num <- i + 1
        col_letter <- LETTERS[col_num]
        formula <- paste0("=", col_letter, "12*1000*($B$3/1000000)")
        writeFormula(wb, "Rates", formula, startRow = 13, startCol = col_num)
      }
      
      # Row 14: µmol H+/min (FORMULA: µmol H+/s * 60)
      writeData(wb, "Rates", "µmol H+/min", startRow = 14, startCol = 1)
      for (i in 1:n_samples) {
        col_num <- i + 1
        col_letter <- LETTERS[col_num]
        formula <- paste0("=", col_letter, "13*60")
        writeFormula(wb, "Rates", formula, startRow = 14, startCol = col_num)
      }
      
      # Row 15: µmol H+/min/mg (FORMULA: µmol H+/min / (protein / 1000))
      writeData(wb, "Rates", "µmol H+/min/mg", startRow = 15, startCol = 1)
      for (i in 1:n_samples) {
        col_num <- i + 1
        col_letter <- LETTERS[col_num]
        formula <- paste0("=", col_letter, "14/($B$4/1000)")
        writeFormula(wb, "Rates", formula, startRow = 15, startCol = col_num)
      }
      
      # ========================================================================
      # ADD PNG CHARTS TO CALIBRATION SHEETS
      # ========================================================================
      
      # Create temporary directory for plot images
      temp_dir <- tempdir()
      
      # Chart 1: Proton Calibration Trace (Time vs Intensity)
      chart1_file <- file.path(temp_dir, "proton_trace_chart.png")
      png(chart1_file, width = 800, height = 400, res = 100)
      plot(proton_raw$Time, proton_raw$Intensity,
           type = "l",
           col = "#3B82F6",
           lwd = 2,
           main = "Proton Calibration Trace",
           xlab = "Time (s)",
           ylab = "Intensity (a.u.)",
           las = 1,
           cex.main = 1.2,
           cex.lab = 1.1)
      grid(col = "gray80", lty = 2)
      dev.off()
      
      # Chart 2: Proton Calibration Curve ([H+] vs 1/FU with trendline)
      chart2_file <- file.path(temp_dir, "proton_calib_chart.png")
      png(chart2_file, width = 600, height = 500, res = 100)
      par(mar = c(5, 5, 4, 2))
      
      # Plot data points
      plot(h_conc, inv_fu,
           pch = 19,
           col = "#3B82F6",
           cex = 1.5,
           main = "FU/Proton Calibration Curve",
           xlab = "[H+] (mM)",
           ylab = "1/FU",
           las = 1,
           cex.main = 1.2,
           cex.lab = 1.1)
      grid(col = "gray80", lty = 2)
      
      # Add trendline
      abline(a = proton_cal$intercept, b = proton_cal$slope, 
             col = "#EF4444", lwd = 2, lty = 2)
      
      # Add equation and R²
      eq_text <- sprintf("y = %.6fx + %.5f", proton_cal$slope, proton_cal$intercept)
      r2_text <- sprintf("R² = %.4f", proton_cal$r_squared)
      
      # Position text in upper left corner
      legend("topleft", 
             legend = c(eq_text, r2_text),
             bty = "n",
             cex = 1.1,
             text.col = "#1F2937")
      
      dev.off()
      
      # Chart 3: Capacity Calibration Trace (Time vs Intensity)
      chart3_file <- file.path(temp_dir, "capacity_trace_chart.png")
      png(chart3_file, width = 800, height = 400, res = 100)
      plot(cap_raw$Time, cap_raw$Intensity,
           type = "l",
           col = "#10B981",
           lwd = 2,
           main = "Capacity Calibration Trace",
           xlab = "Time (s)",
           ylab = "Intensity (a.u.)",
           las = 1,
           cex.main = 1.2,
           cex.lab = 1.1)
      grid(col = "gray80", lty = 2)
      dev.off()
      
      # Chart 4: Capacity Calibration Curve (Total SPQ vs FU with trendline)
      chart4_file <- file.path(temp_dir, "capacity_calib_chart.png")
      png(chart4_file, width = 600, height = 500, res = 100)
      par(mar = c(5, 5, 4, 2))
      
      # Get Total SPQ values
      total_spq <- cap_cal$data$Total_SPQ
      
      # Plot data points
      plot(total_spq, fu_vals_cap,
           pch = 19,
           col = "#10B981",
           cex = 1.5,
           main = "Capacity Calibration Curve",
           xlab = "Total SPQ (µM)",
           ylab = "FU",
           las = 1,
           cex.main = 1.2,
           cex.lab = 1.1)
      grid(col = "gray80", lty = 2)
      
      # Add trendline
      abline(a = cap_cal$intercept, b = cap_cal$slope,
             col = "#EF4444", lwd = 2, lty = 2)
      
      # Add equation and R²
      cap_eq_text <- sprintf("y = %.6fx + %.5f", cap_cal$slope, cap_cal$intercept)
      cap_r2_text <- sprintf("R² = %.4f", cap_cal$r_squared)
      
      # Position text in upper left corner
      legend("topleft",
             legend = c(cap_eq_text, cap_r2_text),
             bty = "n",
             cex = 1.1,
             text.col = "#1F2937")
      
      dev.off()
      
      # Insert charts into workbook
      insertImage(wb, "Proton calibration", chart1_file, 
                  startRow = 1, startCol = 3,
                  width = 6, height = 3, units = "in")
      
      insertImage(wb, "Proton calibration", chart2_file,
                  startRow = 7, startCol = 19,
                  width = 5, height = 4.5, units = "in")
      
      insertImage(wb, "Capacity calibration", chart3_file,
                  startRow = 1, startCol = 3,
                  width = 6, height = 3, units = "in")
      
      insertImage(wb, "Capacity calibration", chart4_file,
                  startRow = 7, startCol = 17,
                  width = 5, height = 4.5, units = "in")
      
      # ========================================================================
      # SAVE WORKBOOK - READY FOR USE
      # ========================================================================
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      # Clean up temp PNG files AFTER save
      unlink(chart1_file)
      unlink(chart2_file)
      unlink(chart3_file)
      unlink(chart4_file)
      
      message("✓ Excel file created successfully with calibration charts!")
      message("")
      message("📋 Your analysis includes:")
      message("  ✓ All raw and processed data")
      message("  ✓ Proton calibration with 2 charts (trace + curve with equation)")
      message("  ✓ Capacity calibration with 2 charts (trace + curve with equation)")  
      message("  ✓ Rates sheet with all formulas pre-populated")
      message("")
      message("📊 To create the mM [H+]/s bar chart (30 seconds):")
      message("  1. Open Rates sheet")
      message("  2. Paste Plateau/Top/K values from Prism into rows 7-9")
      message("  3. Select row 12 (mM [H+]/s values)")
      message("  4. Click: Insert → Column Chart")
      message("  5. Done! Professional results!")
      message("")
      message("✨ Time saved: 90%+ vs manual Excel entry!")
      
    }
  )

  # ==========================================================================
  # BCA
  # ==========================================================================
  bca_data    <- reactiveVal(NULL)
  bca_results <- reactiveVal(NULL)
  bca_history <- reactiveVal(list())

  # Clear all BCA data
  observeEvent(input$bca_clear, {
    bca_data(NULL)
    bca_results(NULL)
    bca_history(list())
    shinyjs::reset("bca_file")
    shinyjs::reset("bca_mode")
    shinyjs::reset("bca_manual_conc")
    shinyjs::reset("bca_volume")
    shinyjs::reset("bca_title")
    shinyjs::reset("bca_digits")
    showNotification("BCA data cleared", type = "message", duration = 2)
  })

  observeEvent(input$bca_file, {
    req(input$bca_file)
    bca_results(NULL)
    tryCatch({
      fp       <- input$bca_file$datapath
      raw_data <- read_softmax_bca(fp)
      if (is.null(raw_data$groups) || nrow(raw_data$groups) == 0)
        stop("No group data found. Ensure this is a valid SoftMax Pro export.")
      bca_data(list(filepath = fp, raw = raw_data))
    }, error = function(e) {
      bca_data(list(error = conditionMessage(e)))
    })
  })

  output$bca_file_status <- renderUI({
    req(bca_data())
    d <- bca_data()
    if (!is.null(d$error)) {
      div(
        div(class = "status-pill error", div(class = "dot"), "Error loading file"),
        div(style = "color: #FF5C5C; font-size: 0.85em; margin-top: 0.5rem; padding: 0.5rem; background: rgba(255,92,92,0.1); border-radius: 4px;",
            paste("Details:", d$error))
      )
    } else {
      div(class = "status-pill ready", div(class = "dot"), "File loaded")
    }
  })

  output$bca_curve_placeholder <- renderUI({
    if (is.null(bca_results()))
      div(class = "plot-placeholder", div(class = "icon", "📈"), "Run analysis to see standard curve")
  })

  observeEvent(input$bca_run, {
    req(bca_data())
    d <- bca_data()
    if (!is.null(d$error)) { showNotification(d$error, type = "error"); return() }
    if (input$bca_mode == "manual" && (is.na(input$bca_manual_conc) || input$bca_manual_conc <= 0)) {
      showNotification("Please enter a valid manual concentration.", type = "warning"); return()
    }
    withProgress(message = "Analysing BCA…", value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Fitting standard curve…")
        std_curve <- create_standard_curve(d$raw)

        incProgress(0.4, detail = "Calculating yield…")
        manual_conc <- if (input$bca_mode == "manual") input$bca_manual_conc else NULL
        res <- calculate_protein_yield(
          file_path            = d$filepath,
          std_curve            = std_curve,
          volume_ml            = input$bca_volume,
          digits               = input$bca_digits,
          title                = input$bca_title,
          manual_concentration = manual_conc
        )
        res_row   <- as.data.frame(res$data)[1, ]
        conc_val  <- as.numeric(res_row[[1]])
        vol_val   <- as.numeric(res_row[[2]])
        yield_val <- as.numeric(res_row[[3]])

        incProgress(0.3, detail = "Rendering…")
        bca_results(list(curve = std_curve, conc = conc_val, vol = vol_val, yield = yield_val, gt = res$gt))

        # Add to history
        entry <- list(
          time     = format(Sys.time(), "%H:%M:%S"),
          file     = input$bca_file$name,
          conc     = conc_val,
          yield    = yield_val,
          r2       = std_curve$r2,
          mode     = input$bca_mode
        )
        bca_history(c(bca_history(), list(entry)))

      }, error = function(e) {
        showNotification(paste("Analysis error:", conditionMessage(e)), type = "error", duration = 10)
      })
    })
  })

  output$bca_curve_plot <- renderPlot({
    req(bca_results())
    std       <- bca_results()$curve
    stds_data <- std$standards
    x_pred <- seq(min(stds_data$conc), max(stds_data$conc), length.out = 100)
    y_pred <- predict(std$model, newdata = data.frame(conc = x_pred))
    pred_df <- data.frame(conc = x_pred, mean_signal = y_pred)

    ggplot(stds_data, aes(x = conc, y = mean_signal)) +
      geom_ribbon(data = pred_df, aes(
        ymin = mean_signal - 0.02 * diff(range(stds_data$mean_signal, na.rm = TRUE)),
        ymax = mean_signal + 0.02 * diff(range(stds_data$mean_signal, na.rm = TRUE))),
        fill = "#00C2FF", alpha = 0.10) +
      geom_line(data = pred_df, colour = "#00C2FF", linewidth = 1.2) +
      geom_errorbar(aes(ymin = mean_signal - sd_signal, ymax = mean_signal + sd_signal),
        colour = "#7A8FAD", width = 0.015, linewidth = 0.7, na.rm = TRUE) +
      geom_point(colour = "#FF7B47", size = 4, shape = 21, fill = "#FF7B47", stroke = 0) +
      annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.4,
        label = sprintf("R^2 = %.4f", std$r2), colour = "#7A8FAD", size = 4, family = "mono") +
      labs(title = "BCA Standard Curve", x = "BSA Concentration (mg/mL)", y = "Mean Absorbance") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#0F1623", colour = NA),
        panel.background = element_rect(fill = "#0F1623", colour = NA),
        panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.4),
        panel.grid.minor = element_line(colour = "#161E2E", linewidth = 0.2),
        text             = element_text(colour = "#E8F0FE"),
        axis.text        = element_text(colour = "#7A8FAD"),
        plot.title       = element_text(face = "bold", size = 14, colour = "#E8F0FE")
      )
  }, bg = "#0F1623")

  output$bca_result_badges <- renderUI({
    req(bca_results())
    r <- bca_results()
    fluidRow(
      column(4, div(class = "result-badge",
        div(class = "result-label", "Concentration"),
        div(class = "result-value", sprintf("%.2f mg/mL", r$conc)))),
      column(4, div(class = "result-badge",
        div(class = "result-label", "Total Yield"),
        div(class = "result-value green", sprintf("%.2f mg", r$yield)))),
      column(4, div(class = "result-badge",
        div(class = "result-label", "R\u00b2"),
        div(class = "result-value orange", sprintf("%.4f", r$curve$r2))))
    )
  })

  output$bca_results_table <- DT::renderDT({
    req(bca_results())
    r   <- bca_results()
    fmt <- paste0("%.", input$bca_digits, "f")
    df  <- data.frame(
      Parameter = c("Concentration (mg/mL)", "Volume (mL)", "Total Yield (mg)", "R\u00b2"),
      Value     = c(sprintf(fmt, r$conc), sprintf(fmt, r$vol),
                    sprintf(fmt, r$yield), sprintf("%.4f", r$curve$r2))
    )
    datatable(df, options = list(dom = "t", ordering = FALSE), rownames = FALSE, class = "compact")
  })

  output$bca_download_buttons <- renderUI({
    req(bca_results())
    tagList(
      downloadButton("bca_dl_png", "\u2193 PNG Standard Curve", class = "btn-download"), " ",
      downloadButton("bca_dl_tbl", "\u2193 PNG Results Table",  class = "btn-download"), " ",
      downloadButton("bca_dl_csv", "\u2193 CSV Results",        class = "btn-download")
    )
  })

  output$bca_dl_png <- downloadHandler(
    filename = function() paste0("BCA_standard_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(file) {
      req(bca_results())
      # Re-theme standard curve for white publication background
      p <- bca_results()$curve$plot +
        theme(plot.background  = element_rect(fill = "white", colour = NA),
              panel.background = element_rect(fill = "white", colour = NA),
              text             = element_text(colour = "black"),
              axis.text        = element_text(colour = "black"),
              panel.grid.major = element_line(colour = "grey90"),
              panel.grid.minor = element_line(colour = "grey95"),
              axis.line        = element_line(colour = "black"),
              plot.title       = element_text(colour = "black"))
      ggsave(file, p, width = 10, height = 6, dpi = 300, bg = "white")
    }
  )

  output$bca_dl_tbl <- downloadHandler(
    filename = function() paste0("BCA_results_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(file) {
      req(bca_results())
      r     <- bca_results()
      title <- if (nchar(trimws(input$bca_title)) > 0) trimws(input$bca_title) else "BCA Assay Protein Yield Summary"

      params <- c("Concentration (mg/mL)",
                  "Sample volume (mL)",
                  "Total yield (mg)",
                  "R\u00b2 (standard curve)")
      vals   <- c(sprintf("%.2f",  r$conc),
                  sprintf("%.2f",  r$vol),
                  sprintf("%.2f",  r$yield),
                  sprintf("%.4f",  r$curve$r2))

      n      <- length(params)
      # y positions: rows evenly spaced, header above
      row_h  <- 1.0   # every row (header and data) occupies exactly this height
      # Row positions bottom-up: 0 = bottom data row, n-1 = top data row, n = header row
      ys     <- seq(n - 1, 0, by = -1) * row_h   # data row baselines
      y_hdr  <- n * row_h                          # header row baseline
      y_rule <- y_hdr + row_h                      # thick rule (between title and header)
      y_ttl  <- y_rule + 0.35                      # title text

      # Everything uses the same offsets: text at baseline+0.65, separator at baseline
      text_top <- 0.65

      df_rows <- data.frame(y = ys, param = params, val = vals)

      p <- ggplot() +
        # ---- title ----
        annotate("text", x = 0, y = y_ttl,
                 label = title, hjust = 0, vjust = 0,
                 size = 4.5, fontface = "bold", colour = "black", family = "sans") +
        # ---- thick rule (under title, above header) ----
        annotate("segment", x = 0, xend = 1, y = y_rule, yend = y_rule,
                 colour = "black", linewidth = 0.9) +
        # ---- header labels ----
        annotate("text", x = 0, y = y_hdr + text_top,
                 label = "PARAMETER", hjust = 0, vjust = 1,
                 size = 2.9, colour = "grey45", family = "sans") +
        annotate("text", x = 1, y = y_hdr + text_top,
                 label = "VALUE", hjust = 1, vjust = 1,
                 size = 2.9, colour = "grey45", family = "sans") +
        # ---- thin rule below header (at header baseline) ----
        annotate("segment", x = 0, xend = 1, y = y_hdr, yend = y_hdr,
                 colour = "grey65", linewidth = 0.4) +
        # ---- data rows: text at top of band, separator at baseline ----
        geom_text(data = df_rows, aes(x = 0, y = y + text_top, label = param),
                  hjust = 0, vjust = 1, size = 3.3,
                  colour = "black", family = "sans") +
        geom_text(data = df_rows, aes(x = 1, y = y + text_top, label = val),
                  hjust = 1, vjust = 1, size = 3.3, fontface = "bold",
                  colour = "black", family = "mono") +
        geom_segment(data = df_rows, aes(x = 0, xend = 1, y = y, yend = y),
                     colour = "grey88", linewidth = 0.35) +
        # ---- tight axis limits ----
        scale_x_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
        scale_y_continuous(limits = c(-0.05, y_ttl + 0.5), expand = c(0, 0)) +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", colour = NA),
              plot.margin     = margin(16, 24, 16, 24))

      ggsave(file, p, width = 5.5, height = 2.8 + n * 0.05, dpi = 300, bg = "white")
    }
  )
  output$bca_dl_csv <- downloadHandler(
    filename = function() paste0("BCA_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      r  <- bca_results()
      write.csv(data.frame(
        Concentration_mgmL = r$conc, Volume_mL = r$vol,
        Total_yield_mg = r$yield, R_squared = r$curve$r2,
        Title = input$bca_title, Date = Sys.time()
      ), file, row.names = FALSE)
    }
  )

  # History
  output$bca_history_ui <- renderUI({
    h <- bca_history()
    if (length(h) == 0)
      return(p("No analyses run yet this session.", style = "color: var(--muted); font-size: 0.8rem;"))
    rows <- lapply(rev(h), function(e) {
      div(class = "history-row",
        style = "grid-template-columns: 80px 1fr 1fr 1fr 1fr;",
        div(class = "history-time", e$time),
        div(style = "font-size:0.78rem; color:var(--txt);", e$file),
        div(class = "history-val", sprintf("%.3f mg/mL", e$conc)),
        div(class = "history-val", style = "color: var(--accent-green);", sprintf("%.3f mg", e$yield)),
        div(class = "history-val", style = "color: var(--accent-warm);", sprintf("R\u00b2 %.4f", e$r2))
      )
    })
    do.call(div, rows)
  })

  output$bca_history_dl <- renderUI({
    req(length(bca_history()) > 0)
    downloadButton("bca_history_csv", "\u2193 Export History CSV", class = "btn-download")
  })

  output$bca_history_csv <- downloadHandler(
    filename = function() paste0("BCA_history_", format(Sys.time(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      h <- bca_history()
      df <- do.call(rbind, lapply(h, function(e) data.frame(
        Time = e$time, File = e$file, Conc_mgmL = e$conc,
        Yield_mg = e$yield, R2 = e$r2, Mode = e$mode
      )))
      write.csv(df, file, row.names = FALSE)
    }
  )


  # ==========================================================================
  # CPM
  # ==========================================================================
  cpm_data         <- reactiveVal(NULL)
  cpm_results      <- reactiveVal(NULL)
  cpm_history      <- reactiveVal(list())

  # Clear all CPM data
  observeEvent(input$cpm_clear, {
    cpm_data(NULL)
    cpm_results(NULL)
    cpm_history(list())
    shinyjs::reset("cpm_file")
    shinyjs::reset("cpm_sample_id")
    shinyjs::reset("cpm_custom_name")
    shinyjs::reset("cpm_mode")
    shinyjs::reset("cpm_lower_t")
    shinyjs::reset("cpm_upper_t")
    showNotification("CPM data cleared", type = "message", duration = 2)
  })

  # Helper: resolve sample name (custom override or original)
  cpm_resolved_name <- reactive({
    d <- cpm_data()
    req(d, !is.null(d$sample_names))
    custom <- trimws(input$cpm_custom_name)
    sid    <- input$cpm_sample_id
    if (nchar(custom) > 0) custom
    else {
      idx <- which(d$sample_ids == sid)
      if (length(idx)) d$sample_names[idx[1]] else sid
    }
  })

  # Parse file on upload
  observeEvent(input$cpm_file, {
    req(input$cpm_file)
    cpm_results(NULL)
    tryCatch({
      d <- read_rotorgene_csv(input$cpm_file$datapath)
      cpm_data(d)
    }, error = function(e) cpm_data(list(error = conditionMessage(e))))
  })

  output$cpm_file_status <- renderUI({
    req(cpm_data())
    d <- cpm_data()
    if (!is.null(d$error))
      div(class = "status-pill error", div(class = "dot"), "Error loading file")
    else {
      n <- length(d$sample_ids)
      div(class = "status-pill ready", div(class = "dot"),
          sprintf("%d sample%s loaded", n, if (n != 1) "s" else ""))
    }
  })

  output$cpm_sample_ui <- renderUI({
    req(cpm_data())
    d <- cpm_data()
    if (!is.null(d$error)) return(NULL)
    choices <- setNames(d$sample_ids, paste0("[", d$sample_ids, "] ", d$sample_names))
    tagList(
      selectInput("cpm_sample_id", "Sample", choices = choices),
      uiOutput("cpm_sample_confirm")
    )
  })

  output$cpm_sample_confirm <- renderUI({
    req(input$cpm_sample_id, cpm_data())
    d   <- cpm_data()
    idx <- which(d$sample_ids == input$cpm_sample_id)
    if (!length(idx)) return(NULL)
    div(class = "status-pill ready", div(class = "dot"),
        paste0("ID ", d$sample_ids[idx], ": ", d$sample_names[idx]))
  })


  # Get sample data for selected ID
  cpm_sample_data <- reactive({
    req(cpm_data(), input$cpm_sample_id)
    d   <- cpm_data()
    idx <- which(d$sample_ids == input$cpm_sample_id)
    req(length(idx) > 0)
    df  <- data.frame(Temperature = d$temperature, dFdT = d$data[, idx[1]])
    na.omit(df)
  })

  # Preview plot
  output$cpm_preview_plot <- renderPlot({
    req(cpm_data(), input$cpm_sample_id)
    df   <- cpm_sample_data()
    sname <- cpm_resolved_name()
    ggplot(df, aes(x = Temperature, y = dFdT)) +
      geom_line(color = "#00C2FF", linewidth = 1.0) +
      labs(title = paste0("Preview: ", sname), x = "Temperature (\u00b0C)", y = "dF/dT (raw)") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#0F1623", colour = NA),
        panel.background = element_rect(fill = "#0F1623", colour = NA),
        panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.4),
        panel.grid.minor = element_line(colour = "#161E2E", linewidth = 0.2),
        text  = element_text(colour = "#E8F0FE"),
        axis.text = element_text(colour = "#7A8FAD"),
        plot.title = element_text(face = "bold", colour = "#E8F0FE")
      )
  }, bg = "#0F1623")

  # ---- Single run -----------------------------------------------------------
  observeEvent(input$cpm_run, {
    req(cpm_data(), input$cpm_sample_id)
    d <- cpm_data()
    if (!is.null(d$error)) { showNotification(d$error, type = "error"); return() }
    sample_name <- cpm_resolved_name()
    withProgress(message = "Analysing CPM…", value = 0, {
      tryCatch({
        incProgress(0.4, detail = "Calculating Tm…")
        res <- if (input$cpm_mode == "manual") {
          calculate_tm(
            data        = cpm_sample_data(),
            T_lower     = input$cpm_tlow,
            T_upper     = input$cpm_thigh,
            sample_name = sample_name,
            sample_id   = input$cpm_sample_id
          )
        } else {
          calculate_tm_automatic(
            data             = cpm_sample_data(),
            T_min            = input$cpm_tmin,
            T_max            = input$cpm_tmax,
            min_prominence   = input$cpm_prominence,
            smooth_sigma_deg = input$cpm_smooth_sigma,
            min_peak_sep_deg = input$cpm_min_sep,
            boundary_thresh  = input$cpm_boundary_thresh,
            sample_name      = sample_name,
            sample_id        = input$cpm_sample_id
          )
        }
        incProgress(0.5, detail = "Rendering…")
        cpm_results(list(res = res, mode = input$cpm_mode, sample_name = sample_name))
        nav_select("cpm_tabs", "\U0001f4c8  Results")

        # Add to history
        if (input$cpm_mode == "manual") {
          entry <- list(time = format(Sys.time(), "%H:%M:%S"), sample = sample_name,
            tm = res$tm, range = sprintf("%.1f-%.1f", res$T_lower, res$T_upper),
            area = res$area, fwhm = res$fwhm, mode = "manual")
        } else {
          pk1 <- res$peak_results[[1]]
          entry <- list(time = format(Sys.time(), "%H:%M:%S"), sample = sample_name,
            tm = pk1$tm, range = sprintf("%.1f-%.1f", pk1$T_start, pk1$T_end),
            area = pk1$area, fwhm = NA, mode = "auto",
            n_peaks = res$n_peaks)
        }
        cpm_history(c(cpm_history(), list(entry)))

      }, error = function(e) {
        showNotification(paste("Analysis error:", conditionMessage(e)), type = "error", duration = 12)
      })
    })
  })

  # Result badges
  output$cpm_result_badges <- renderUI({
    req(cpm_results())
    r <- cpm_results()
    if (r$mode == "manual") {
      fluidRow(
        column(4, div(class = "result-badge",
          div(class = "result-label", "Tm"),
          div(class = "result-value", sprintf("%.2f \u00b0C", r$res$tm)))),
        column(4, div(class = "result-badge",
          div(class = "result-label", "Integration Range"),
          div(class = "result-value green",
            sprintf("%.1f \u2013 %.1f \u00b0C", r$res$T_lower, r$res$T_upper)))),
        column(4, div(class = "result-badge",
          div(class = "result-label", "Peak Area"),
          div(class = "result-value orange", sprintf("%.4f", r$res$area))))
      )
    } else {
      n <- r$res$n_peaks
      rows <- lapply(seq_len(n), function(i) {
        pk  <- r$res$peak_results[[i]]
        lbl <- if (n == 1) "" else paste0("Peak ", i, " — ")
        tagList(
          fluidRow(
            column(4, div(class = "result-badge",
              div(class = "result-label", paste0(lbl, "Tm")),
              div(class = "result-value", sprintf("%.2f \u00b0C", pk$tm)))),
            column(4, div(class = "result-badge",
              div(class = "result-label", paste0(lbl, "Integration Range")),
              div(class = "result-value green",
                sprintf("%.1f \u2013 %.1f \u00b0C", pk$T_start, pk$T_end)))),
            column(4, div(class = "result-badge",
              div(class = "result-label", paste0(lbl, "Peak Area")),
              div(class = "result-value orange", sprintf("%.4f", pk$area))))
          ),
          if (i < n) tags$hr(style = "border-color:#1E2D45; margin:0.5rem 0;") else NULL
        )
      })
      do.call(tagList, rows)
    }
  })

  # Result plot
  output$cpm_result_plot <- renderPlot({
    req(cpm_results())
    p <- cpm_results()$res$plot
    p + theme(
      plot.background  = element_rect(fill = "#0F1623", colour = NA),
      panel.background = element_rect(fill = "#0F1623", colour = NA),
      panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.4),
      panel.grid.minor = element_line(colour = "#161E2E", linewidth = 0.2),
      text             = element_text(colour = "#E8F0FE"),
      axis.text        = element_text(colour = "#7A8FAD"),
      plot.title       = element_text(face = "bold", colour = "#E8F0FE"),
      plot.subtitle    = element_text(colour = "#7A8FAD")
    )
  }, bg = "#0F1623")

  # Results table (vertical, styled)
  output$cpm_results_table <- renderUI({
    req(cpm_results())
    r <- cpm_results()

    make_tbl <- function(params, vals) {
      th_style <- "background:#161E2E;color:#7A8FAD;font-family:monospace;font-size:0.72rem;letter-spacing:0.08em;text-transform:uppercase;padding:0.55rem 0.75rem;text-align:left;border-bottom:1px solid #1E2D45;"
      td_p <- "padding:0.5rem 0.75rem;color:#7A8FAD;font-size:0.82rem;font-weight:500;border-bottom:1px solid #1E2D45;"
      td_v <- "padding:0.5rem 0.75rem;color:#E8F0FE;font-size:0.82rem;font-family:monospace;border-bottom:1px solid #1E2D45;"
      rows <- mapply(function(p, v) tags$tr(tags$td(p, style = td_p), tags$td(v, style = td_v)),
                     params, vals, SIMPLIFY = FALSE)
      tags$table(style = "width:100%;border-collapse:collapse;",
        tags$thead(tags$tr(tags$th("PARAMETER", style = th_style), tags$th("VALUE", style = th_style))),
        tags$tbody(rows))
    }

    if (r$mode == "manual") {
      make_tbl(
        c("Sample", "Tm (\u00b0C)", "Integration Range", "Peak Area", "FWHM (\u00b0C)", "N points"),
        c(r$sample_name, sprintf("%.2f", r$res$tm),
          sprintf("%.1f \u2013 %.1f \u00b0C", r$res$T_lower, r$res$T_upper),
          sprintf("%.4f", r$res$area),
          if (!is.null(r$res$fwhm) && !is.na(r$res$fwhm)) sprintf("%.2f", r$res$fwhm) else "N/A",
          as.character(r$res$n_points)))
    } else {
      n <- r$res$n_peaks
      hdr <- "font-size:0.72rem;letter-spacing:0.1em;text-transform:uppercase;color:#00C2FF;padding:0.75rem 0.75rem 0.35rem;font-family:monospace;"
      div_sep <- "border-top:2px solid #1E2D45;margin-top:0.75rem;"
      blocks <- lapply(seq_len(n), function(i) {
        pk <- r$res$peak_results[[i]]
        tagList(
          if (i > 1) tags$div(style = div_sep) else NULL,
          tags$div(if (n == 1) "Detected Peak" else paste0("Peak ", i, " of ", n), style = hdr),
          make_tbl(
            c("Tm (\u00b0C)", "Integration Range", "Peak Area", "Height", "Prominence", "FWHM (\u00b0C)", "N points"),
            c(sprintf("%.2f", pk$tm),
              sprintf("%.1f \u2013 %.1f \u00b0C", pk$T_start, pk$T_end),
              sprintf("%.4f", pk$area),
              sprintf("%.3f", pk$height),
              sprintf("%.3f", pk$prominence),
              if (!is.null(pk$fwhm) && !is.na(pk$fwhm)) sprintf("%.2f", pk$fwhm) else "N/A",
              as.character(pk$n_points)))
        )
      })
      do.call(tagList, blocks)
    }
  })

  # Single run downloads
  output$cpm_download_buttons <- renderUI({
    req(cpm_results())
    tagList(
      downloadButton("cpm_dl_png", "\u2193 PNG Plot",  class = "btn-download"), " ",
      downloadButton("cpm_dl_pdf", "\u2193 PDF Plot",  class = "btn-download"), " ",
      downloadButton("cpm_dl_csv", "\u2193 CSV Results", class = "btn-download")
    )
  })

  dl_cpm_plot <- function(file, device, bg) {
    req(cpm_results())
    p <- cpm_results()$res$plot + theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      text = element_text(colour = "black"), axis.text = element_text(colour = "black"),
      panel.grid.major = element_line(colour = "grey90"),
      legend.background = element_rect(fill = "white"))
    ggsave(file, p, width = 12, height = 6, dpi = 300, device = device, bg = bg)
  }

  output$cpm_dl_png <- downloadHandler(
    filename = function() paste0("CPM_Tm_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) dl_cpm_plot(f, "png", "white"))
  output$cpm_dl_pdf <- downloadHandler(
    filename = function() paste0("CPM_Tm_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content  = function(f) dl_cpm_plot(f, "pdf", NULL))
  output$cpm_dl_csv <- downloadHandler(
    filename = function() paste0("CPM_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      r <- cpm_results()
      if (r$mode == "manual") {
        df <- cbind(Sample = r$sample_name, Mode = "Manual",
          Tm_degC = r$res$tm, T_lower = r$res$T_lower, T_upper = r$res$T_upper,
          Area = r$res$area, FWHM = r$res$fwhm, N_points = r$res$n_points,
          Date = Sys.time())
      } else {
        df <- cbind(Sample = r$sample_name, Mode = "Automatic", r$res$summary, Date = Sys.time())
      }
      write.csv(df, file, row.names = FALSE)
    })


  # ---- CPM History ----------------------------------------------------------
  output$cpm_history_ui <- renderUI({
    h <- cpm_history()
    if (length(h) == 0)
      return(p("No analyses run yet.", style = "color:var(--muted);font-size:0.8rem;"))
    rows <- lapply(rev(h), function(e) {
      div(class = "history-row",
        style = "grid-template-columns: 70px 1fr 80px 120px 80px;",
        div(class = "history-time", e$time),
        div(style = "font-size:0.78rem;color:var(--txt);overflow:hidden;text-overflow:ellipsis;white-space:nowrap;", e$sample),
        div(class = "history-val", sprintf("%.2f\u00b0C", e$tm)),
        div(style = "font-size:0.75rem;color:var(--muted);", e$range),
        div(class = "history-val", style = "color:var(--accent-warm);", sprintf("A=%.4f", e$area))
      )
    })
    do.call(div, rows)
  })

  output$cpm_history_dl <- renderUI({
    req(length(cpm_history()) > 0)
    downloadButton("cpm_history_csv", "\u2193 Export History CSV", class = "btn-download")
  })

  output$cpm_history_csv <- downloadHandler(
    filename = function() paste0("CPM_history_", format(Sys.time(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      h  <- cpm_history()
      df <- do.call(rbind, lapply(h, function(e)
        data.frame(Time = e$time, Sample = e$sample, Tm_degC = e$tm,
          Range = e$range, Area = e$area,
          FWHM = if (is.null(e$fwhm) || is.na(e$fwhm)) NA else e$fwhm,
          Mode = e$mode)))
      write.csv(df, file, row.names = FALSE)
    })


  # ==========================================================================
  # CPM QC
  # ==========================================================================
  qc_data    <- reactiveVal(NULL)
  qc_results <- reactiveVal(NULL)

  
  # Clear all QC data
  observeEvent(input$qc_clear, {
    qc_data(NULL)
    qc_results(NULL)
    shinyjs::reset("qc_file")
    shinyjs::reset("qc_sample_a")
    shinyjs::reset("qc_sample_b")
    shinyjs::reset("qc_label_a")
    shinyjs::reset("qc_label_b")
    shinyjs::reset("qc_color_a")
    shinyjs::reset("qc_color_b")
    updateNumericInput(session, "qc_tm_a", value = NA)
    updateNumericInput(session, "qc_tm_b", value = NA)
    showNotification("CPM QC data cleared", type = "message", duration = 2)
  })

  observeEvent(input$qc_file, {
    req(input$qc_file)
    qc_results(NULL)
    tryCatch({
      d <- read_rotorgene_csv_full(input$qc_file$datapath)
      qc_data(d)
    }, error = function(e) qc_data(list(error = conditionMessage(e))))
  })

  output$qc_file_status <- renderUI({
    req(qc_data())
    d <- qc_data()
    if (!is.null(d$error))
      div(class = "status-pill error", div(class = "dot"), "Error loading file")
    else {
      n <- length(d$sample_ids)
      div(class = "status-pill ready", div(class = "dot"),
          sprintf("%d sample%s loaded", n, if (n != 1) "s" else ""))
    }
  })

  qc_sample_choices <- reactive({
    req(qc_data())
    d <- qc_data()
    if (!is.null(d$error)) return(NULL)
    setNames(d$sample_ids, paste0("[", d$sample_ids, "] ", d$sample_names))
  })

  output$qc_sample_a_ui <- renderUI({
    req(qc_sample_choices())
    selectInput("qc_sample_a", "Sample A", choices = qc_sample_choices())
  })

  output$qc_sample_b_ui <- renderUI({
    req(qc_sample_choices())
    ch <- qc_sample_choices()
    # Default to second sample if available
    sel <- if (length(ch) >= 2) ch[2] else ch[1]
    selectInput("qc_sample_b", "Sample B", choices = ch, selected = sel)
  })

  # Auto-detect Tms from dF/dT maxima and populate the number inputs
  output$qc_tm_auto_status <- renderUI({
    req(qc_data(), input$qc_sample_a, input$qc_sample_b)
    d <- qc_data()
    get_auto_tm <- function(sid) {
      idx <- which(d$sample_ids == sid)
      if (!length(idx)) return(NA)
      dfdt <- d$data[, idx[1]]
      temps <- d$temperature
      valid <- !is.na(dfdt)
      if (!any(valid)) return(NA)
      round(temps[which.max(dfdt)], 2)
    }
    tm_a <- get_auto_tm(input$qc_sample_a)
    tm_b <- get_auto_tm(input$qc_sample_b)

    # Always update Tm inputs when sample selection changes.
    # No "only if empty" guard — stale values from a previous sample must be replaced.
    if (!is.null(tm_a) && !is.na(tm_a))
      updateNumericInput(session, "qc_tm_a", value = tm_a)
    if (!is.null(tm_b) && !is.na(tm_b))
      updateNumericInput(session, "qc_tm_b", value = tm_b)

    div(class = "status-pill ready", style = "margin-top:0.3rem;",
      div(class = "dot"),
      sprintf("Auto: A = %.1f\u00b0C  |  B = %.1f\u00b0C",
              if (is.na(tm_a)) 0 else tm_a,
              if (is.na(tm_b)) 0 else tm_b))
  })

  # ── Run QC ────────────────────────────────────────────────────────────────
  observeEvent(input$qc_run, {
    req(qc_data(), input$qc_sample_a, input$qc_sample_b)
    d <- qc_data()
    if (!is.null(d$error)) { showNotification(d$error, type = "error"); return() }

    idx_a <- which(d$sample_ids == input$qc_sample_a)[1]
    idx_b <- which(d$sample_ids == input$qc_sample_b)[1]
    if (is.na(idx_a) || is.na(idx_b)) {
      showNotification("Could not find selected samples in file.", type = "error"); return()
    }

    # Validate matrices before indexing — avoids 'set attribute on NULL' crash
    if (is.null(d$data) || ncol(d$data) < max(idx_a, idx_b)) {
      showNotification("dF/dT data matrix is missing or too narrow. Check file format.", type = "error"); return()
    }
    if (is.null(d$raw_data) || nrow(d$raw_data) == 0 || ncol(d$raw_data) < max(idx_a, idx_b)) {
      showNotification(paste0(
        "Raw fluorescence block could not be parsed. ",
        "Ensure this is a full RotorGene Q export containing both the raw ",
        "fluorescence section (~row 23) and the Melt analysis dF/dT section (~row 97)."),
        type = "error", duration = 15)
      return()
    }

    lbl_a  <- if (nchar(trimws(input$qc_label_a)) > 0) trimws(input$qc_label_a) else d$sample_names[idx_a]
    lbl_b  <- if (nchar(trimws(input$qc_label_b)) > 0) trimws(input$qc_label_b) else d$sample_names[idx_b]
    col_a  <- if (!is.null(input$qc_col_a) && nchar(input$qc_col_a) > 0) input$qc_col_a else "#E41A1C"
    col_b  <- if (!is.null(input$qc_col_b) && nchar(input$qc_col_b) > 0) input$qc_col_b else "#377EB8"
    lw     <- input$qc_linewidth
    title  <- trimws(input$qc_title)

    # Tm values: manual override wins, else auto
    get_auto_tm <- function(idx) {
      dfdt <- d$data[, idx]
      temps <- d$temperature
      round(temps[which.max(dfdt)], 2)
    }
    tm_a <- if (!is.na(input$qc_tm_a) && input$qc_tm_a > 0) input$qc_tm_a else get_auto_tm(idx_a)
    tm_b <- if (!is.na(input$qc_tm_b) && input$qc_tm_b > 0) input$qc_tm_b else get_auto_tm(idx_b)

    # ── Build data frames ──────────────────────────────────────────────────
    # Raw fluorescence
    raw_a <- na.omit(data.frame(Temperature = d$raw_temperature,
                                Value       = d$raw_data[, idx_a],
                                Sample      = lbl_a))
    raw_b <- na.omit(data.frame(Temperature = d$raw_temperature,
                                Value       = d$raw_data[, idx_b],
                                Sample      = lbl_b))
    raw_df <- rbind(raw_a, raw_b)

    # dF/dT (unnormalised)
    dfdt_a <- na.omit(data.frame(Temperature = d$temperature,
                                 Value       = d$data[, idx_a],
                                 Sample      = lbl_a))
    dfdt_b <- na.omit(data.frame(Temperature = d$temperature,
                                 Value       = d$data[, idx_b],
                                 Sample      = lbl_b))
    dfdt_df <- rbind(dfdt_a, dfdt_b)

    pal <- c(lbl_a = col_a, lbl_b = col_b)
    names(pal) <- c(lbl_a, lbl_b)

    # ── Resolve legend position from UI ──────────────────────────────────────
    leg_choice <- if (!is.null(input$qc_legend_pos)) input$qc_legend_pos else "topright"
    leg_pos  <- switch(leg_choice,
      topright    = c(0.98, 0.98), topleft    = c(0.02, 0.98),
      bottomright = c(0.98, 0.02), bottomleft = c(0.02, 0.02),
      c(0.98, 0.98))
    leg_just <- switch(leg_choice,
      topright    = c(1, 1), topleft    = c(0, 1),
      bottomright = c(1, 0), bottomleft = c(0, 0),
      c(1, 1))

    # ── Shared theme (Prism-style: white, clean) ──────────────────────────────
    prism_theme <- function(base = 12, legend_pos = leg_pos,
                            legend_just = leg_just) {
      theme_classic(base_size = base) +
      theme(
        plot.title        = element_text(face = "bold", size = base + 2, hjust = 0),
        axis.title        = element_text(face = "bold", size = base),
        axis.text         = element_text(size = base - 1, colour = "black"),
        axis.line         = element_line(colour = "black", linewidth = 0.7),
        axis.ticks        = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.18, "cm"),
        legend.position   = legend_pos,
        legend.justification = legend_just,
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key        = element_rect(fill = NA, colour = NA),
        legend.key.size   = unit(0.9, "lines"),
        legend.text       = element_text(size = base - 1),
        legend.title      = element_blank(),
        panel.grid        = element_blank(),
        plot.background   = element_rect(fill = "white", colour = NA),
        panel.background  = element_rect(fill = "white", colour = NA),
        plot.margin       = margin(12, 16, 10, 10)
      )
    }

    # -- Plot 1: Raw Fluorescence ------------------------------------------
    p_raw <- ggplot(raw_df, aes(x = Temperature, y = Value, colour = Sample)) +
      geom_line(linewidth = lw) +
      scale_colour_manual(values = pal) +
      scale_x_continuous(limits = c(25, 90), breaks = seq(25, 90, 5),
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                         expand = c(0, 0)) +
      labs(title = if (nchar(title) > 0) paste(title, "FU") else NULL,
           x = "Temperature (\u00b0C)", y = "Fluorescence (a.u.)") +
      prism_theme()

    # -- Plot 2: dF/dT -----------------------------------------------------
    p_dfdt <- ggplot(dfdt_df, aes(x = Temperature, y = Value, colour = Sample)) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_line(linewidth = lw) +
      scale_colour_manual(values = pal) +
      scale_x_continuous(limits = c(25.5, 89.5), breaks = seq(30, 85, 10),
                         expand = c(0, 0)) +
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
      labs(title = if (nchar(title) > 0) paste(title, "dF/dT") else NULL,
           x = "Temperature (\u00b0C)", y = "dF/dT") +
      prism_theme()

    # -- Plot 3: Tm bar chart -----------------------------------------------
    show_tm_labels <- isTRUE(input$qc_show_tm_labels)
    show_dtm       <- isTRUE(input$qc_show_dtm)
    dtm_val        <- tm_b - tm_a

    tm_df <- data.frame(
      Sample = factor(c(lbl_a, lbl_b), levels = c(lbl_a, lbl_b)),
      Tm     = c(tm_a, tm_b),
      Col    = c(col_a, col_b)
    )
    tm_df$x_num <- c(1, 2)
    tm_df$label <- c(lbl_a, lbl_b)
    bar_width    <- 0.4

    # x-axis upper limit: extend right if showing ΔTm annotation
    x_hi <- if (show_dtm) 3.4 else 2.6

    p_tm <- ggplot(tm_df) +
      geom_rect(aes(xmin = x_num - bar_width/2, xmax = x_num + bar_width/2,
                    ymin = 25, ymax = Tm, fill = label),
                colour = "black", linewidth = 0.6, show.legend = FALSE) +
      geom_point(aes(x = x_num, y = Tm), size = 2.5, colour = "black", shape = 16) +
      scale_fill_manual(values = pal) +
      scale_x_continuous(breaks = c(1, 2), labels = c(lbl_a, lbl_b),
                         limits = c(0.4, x_hi), expand = c(0, 0)) +
      scale_y_continuous(limits = c(25, 75), breaks = seq(25, 75, 10),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(title = if (nchar(title) > 0) paste(title, "Tm") else NULL,
           x = NULL, y = "T\u2098 (\u00b0C)") +
      prism_theme(legend_pos = "none", legend_just = c(0.5, 0.5))

    # Optional: Tm value labels above each bar in matching bar colour
    if (show_tm_labels) {
      p_tm <- p_tm +
        geom_text(data = tm_df,
                  aes(x = x_num, y = Tm + 0.8,
                      label = sprintf("%.1f\u00b0C", Tm),
                      colour = label),
                  vjust = 0, size = 3.5, fontface = "bold",
                  inherit.aes = FALSE) +
        scale_colour_manual(values = pal, guide = "none")
    }

    # Optional: ΔTm annotation to the right of the bars
    if (show_dtm) {
      mid_y   <- (tm_a + tm_b) / 2
      x_line  <- 2.38   # closer to bar 2 (right edge of bar is at 2 + 0.2 = 2.2)
      x_label <- 2.46   # tight gap between bracket and text

      p_tm <- p_tm +
        # bracket lines
        annotate("segment", x = x_line, xend = x_line,
                 y = tm_a, yend = tm_b,
                 colour = "black", linewidth = 0.6) +
        annotate("segment", x = x_line - 0.05, xend = x_line,
                 y = tm_a, yend = tm_a,
                 colour = "black", linewidth = 0.6) +
        annotate("segment", x = x_line - 0.05, xend = x_line,
                 y = tm_b, yend = tm_b,
                 colour = "black", linewidth = 0.6) +
        # ΔTₘ label — left-aligned from x_label
        annotate("text",
                 x = x_label, y = mid_y,
                 label = sprintf("\u0394T\u2098 = %.1f\u00b0C", dtm_val),
                 hjust = 0, vjust = 0.5,
                 size = 3.5, fontface = "bold", colour = "black")
    }

    qc_results(list(
      p_raw  = p_raw,
      p_dfdt = p_dfdt,
      p_tm   = p_tm,
      tm_a   = tm_a, tm_b   = tm_b,
      lbl_a  = lbl_a, lbl_b = lbl_b,
      title  = title
    ))
  })

  # ── Output badges ─────────────────────────────────────────────────────────
  output$qc_badges <- renderUI({
    req(qc_results())
    r <- qc_results()
    fluidRow(
      column(6, div(class = "result-badge",
        div(class = "result-label", paste0(r$lbl_a, " Tm")),
        div(class = "result-value", sprintf("%.2f \u00b0C", r$tm_a))
      )),
      column(6, div(class = "result-badge",
        div(class = "result-label", paste0(r$lbl_b, " Tm")),
        div(class = "result-value green", sprintf("%.2f \u00b0C", r$tm_b))
      ))
    )
  })

  output$qc_raw_placeholder <- renderUI({
    if (is.null(qc_results()))
      div(class = "plot-placeholder", div(class = "icon", "\U0001f321\ufe0f"),
          "Upload a file, select two samples and click Generate QC Plots")
  })

  # ── Plot renderers ────────────────────────────────────────────────────────
  dark_overlay <- function(p) {
    p + theme(
      plot.background  = element_rect(fill = "#0F1623", colour = NA),
      panel.background = element_rect(fill = "#0F1623", colour = NA),
      axis.line        = element_line(colour = "#7A8FAD"),
      axis.ticks       = element_line(colour = "#7A8FAD"),
      axis.text        = element_text(colour = "#7A8FAD"),
      axis.title       = element_text(colour = "#E8F0FE"),
      plot.title       = element_text(colour = "#E8F0FE"),
      legend.text      = element_text(colour = "#E8F0FE"),
      panel.grid       = element_blank()
    )
  }

  output$qc_raw_plot <- renderPlot({
    req(qc_results())
    dark_overlay(qc_results()$p_raw)
  }, bg = "#0F1623")

  output$qc_dfdt_plot <- renderPlot({
    req(qc_results())
    dark_overlay(qc_results()$p_dfdt)
  }, bg = "#0F1623")

  output$qc_tm_plot <- renderPlot({
    req(qc_results())
    dark_overlay(qc_results()$p_tm)
  }, bg = "#0F1623")

  # ── Downloads ─────────────────────────────────────────────────────────────
  output$qc_download_buttons <- renderUI({
    req(qc_results())
    tagList(
      downloadButton("qc_dl_raw",  "\u2193 PNG Fluorescence", class = "btn-download"), " ",
      downloadButton("qc_dl_dfdt", "\u2193 PNG dF/dT",       class = "btn-download"), " ",
      downloadButton("qc_dl_tm",   "\u2193 PNG Tm Chart",    class = "btn-download"), " ",
      downloadButton("qc_dl_csv",  "\u2193 CSV Summary",     class = "btn-download")
    )
  })

  save_qc_plot <- function(p, file, w = 5.5, h = 4.5) {
    ggsave(file, p, width = w, height = h, dpi = 300, bg = "white")
  }

  output$qc_dl_raw  <- downloadHandler(
    filename = function() paste0("CPM_QC_fluorescence_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) save_qc_plot(qc_results()$p_raw, f))
  output$qc_dl_dfdt <- downloadHandler(
    filename = function() paste0("CPM_QC_dFdT_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) save_qc_plot(qc_results()$p_dfdt, f))
  output$qc_dl_tm   <- downloadHandler(
    filename = function() paste0("CPM_QC_Tm_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) {
      w <- if (isTRUE(isolate(input$qc_show_dtm))) 6 else 4
      save_qc_plot(qc_results()$p_tm, f, w = w, h = 5)
    })
  output$qc_dl_csv  <- downloadHandler(
    filename = function() paste0("CPM_QC_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      r <- qc_results()
      write.csv(data.frame(
        Label  = c(r$lbl_a, r$lbl_b),
        Tm_degC = c(r$tm_a, r$tm_b),
        dTm_degC = c(NA, r$tm_b - r$tm_a),
        Date   = Sys.time()
      ), file, row.names = FALSE)
    })



  # ==========================================================================
  # CPM QC — MULTI-SAMPLE MODE
  # ==========================================================================
  # Qualitative palette for up to 10 samples (ColorBrewer Set1 + extra)
  MQC_PALETTE <- c("#E41A1C","#377EB8","#4DAF4A","#FF7F00","#984EA3",
                   "#A65628","#F781BF","#00CED1","#FFDB58","#555555")

  mqc_data    <- reactiveVal(NULL)
  mqc_results <- reactiveVal(NULL)

  observeEvent(input$qc_clear, {
    mqc_data(NULL)
    mqc_results(NULL)
    shinyjs::reset("mqc_file")
  })

  observeEvent(input$mqc_file, {
    req(input$mqc_file)
    mqc_results(NULL)
    tryCatch({
      d <- read_rotorgene_csv_full(input$mqc_file$datapath)
      mqc_data(d)
    }, error = function(e) mqc_data(list(error = conditionMessage(e))))
  })

  output$mqc_file_status <- renderUI({
    req(mqc_data())
    d <- mqc_data()
    if (!is.null(d$error))
      div(class = "status-pill error", div(class = "dot"), "Error loading file")
    else {
      n <- length(d$sample_ids)
      div(class = "status-pill ready", div(class = "dot"),
          sprintf("%d sample%s loaded", n, if (n != 1) "s" else ""))
    }
  })

  mqc_sample_choices <- reactive({
    req(mqc_data())
    d <- mqc_data()
    if (!is.null(d$error)) return(NULL)
    setNames(d$sample_ids, paste0("[", d$sample_ids, "] ", d$sample_names))
  })

  output$mqc_sample_select_ui <- renderUI({
    req(mqc_sample_choices())
    ch  <- mqc_sample_choices()
    # selected must be the VALUES (sample_ids), not the named vector
    sel <- as.character(ch[seq_len(min(length(ch), 10))])
    n_vis <- min(length(ch), 8)   # show up to 8 rows before scrolling
    tagList(
      tags$style(HTML("#mqc_samples { height: auto !important; }")),
      selectInput("mqc_samples", NULL, choices = ch, selected = sel,
                  multiple = TRUE, width = "100%",
                  selectize = FALSE, size = n_vis)
    )
  })

  output$mqc_labels_ui <- renderUI({
    req(mqc_data())
    d    <- mqc_data()
    if (!is.null(d$error)) return(NULL)
    # Fall back to all sample ids if nothing selected yet
    sids <- if (!is.null(input$mqc_samples) && length(input$mqc_samples) > 0) {
      input$mqc_samples
    } else {
      as.character(d$sample_ids[seq_len(min(length(d$sample_ids), 10))])
    }
    n    <- length(sids)
    cols <- MQC_PALETTE[seq_len(n)]

    rows <- lapply(seq_len(n), function(i) {
      sid    <- sids[i]
      idx    <- which(d$sample_ids == sid)[1]
      dname  <- if (!is.na(idx)) d$sample_names[idx] else sid
      fluidRow(style = "margin-bottom:4px;",
        column(2,
          div(style = sprintf(
            "width:1.1rem;height:1.1rem;border-radius:50%%;background:%s;margin-top:0.5rem;", cols[i]))
        ),
        column(10,
          textInput(paste0("mqc_lbl_", i), NULL,
            placeholder = dname, width = "100%")
        )
      )
    })
    tagList(rows)
  })

  # ── Run multi-sample QC ───────────────────────────────────────────────────
  observeEvent(input$mqc_run, {
    req(mqc_data(), input$mqc_samples)
    d    <- mqc_data()
    sids <- input$mqc_samples
    n    <- length(sids)

    if (!is.null(d$error)) { showNotification(d$error, type = "error"); return() }
    if (n < 2) { showNotification("Select at least 2 samples.", type = "warning"); return() }
    if (n > 10) { showNotification("Maximum 10 samples.", type = "warning"); return() }

    cols   <- MQC_PALETTE[seq_len(n)]
    title  <- trimws(input$mqc_title)
    lw     <- input$mqc_linewidth

    # Resolve labels (use override if non-empty, else original sample name)
    labels <- sapply(seq_len(n), function(i) {
      ov  <- trimws(input[[paste0("mqc_lbl_", i)]])
      idx <- which(d$sample_ids == sids[i])[1]
      if (!is.null(ov) && nchar(ov) > 0) ov else d$sample_names[idx]
    })

    # Validate data matrices
    idxs <- sapply(sids, function(s) which(d$sample_ids == s)[1])
    if (any(is.na(idxs))) { showNotification("Could not locate all selected samples.", type = "error"); return() }
    if (is.null(d$data) || ncol(d$data) < max(idxs)) {
      showNotification("dF/dT data matrix too narrow. Check file format.", type = "error"); return()
    }
    if (is.null(d$raw_data) || ncol(d$raw_data) < max(idxs)) {
      showNotification("Raw fluorescence block missing. Ensure this is a full RotorGene Q export.", type = "error", duration = 10); return()
    }

    pal <- setNames(cols, labels)

    # ── Build combined data frames ────────────────────────────────────────
    raw_df  <- do.call(rbind, lapply(seq_len(n), function(i) {
      na.omit(data.frame(Temperature = d$raw_temperature,
                         Value       = d$raw_data[, idxs[i]],
                         Sample      = labels[i]))
    }))
    raw_df$Sample <- factor(raw_df$Sample, levels = labels)

    dfdt_df <- do.call(rbind, lapply(seq_len(n), function(i) {
      na.omit(data.frame(Temperature = d$temperature,
                         Value       = d$data[, idxs[i]],
                         Sample      = labels[i]))
    }))
    dfdt_df$Sample <- factor(dfdt_df$Sample, levels = labels)

    # Auto Tm — peak of dF/dT for each sample
    tms <- sapply(idxs, function(idx) {
      dfdt <- d$data[, idx]
      round(d$temperature[which.max(dfdt)], 2)
    })
    names(tms) <- labels

    # ── Shared Prism theme (same as simple mode) ──────────────────────────
    prism_theme <- function(base = 12, legend_pos = c(0.98, 0.98),
                            legend_just = c(1, 1)) {
      theme_classic(base_size = base) +
      theme(
        plot.title        = element_text(face = "bold", size = base + 2, hjust = 0),
        axis.title        = element_text(face = "bold", size = base),
        axis.text         = element_text(size = base - 1, colour = "black"),
        axis.line         = element_line(colour = "black", linewidth = 0.7),
        axis.ticks        = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.18, "cm"),
        legend.position   = legend_pos,
        legend.justification = legend_just,
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key        = element_rect(fill = NA, colour = NA),
        legend.key.size   = unit(0.9, "lines"),
        legend.text       = element_text(size = base - 1),
        legend.title      = element_blank(),
        panel.grid        = element_blank(),
        plot.background   = element_rect(fill = "white", colour = NA),
        panel.background  = element_rect(fill = "white", colour = NA),
        plot.margin       = margin(12, 16, 10, 10)
      )
    }

    # -- Plot 1: Raw Fluorescence ------------------------------------------
    p_raw <- ggplot(raw_df, aes(x = Temperature, y = Value, colour = Sample)) +
      geom_line(linewidth = lw) +
      scale_colour_manual(values = pal) +
      scale_x_continuous(limits = c(25, 90), breaks = seq(25, 90, 5), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
      labs(title = if (nchar(title) > 0) paste(title, "FU") else NULL,
           x = "Temperature (\u00b0C)", y = "Fluorescence (a.u.)") +
      prism_theme()

    # -- Plot 2: dF/dT --------------------------------------------------------
    p_dfdt <- ggplot(dfdt_df, aes(x = Temperature, y = Value, colour = Sample)) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_line(linewidth = lw) +
      scale_colour_manual(values = pal) +
      scale_x_continuous(limits = c(25.5, 89.5), breaks = seq(30, 85, 10), expand = c(0, 0)) +
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
      labs(title = if (nchar(title) > 0) paste(title, "dF/dT") else NULL,
           x = "Temperature (\u00b0C)", y = "dF/dT") +
      prism_theme()

    # -- Plot 3: Tm bar chart (generalised to n samples) ----------------------
    tm_df <- data.frame(
      Sample = factor(labels, levels = labels),
      Tm     = tms,
      x_num  = seq_len(n),
      Col    = cols,
      stringsAsFactors = FALSE
    )
    bar_width <- max(0.25, min(0.5, 2.5 / n))  # narrower bars for more samples

    p_tm <- ggplot(tm_df) +
      geom_rect(aes(xmin = x_num - bar_width/2, xmax = x_num + bar_width/2,
                    ymin = 25, ymax = Tm, fill = Sample),
                colour = "black", linewidth = 0.5, show.legend = FALSE) +
      geom_point(aes(x = x_num, y = Tm), size = 2, colour = "black", shape = 16) +
      scale_fill_manual(values = pal) +
      scale_x_continuous(breaks = seq_len(n), labels = labels,
                         limits = c(0.4, n + 0.6), expand = c(0, 0)) +
      scale_y_continuous(limits = c(25, 75), breaks = seq(25, 75, 10),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(title = if (nchar(title) > 0) paste(title, "Tm") else NULL,
           x = NULL, y = "T\u2098 (\u00b0C)") +
      prism_theme(legend_pos = "none") +
      theme(axis.text.x = element_text(angle = if (n > 4) 35 else 0,
                                        hjust = if (n > 4) 1 else 0.5))

    mqc_results(list(
      p_raw  = p_raw,
      p_dfdt = p_dfdt,
      p_tm   = p_tm,
      tms    = tms,
      labels = labels,
      title  = title,
      n      = n
    ))
  })

  # ── Badges ────────────────────────────────────────────────────────────────
  output$mqc_badges <- renderUI({
    req(mqc_results())
    r   <- mqc_results()
    cols <- MQC_PALETTE[seq_len(r$n)]
    badge_cols <- lapply(seq_len(r$n), function(i) {
      column(max(2, floor(12 / r$n)),
        div(class = "result-badge",
          div(class = "result-label",
            div(style = sprintf("display:inline-block;width:0.6rem;height:0.6rem;border-radius:50%%;background:%s;margin-right:0.3rem;vertical-align:middle;", cols[i])),
            r$labels[i]
          ),
          div(class = "result-value", style = "font-size:1.1rem;",
            sprintf("%.2f\u00b0C", r$tms[i]))
        )
      )
    })
    do.call(fluidRow, badge_cols)
  })

  output$mqc_raw_placeholder <- renderUI({
    if (is.null(mqc_results()))
      div(class = "plot-placeholder", div(class = "icon", "\U0001f321\ufe0f"),
          "Upload a file, select samples and click Generate QC Plots")
  })

  # ── Plot renderers ────────────────────────────────────────────────────────
  dark_overlay_mqc <- function(p) {
    p + theme(
      plot.background  = element_rect(fill = "#0F1623", colour = NA),
      panel.background = element_rect(fill = "#0F1623", colour = NA),
      axis.line        = element_line(colour = "#7A8FAD"),
      axis.ticks       = element_line(colour = "#7A8FAD"),
      axis.text        = element_text(colour = "#7A8FAD"),
      axis.title       = element_text(colour = "#E8F0FE"),
      plot.title       = element_text(colour = "#E8F0FE"),
      legend.text      = element_text(colour = "#E8F0FE"),
      panel.grid       = element_blank()
    )
  }

  output$mqc_raw_plot  <- renderPlot({ req(mqc_results()); dark_overlay_mqc(mqc_results()$p_raw)  }, bg = "#0F1623")
  output$mqc_dfdt_plot <- renderPlot({ req(mqc_results()); dark_overlay_mqc(mqc_results()$p_dfdt) }, bg = "#0F1623")
  output$mqc_tm_plot   <- renderPlot({ req(mqc_results()); dark_overlay_mqc(mqc_results()$p_tm)   }, bg = "#0F1623")

  # ── Downloads ─────────────────────────────────────────────────────────────
  output$mqc_download_buttons <- renderUI({
    req(mqc_results())
    tagList(
      downloadButton("mqc_dl_raw",  "\u2193 PNG Fluorescence", class = "btn-download"), " ",
      downloadButton("mqc_dl_dfdt", "\u2193 PNG dF/dT",        class = "btn-download"), " ",
      downloadButton("mqc_dl_tm",   "\u2193 PNG Tm Chart",     class = "btn-download"), " ",
      downloadButton("mqc_dl_csv",  "\u2193 CSV Summary",      class = "btn-download")
    )
  })

  save_mqc_plot <- function(p, file, w = 6.5, h = 4.5) {
    ggsave(file, p, width = w, height = h, dpi = 300, bg = "white")
  }

  output$mqc_dl_raw  <- downloadHandler(
    filename = function() paste0("CPM_multiQC_fluorescence_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) save_mqc_plot(mqc_results()$p_raw,  f))
  output$mqc_dl_dfdt <- downloadHandler(
    filename = function() paste0("CPM_multiQC_dFdT_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) save_mqc_plot(mqc_results()$p_dfdt, f))
  output$mqc_dl_tm   <- downloadHandler(
    filename = function() paste0("CPM_multiQC_Tm_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) {
      r <- mqc_results()
      w <- max(4, 1.2 * r$n)   # wider chart for more samples
      save_mqc_plot(r$p_tm, f, w = w, h = 5)
    })
  output$mqc_dl_csv  <- downloadHandler(
    filename = function() paste0("CPM_multiQC_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      r <- mqc_results()
      tms <- r$tms
      write.csv(data.frame(
        Label    = names(tms),
        Tm_degC  = as.numeric(tms),
        dTm_degC = as.numeric(tms) - as.numeric(tms[1]),
        Date     = Sys.time()
      ), file, row.names = FALSE)
    })

  # ==========================================================================
  # UCP1 Proton Conductance
  # ==========================================================================
  ucp1_cal_data <- reactiveVal(NULL)
  ucp1_clearing <- reactiveVal(FALSE)  # Flag to prevent reactive cascade during clear
  ucp1_last_clear <- reactiveVal(Sys.time() - 10)  # Track last clear time to prevent rapid re-renders

  # Clearing overlay - blocks all rendering during clear
  output$ucp1_clearing_overlay <- renderUI({
    if (ucp1_clearing()) {
      div(class = "clearing-overlay",
        div(class = "clearing-spinner"),
        div(class = "clearing-text", "Clearing data...")
      )
    }
  })

  # AGGRESSIVE clear optimization - prevents all reactive firing
  observeEvent(input$ucp1_clear, {
    # NUCLEAR OPTION: Hide entire UCP1 section (like minimizing window)
    # This COMPLETELY stops all rendering
    shinyjs::runjs("document.getElementById('ucp1_content_section').style.display = 'none';")
    
    # Show overlay
    ucp1_clearing(TRUE)
    
    # Small delay to let DOM update
    Sys.sleep(0.1)
    
    # Clear ALL data instantly (no batching needed when content is hidden)
    ucp1_raw_data(NULL)
    ucp1_cap_data(NULL)
    ucp1_cal_data(NULL)
    
    # Reset inputs (isolated to prevent observer cascade)
    isolate({
      shinyjs::reset("ucp1_cal_file")
      shinyjs::reset("ucp1_cap_file")
      shinyjs::reset("ucp1_raw_files")
      shinyjs::reset("ucp1_line_width")
      shinyjs::reset("ucp1_show_titles")
    })
    
    # Aggressive garbage collection (3x for large files)
    gc()
    gc()
    gc()
    
    # Give browser time to finish any pending work
    Sys.sleep(0.5)
    
    # Hide overlay
    ucp1_clearing(FALSE)
    ucp1_last_clear(Sys.time())
    
    # Small delay before showing content
    Sys.sleep(0.2)
    
    # Show UCP1 section again
    shinyjs::runjs("document.getElementById('ucp1_content_section').style.display = 'block';")
    
    showNotification("UCP1 data cleared", type = "message", duration = 2)
  }, ignoreInit = TRUE, priority = 1000)  # VERY high priority

  # Detect plateaus in proton calibration data (cached for performance)
  ucp1_plateaus <- reactive({
    req(!ucp1_clearing())  # Skip if currently clearing
    req(ucp1_cal_data())
    d <- ucp1_cal_data()
    if (!is.null(d$error)) return(NULL)
    
    # Detect plateau regions by finding dramatic drops (>90% from local max)
    # Strategy: intensity drops to <10% indicate gaps between plateaus
    threshold <- max(d$Intensity, na.rm = TRUE) * 0.1
    
    # Label each point as "gap" or "plateau"
    d$is_gap <- d$Intensity < threshold
    
    # Find runs of plateau vs gap
    rle_result <- rle(d$is_gap)
    
    # Assign segment IDs
    segment_id <- rep(seq_along(rle_result$lengths), rle_result$lengths)
    d$segment <- segment_id
    
    # Filter to only plateau segments (not gaps)
    plateau_segments <- unique(segment_id[!d$is_gap])
    
    # Calculate representative intensity for each plateau
    plateau_stats <- lapply(plateau_segments, function(seg) {
      seg_data <- d[d$segment == seg & !d$is_gap, ]
      if (nrow(seg_data) < 3) return(NULL)  # Skip very short segments
      
      # Use median to be robust against spikes
      median_intensity <- median(seg_data$Intensity, na.rm = TRUE)
      
      # Also calculate trimmed mean (10% trim on each side) as alternative
      trim_mean <- mean(seg_data$Intensity, trim = 0.1, na.rm = TRUE)
      
      # Calculate time range of this plateau
      time_start <- min(seg_data$Time, na.rm = TRUE)
      time_end <- max(seg_data$Time, na.rm = TRUE)
      time_mid <- mean(c(time_start, time_end))
      
      list(
        segment = seg,
        n_points = nrow(seg_data),
        time_start = time_start,
        time_end = time_end,
        time_mid = time_mid,
        median_intensity = median_intensity,
        trimmed_mean = trim_mean,
        sd = sd(seg_data$Intensity, na.rm = TRUE)
      )
    })
    
    # Remove NULL entries
    plateau_stats <- plateau_stats[!sapply(plateau_stats, is.null)]
    
    if (length(plateau_stats) == 0) return(NULL)
    
    # Convert to data frame
    plateau_df <- do.call(rbind, lapply(plateau_stats, function(x) {
      data.frame(
        plateau_number = which(sapply(plateau_stats, function(y) y$segment == x$segment)),
        segment = x$segment,
        n_points = x$n_points,
        time_start = x$time_start,
        time_end = x$time_end,
        time_mid = x$time_mid,
        median_intensity = x$median_intensity,
        trimmed_mean = x$trimmed_mean,
        sd = x$sd
      )
    }))
    
    list(
      data_with_segments = d,
      plateau_summary = plateau_df
    )
  })

  observeEvent(input$ucp1_cal_file, ignoreNULL = TRUE, ignoreInit = TRUE, {
    # Debounced to prevent multiple triggers
    req(input$ucp1_cal_file)
    ucp1_cal_data(NULL)
    
    tryCatch({
      # Read CSV: skip first line, then read Time and Intensity
      lines <- readLines(input$ucp1_cal_file$datapath, warn = FALSE)
      # Remove header lines and parse
      data_lines <- lines[3:length(lines)]  # Skip "Proton Cal," and column headers
      
      # Parse into data frame
      parsed <- strsplit(data_lines, ",")
      time_vals <- sapply(parsed, function(x) as.numeric(x[1]))
      intensity_vals <- sapply(parsed, function(x) as.numeric(x[2]))
      
      df <- data.frame(
        Time = time_vals,
        Intensity = intensity_vals
      )
      df <- na.omit(df)
      
      ucp1_cal_data(df)
    }, error = function(e) {
      ucp1_cal_data(list(error = conditionMessage(e)))
    })
  })

  output$ucp1_cal_status <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_cal_data())
    d <- ucp1_cal_data()
    if (!is.null(d$error)) {
      div(class = "status-pill error", div(class = "dot"), 
          paste("Error:", d$error))
    } else {
      n_points <- nrow(d)
      t_range <- range(d$Time, na.rm = TRUE)
      
      # Check if plateaus detected
      plat <- ucp1_plateaus()
      if (!is.null(plat)) {
        n_plateaus <- nrow(plat$plateau_summary)
        div(
          div(class = "status-pill ready", div(class = "dot"),
              sprintf("%d points | %.1f - %.1f s", n_points, t_range[1], t_range[2])),
          div(class = "status-pill ready", div(class = "dot"),
              sprintf("%d plateaus detected", n_plateaus))
        )
      } else {
        div(class = "status-pill ready", div(class = "dot"),
            sprintf("%d points | %.1f - %.1f s", n_points, t_range[1], t_range[2]))
      }
    }
  })


  output$ucp1_plateau_info <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    plat <- ucp1_plateaus()
    if (is.null(plat)) {
      div(style = "color: #7A8FAD; font-size: 0.9em; padding: 0.5rem;",
          "Upload proton calibration data to detect plateaus")
    } else {
      div(style = "color: #22C55E; font-size: 0.9em; padding: 0.5rem;",
          sprintf("✓ %d plateau regions detected and quantified", 
                  nrow(plat$plateau_summary)))
    }
  })
  
  output$ucp1_plateau_table <- renderTable({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_plateaus())
    plat <- isolate(ucp1_plateaus())  # isolate to prevent excessive re-renders
    
    df <- plat$plateau_summary
    df$time_range <- sprintf("%.1f - %.1f s", df$time_start, df$time_end)
    
    # Format output table
    data.frame(
      "Plateau" = df$plateau_number,
      "Time Range" = df$time_range,
      "Median Intensity" = round(df$median_intensity, 2),
      "Trimmed Mean" = round(df$trimmed_mean, 2),
      "SD" = round(df$sd, 2),
      "Points" = df$n_points,
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%", align = "c")

  output$ucp1_cal_placeholder <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    if (is.null(ucp1_cal_data())) {
      div(class = "plot-placeholder", 
          div(class = "icon", "📈"),
          "Upload a proton calibration CSV file to visualize the trace")
    }
  })

  output$ucp1_cal_plot <- renderPlot({
    req(!ucp1_clearing())  # Skip rendering during clear
    req(ucp1_cal_data())
    d <- ucp1_cal_data()
    if (!is.null(d$error)) return(NULL)
    
    lw <- if (!is.null(isolate(input$ucp1_line_width))) isolate(input$ucp1_line_width) else 1
    show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
    
    title_text <- if (show_titles) {
      if (!is.null(isolate(input$ucp1_proton_trace_title)) && nchar(isolate(input$ucp1_proton_trace_title)) > 0) {
        isolate(input$ucp1_proton_trace_title)
      } else {
        "Intensity (a.u.)"
      }
    } else {
      NULL
    }
    
    # Create plot matching Excel style
    p <- ggplot(d, aes(x = Time, y = Intensity)) +
      geom_line(colour = "#2E5CB8", linewidth = lw) +
      scale_x_continuous(expand = c(0, 0), 
                         breaks = scales::pretty_breaks(n = 6)) +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::pretty_breaks(n = 7)) +
      labs(title = title_text,
           x = "Time (s)",
           y = "Intensity (a.u.)") +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, colour = "#E8F0FE"),
        axis.title = element_text(face = "bold", size = 11, colour = "#E8F0FE"),
        axis.text = element_text(size = 10, colour = "#7A8FAD"),
        axis.line = element_line(colour = "#7A8FAD", linewidth = 0.6),
        panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.3),
        plot.background = element_rect(fill = "#0F1623", colour = NA),
        panel.background = element_rect(fill = "#0F1623", colour = NA)
      )
    
    p
  }, bg = "#0F1623")

  output$ucp1_cal_download <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_cal_data())
    d <- ucp1_cal_data()
    if (!is.null(d$error)) return(NULL)
    
    downloadButton("ucp1_cal_dl_png", "↓ PNG Plot", class = "btn-download")
  })

  output$ucp1_cal_dl_png <- downloadHandler(
    filename = function() paste0("UCP1_proton_calibration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ucp1_cal_data())
      d <- ucp1_cal_data()
      
      lw <- if (!is.null(isolate(input$ucp1_line_width))) isolate(input$ucp1_line_width) else 1
      title_text <- if (!is.null(input$ucp1_plot_title) && nchar(input$ucp1_plot_title) > 0) {
        input$ucp1_plot_title
      } else {
        "Intensity (a.u.)"
      }
      
      p <- ggplot(d, aes(x = Time, y = Intensity)) +
        geom_line(colour = "#2E5CB8", linewidth = lw) +
        scale_x_continuous(expand = c(0, 0),
                           breaks = scales::pretty_breaks(n = 6)) +
        scale_y_continuous(limits = c(0, NA),
                           expand = expansion(mult = c(0, 0.05)),
                           breaks = scales::pretty_breaks(n = 7)) +
        labs(title = title_text,
             x = "Time (s)",
             y = "Intensity (a.u.)") +
        theme_classic(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 11),
          axis.text = element_text(size = 10, colour = "black"),
          axis.line = element_line(colour = "black", linewidth = 0.6),
          panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(file, p, width = 8, height = 5, dpi = 300, bg = "white")
    }
  )


  # FU/Proton calibration curve reactive
  ucp1_calibration <- reactive({
    req(!ucp1_clearing())  # Skip if currently clearing
    req(ucp1_plateaus())
    plat <- ucp1_plateaus()
    req(input$ucp1_n_plateaus)
    
    n_use <- as.integer(input$ucp1_n_plateaus)
    n_total <- nrow(plat$plateau_summary)
    
    if (n_use < 2 || n_use > n_total) return(NULL)
    
    # Fixed H+ concentration series (mM): 0, 4, 8, 12, 16, 20, ...
    h_conc <- seq(0, (n_use - 1) * 4, by = 4)
    
    # Get FU values (median intensities) for the first n_use plateaus
    fu_vals <- plat$plateau_summary$median_intensity[1:n_use]
    
    # Calculate 1/FU
    inv_fu <- 1 / fu_vals
    
    # Linear regression: 1/FU ~ [H+]
    calib_df <- data.frame(H_conc = h_conc, inv_FU = inv_fu)
    
    lm_fit <- lm(inv_FU ~ H_conc, data = calib_df)
    slope <- coef(lm_fit)[2]
    intercept <- coef(lm_fit)[1]
    r_squared <- summary(lm_fit)$r.squared
    
    list(
      data = calib_df,
      fu_values = fu_vals,
      model = lm_fit,
      slope = slope,
      intercept = intercept,
      r_squared = r_squared,
      n_used = n_use
    )
  })
  
  output$ucp1_plateau_selector <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    plat <- ucp1_plateaus()
    if (is.null(plat)) return(NULL)
    
    n_total <- nrow(plat$plateau_summary)
    default_n <- max(2, n_total - 1)  # Default: exclude last plateau
    
    tagList(
      div(style = "margin-top: 1rem;"),
      numericInput("ucp1_n_plateaus", 
                   "Number of plateaus to use for calibration",
                   value = default_n,
                   min = 2,
                   max = n_total,
                   step = 1),
      div(style = "color: #7A8FAD; font-size: 0.85em; margin-top: -0.5rem;",
          sprintf("Total detected: %d | Recommended: %d (excludes last plateau)", 
                  n_total, default_n))
    )
  })
  
  output$ucp1_calibcurve_ui <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    calib <- ucp1_calibration()
    
    if (is.null(calib)) {
      div(class = "plot-placeholder", 
          div(class = "icon", "📉"),
          "Configure plateau selection to generate calibration curve")
    } else {
      tagList(
        plotOutput("ucp1_calibcurve_plot", height = "300px"),
        div(style = "background: #1E2D45; padding: 0.6rem; border-radius: 8px; margin-top: 0.5rem;",
          fluidRow(
            column(6,
              div(style = "font-weight: bold; color: #E8F0FE; margin-bottom: 0.5rem;", "Calibration Parameters"),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("Slope: %.6f", calib$slope)),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("Intercept: %.5f", calib$intercept))
            ),
            column(6,
              div(style = "font-weight: bold; color: #E8F0FE; margin-bottom: 0.5rem;", "Fit Quality"),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("R² = %.4f", calib$r_squared)),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("Points used: %d", calib$n_used))
            )
          )
        ),
        div(style = "margin-top: 0.5rem;",
          downloadButton("ucp1_calib_dl_png", "↓ PNG Calibration Curve", class = "btn-download")
        )
      )
    }
  })
  
  output$ucp1_calibcurve_plot <- renderPlot({
    req(!ucp1_clearing())  # Skip rendering during clear
    req(ucp1_calibration())
    calib <- ucp1_calibration()
    show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
    
    curve_title <- if (show_titles) {
      if (!is.null(isolate(input$ucp1_proton_curve_title)) && nchar(isolate(input$ucp1_proton_curve_title)) > 0) {
        isolate(input$ucp1_proton_curve_title)
      } else {
        "FU/Proton Calibration Curve"
      }
    } else {
      NULL
    }
    
    # Plot with regression line
    p <- ggplot(calib$data, aes(x = H_conc, y = inv_FU)) +
      geom_point(size = 3, colour = "#2E5CB8") +
      geom_smooth(method = "lm", se = FALSE, colour = "#2E5CB8", linewidth = 1) +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      annotate("text", x = Inf, y = Inf, 
               label = sprintf("y = %.6fx + %.5f\nR² = %.4f", 
                             calib$slope, calib$intercept, calib$r_squared),
               hjust = 1.1, vjust = 1.5, size = 4, colour = "#E8F0FE") +
      labs(title = curve_title,
           x = "[H+] (mM)",
           y = "1/FU") +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, colour = "#E8F0FE"),
        axis.title = element_text(face = "bold", size = 11, colour = "#E8F0FE"),
        axis.text = element_text(size = 10, colour = "#7A8FAD"),
        axis.line = element_line(colour = "#7A8FAD", linewidth = 0.6),
        panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.3),
        plot.background = element_rect(fill = "#0F1623", colour = NA),
        panel.background = element_rect(fill = "#0F1623", colour = NA)
      )
    
    p
  }, bg = "#0F1623")
  
  output$ucp1_calib_dl_png <- downloadHandler(
    filename = function() paste0("UCP1_calibration_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ucp1_calibration())
      calib <- ucp1_calibration()
      show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
      
      curve_title <- if (show_titles) {
        if (!is.null(isolate(input$ucp1_proton_curve_title)) && nchar(isolate(input$ucp1_proton_curve_title)) > 0) {
          isolate(input$ucp1_proton_curve_title)
        } else {
          "FU/Proton Calibration Curve"
        }
      } else {
        NULL
      }
      
      p <- ggplot(calib$data, aes(x = H_conc, y = inv_FU)) +
        geom_point(size = 3, colour = "#2E5CB8") +
        geom_smooth(method = "lm", se = FALSE, colour = "#2E5CB8", linewidth = 1) +
        scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
        annotate("text", x = Inf, y = Inf, 
                 label = sprintf("y = %.6fx + %.5f\nR² = %.4f", 
                               calib$slope, calib$intercept, calib$r_squared),
                 hjust = 1.1, vjust = 1.5, size = 4, colour = "black") +
        labs(title = curve_title,
             x = "[H+] (mM)",
             y = "1/FU") +
        theme_classic(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 11),
          axis.text = element_text(size = 10, colour = "black"),
          axis.line = element_line(colour = "black", linewidth = 0.6),
          panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(file, p, width = 6, height = 5, dpi = 300, bg = "white")
    }
  )
  
  # -- Capacity Calibration Data ----------------------------------------------
  ucp1_cap_data <- reactiveVal(NULL)
  
  observeEvent(input$ucp1_cap_file, ignoreNULL = TRUE, ignoreInit = TRUE, {
    req(input$ucp1_cap_file)
    req(!ucp1_clearing())  # Don't process during clear
    ucp1_cap_data(NULL)
    
    tryCatch({
      lines <- readLines(input$ucp1_cap_file$datapath, warn = FALSE)
      data_lines <- lines[3:length(lines)]
      
      parsed <- strsplit(data_lines, ",")
      time_vals <- sapply(parsed, function(x) as.numeric(x[1]))
      intensity_vals <- sapply(parsed, function(x) as.numeric(x[2]))
      
      df <- data.frame(Time = time_vals, Intensity = intensity_vals)
      df <- na.omit(df)
      
      ucp1_cap_data(df)
    }, error = function(e) {
      ucp1_cap_data(list(error = conditionMessage(e)))
    })
  })
  
  # Detect plateaus in capacity calibration
  ucp1_cap_plateaus <- reactive({
    req(!ucp1_clearing())  # Skip if currently clearing
    req(ucp1_cap_data())
    d <- ucp1_cap_data()
    if (!is.null(d$error)) return(NULL)
    
    threshold <- max(d$Intensity, na.rm = TRUE) * 0.1
    d$is_gap <- d$Intensity < threshold
    rle_result <- rle(d$is_gap)
    segment_id <- rep(seq_along(rle_result$lengths), rle_result$lengths)
    d$segment <- segment_id
    
    plateau_segments <- unique(segment_id[!d$is_gap])
    
    plateau_stats <- lapply(plateau_segments, function(seg) {
      seg_data <- d[d$segment == seg & !d$is_gap, ]
      if (nrow(seg_data) < 3) return(NULL)
      
      list(
        segment = seg,
        n_points = nrow(seg_data),
        time_start = min(seg_data$Time, na.rm = TRUE),
        time_end = max(seg_data$Time, na.rm = TRUE),
        median_intensity = median(seg_data$Intensity, na.rm = TRUE),
        trimmed_mean = mean(seg_data$Intensity, trim = 0.1, na.rm = TRUE),
        sd = sd(seg_data$Intensity, na.rm = TRUE)
      )
    })
    
    plateau_stats <- plateau_stats[!sapply(plateau_stats, is.null)]
    if (length(plateau_stats) == 0) return(NULL)
    
    plateau_df <- do.call(rbind, lapply(plateau_stats, function(x) {
      data.frame(
        plateau_number = which(sapply(plateau_stats, function(y) y$segment == x$segment)),
        segment = x$segment,
        n_points = x$n_points,
        time_start = x$time_start,
        time_end = x$time_end,
        median_intensity = x$median_intensity,
        trimmed_mean = x$trimmed_mean,
        sd = x$sd
      )
    }))
    
    list(data_with_segments = d, plateau_summary = plateau_df)
  })
  
  output$ucp1_cap_status <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_cap_data())
    d <- ucp1_cap_data()
    if (!is.null(d$error)) {
      div(class = "status-pill error", div(class = "dot"), 
          paste("Error:", d$error))
    } else {
      n_points <- nrow(d)
      t_range <- range(d$Time, na.rm = TRUE)
      
      plat <- ucp1_cap_plateaus()
      if (!is.null(plat)) {
        n_plateaus <- nrow(plat$plateau_summary)
        div(
          div(class = "status-pill ready", div(class = "dot"),
              sprintf("%d points | %.1f - %.1f s", n_points, t_range[1], t_range[2])),
          div(class = "status-pill ready", div(class = "dot"),
              sprintf("%d plateaus detected", n_plateaus))
        )
      } else {
        div(class = "status-pill ready", div(class = "dot"),
            sprintf("%d points | %.1f - %.1f s", n_points, t_range[1], t_range[2]))
      }
    }
  })
  
  output$ucp1_cap_plateau_info <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    plat <- ucp1_cap_plateaus()
    if (is.null(plat)) {
      div(style = "color: #7A8FAD; font-size: 0.9em; padding: 0.5rem;",
          "Upload capacity calibration data to detect plateaus")
    } else {
      div(style = "color: #22C55E; font-size: 0.9em; padding: 0.5rem;",
          sprintf("✓ %d plateau regions detected", nrow(plat$plateau_summary)))
    }
  })
  
  output$ucp1_cap_plateau_selector <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    plat <- ucp1_cap_plateaus()
    if (is.null(plat)) return(NULL)
    
    n_total <- nrow(plat$plateau_summary)
    default_n <- n_total  # Default: use ALL plateaus
    
    tagList(
      div(style = "margin-top: 1rem;"),
      numericInput("ucp1_n_cap_plateaus", 
                   "Number of plateaus to use for calibration",
                   value = default_n,
                   min = 2,
                   max = n_total,
                   step = 1),
      div(style = "color: #7A8FAD; font-size: 0.85em; margin-top: -0.5rem;",
          sprintf("Total detected: %d | Using all plateaus", n_total))
    )
  })
  
  output$ucp1_cap_plateau_table <- renderTable({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_cap_plateaus())
    plat <- isolate(ucp1_cap_plateaus())  # isolate to prevent excessive re-renders
    
    df <- plat$plateau_summary
    df$time_range <- sprintf("%.1f - %.1f s", df$time_start, df$time_end)
    
    data.frame(
      "Plateau" = df$plateau_number,
      "Time Range" = df$time_range,
      "Median Intensity" = round(df$median_intensity, 2),
      "Trimmed Mean" = round(df$trimmed_mean, 2),
      "SD" = round(df$sd, 2),
      "Points" = df$n_points,
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%", align = "c")
  

  output$ucp1_cap_placeholder <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    if (is.null(ucp1_cap_data())) {
      div(class = "plot-placeholder", 
          div(class = "icon", "📈"),
          "Upload capacity calibration CSV to visualize")
    }
  })
  
  output$ucp1_cap_plot <- renderPlot({
    req(!ucp1_clearing())  # Skip rendering during clear
    req(ucp1_cap_data())
    d <- ucp1_cap_data()
    if (!is.null(d$error)) return(NULL)
    
    lw <- if (!is.null(isolate(input$ucp1_line_width))) isolate(input$ucp1_line_width) else 1
    show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
    
    cap_title <- if (show_titles) {
      if (!is.null(isolate(input$ucp1_capacity_trace_title)) && nchar(isolate(input$ucp1_capacity_trace_title)) > 0) {
        isolate(input$ucp1_capacity_trace_title)
      } else {
        "Intensity (a.u.)"
      }
    } else {
      NULL
    }
    
    ggplot(d, aes(x = Time, y = Intensity)) +
      geom_line(colour = "#2E5CB8", linewidth = lw) +
      scale_x_continuous(expand = c(0, 0), 
                         breaks = scales::pretty_breaks(n = 6)) +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.05)),
                         breaks = scales::pretty_breaks(n = 7)) +
      labs(title = cap_title,
           x = "Time (s)",
           y = "Intensity (a.u.)") +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, colour = "#E8F0FE"),
        axis.title = element_text(face = "bold", size = 11, colour = "#E8F0FE"),
        axis.text = element_text(size = 10, colour = "#7A8FAD"),
        axis.line = element_line(colour = "#7A8FAD", linewidth = 0.6),
        panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.3),
        plot.background = element_rect(fill = "#0F1623", colour = NA),
        panel.background = element_rect(fill = "#0F1623", colour = NA)
      )
  }, bg = "#0F1623")
  
  output$ucp1_cap_download <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    req(ucp1_cap_data())
    d <- ucp1_cap_data()
    if (!is.null(d$error)) return(NULL)
    downloadButton("ucp1_cap_dl_png", "↓ PNG Plot", class = "btn-download")
  })
  
  output$ucp1_cap_dl_png <- downloadHandler(
    filename = function() paste0("UCP1_capacity_calibration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ucp1_cap_data())
      d <- ucp1_cap_data()
      lw <- if (!is.null(isolate(input$ucp1_line_width))) isolate(input$ucp1_line_width) else 1
      show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
      
      cap_title <- if (show_titles) {
        if (!is.null(isolate(input$ucp1_capacity_trace_title)) && nchar(isolate(input$ucp1_capacity_trace_title)) > 0) {
          isolate(input$ucp1_capacity_trace_title)
        } else {
          "Intensity (a.u.)"
        }
      } else {
        NULL
      }
      
      p <- ggplot(d, aes(x = Time, y = Intensity)) +
        geom_line(colour = "#2E5CB8", linewidth = lw) +
        scale_x_continuous(expand = c(0, 0),
                           breaks = scales::pretty_breaks(n = 6)) +
        scale_y_continuous(limits = c(0, NA),
                           expand = expansion(mult = c(0, 0.05)),
                           breaks = scales::pretty_breaks(n = 7)) +
        labs(title = cap_title,
             x = "Time (s)",
             y = "Intensity (a.u.)") +
        theme_classic(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 11),
          axis.text = element_text(size = 10, colour = "black"),
          axis.line = element_line(colour = "black", linewidth = 0.6),
          panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(file, p, width = 8, height = 5, dpi = 300, bg = "white")
    }
  )
  
  # Capacity calibration curve (Total SPQ vs FU)
  ucp1_cap_calibration <- reactive({
    req(!ucp1_clearing())  # Skip if currently clearing
    req(ucp1_cap_plateaus())
    plat <- ucp1_cap_plateaus()
    req(input$ucp1_n_cap_plateaus)
    
    # Use selected number of plateaus
    n_use <- as.integer(input$ucp1_n_cap_plateaus)
    n_total <- nrow(plat$plateau_summary)
    
    if (n_use < 2 || n_use > n_total) return(NULL)
    
    # SPQ series: 0, 1, 2, 3, 4, 5... (µM cumulative)
    total_spq <- seq(0, n_use - 1, by = 1)
    
    fu_vals <- plat$plateau_summary$median_intensity[1:n_use]
    
    calib_df <- data.frame(Total_SPQ = total_spq, FU = fu_vals)
    
    lm_fit <- lm(FU ~ Total_SPQ, data = calib_df)
    slope <- coef(lm_fit)[2]
    intercept <- coef(lm_fit)[1]
    r_squared <- summary(lm_fit)$r.squared
    
    list(
      data = calib_df,
      model = lm_fit,
      slope = slope,
      intercept = intercept,
      r_squared = r_squared,
      n_used = n_use
    )
  })
  
  # Capacity calibration summary values
  ucp1_cap_summary <- reactive({
    req(!ucp1_clearing())  # Skip if currently clearing
    req(ucp1_cap_calibration())
    req(ucp1_cap_plateaus())
    
    cap_cal <- ucp1_cap_calibration()
    plateaus <- ucp1_cap_plateaus()$plateau_summary
    
    # FU/µM = slope of capacity calibration
    fu_per_um <- cap_cal$slope
    
    # Initial FU = intensity of first plateau
    initial_fu <- plateaus$median_intensity[1]
    
    # µM = Initial FU / (FU/µM)
    um <- initial_fu / fu_per_um
    
    # µL/75 µL sample = 500/(2000/µM)
    ul_per_75ul <- 500 / (2000 / um)
    
    data.frame(
      Parameter = c("FU/µM", "Initial FU", "µM", "µL/75 µL sample"),
      Value = c(fu_per_um, initial_fu, um, ul_per_75ul)
    )
  })
  
  output$ucp1_cap_calibcurve_ui <- renderUI({
    req(!ucp1_clearing())  # Skip during clear
    calib <- ucp1_cap_calibration()
    
    if (is.null(calib)) {
      div(class = "plot-placeholder", 
          div(class = "icon", "📉"),
          "Configure plateau selection to generate calibration")
    } else {
      cap_sum <- ucp1_cap_summary()
      
      tagList(
        plotOutput("ucp1_cap_calibcurve_plot", height = "300px"),
        div(style = "background: #1E2D45; padding: 0.6rem; border-radius: 8px; margin-top: 0.5rem;",
          fluidRow(
            column(6,
              div(style = "font-weight: bold; color: #E8F0FE; margin-bottom: 0.5rem;", "Calibration Parameters"),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("Slope (a): %.3f", calib$slope)),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("Intercept (b): %.3f", calib$intercept)),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("R² = %.4f", calib$r_squared))
            ),
            column(6,
              div(style = "font-weight: bold; color: #E8F0FE; margin-bottom: 0.5rem;", "Summary Values"),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("FU/µM: %.2f", cap_sum$Value[1])),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("Initial FU: %.2f", cap_sum$Value[2])),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("µM: %.2f", cap_sum$Value[3])),
              div(style = "color: #7A8FAD; font-size: 0.9em;",
                sprintf("µL/75 µL sample: %.2f", cap_sum$Value[4]))
            )
          )
        ),
        div(style = "margin-top: 0.5rem;",
          downloadButton("ucp1_cap_calib_dl_png", "↓ PNG Calibration Curve", class = "btn-download")
        )
      )
    }
  })
  
  output$ucp1_cap_calibcurve_plot <- renderPlot({
    req(!ucp1_clearing())  # Skip rendering during clear
    req(ucp1_cap_calibration())
    calib <- ucp1_cap_calibration()
    show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
    
    cap_curve_title <- if (show_titles) {
      if (!is.null(isolate(input$ucp1_capacity_curve_title)) && nchar(isolate(input$ucp1_capacity_curve_title)) > 0) {
        isolate(input$ucp1_capacity_curve_title)
      } else {
        "Internal Volume Calibration"
      }
    } else {
      NULL
    }
    
    p <- ggplot(calib$data, aes(x = Total_SPQ, y = FU)) +
      geom_point(size = 3, colour = "#2E5CB8") +
      geom_smooth(method = "lm", se = FALSE, colour = "#2E5CB8", linewidth = 1) +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      annotate("text", x = Inf, y = Inf, 
               label = sprintf("y = %.3fx + %.3f\nR² = %.4f", 
                             calib$slope, calib$intercept, calib$r_squared),
               hjust = 1.1, vjust = 1.5, size = 4, colour = "#E8F0FE") +
      labs(title = cap_curve_title,
           x = "Total SPQ (µM)",
           y = "FU") +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, colour = "#E8F0FE"),
        axis.title = element_text(face = "bold", size = 11, colour = "#E8F0FE"),
        axis.text = element_text(size = 10, colour = "#7A8FAD"),
        axis.line = element_line(colour = "#7A8FAD", linewidth = 0.6),
        panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.3),
        plot.background = element_rect(fill = "#0F1623", colour = NA),
        panel.background = element_rect(fill = "#0F1623", colour = NA)
      )
    
    p
  }, bg = "#0F1623")
  
  output$ucp1_cap_calib_dl_png <- downloadHandler(
    filename = function() paste0("UCP1_capacity_curve_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ucp1_cap_calibration())
      calib <- ucp1_cap_calibration()
      show_titles <- if (!is.null(input$ucp1_show_titles)) input$ucp1_show_titles else TRUE
      
      cap_curve_title <- if (show_titles) {
        if (!is.null(isolate(input$ucp1_capacity_curve_title)) && nchar(isolate(input$ucp1_capacity_curve_title)) > 0) {
          isolate(input$ucp1_capacity_curve_title)
        } else {
          "Internal Volume Calibration"
        }
      } else {
        NULL
      }
      
      p <- ggplot(calib$data, aes(x = Total_SPQ, y = FU)) +
        geom_point(size = 3, colour = "#2E5CB8") +
        geom_smooth(method = "lm", se = FALSE, colour = "#2E5CB8", linewidth = 1) +
        scale_x_continuous(expansion(mult = c(0.05, 0.05))) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
        annotate("text", x = Inf, y = Inf, 
                 label = sprintf("y = %.3fx + %.3f\nR² = %.4f", 
                               calib$slope, calib$intercept, calib$r_squared),
                 hjust = 1.1, vjust = 1.5, size = 4, colour = "black") +
        labs(title = cap_curve_title,
             x = "Total SPQ (µM)",
             y = "FU") +
        theme_classic(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 11),
          axis.text = element_text(size = 10, colour = "black"),
          axis.line = element_line(colour = "black", linewidth = 0.6),
          panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(file, p, width = 6, height = 5, dpi = 300, bg = "white")
    }
  )

  # ==========================================================================
  # AKTA
  # ==========================================================================
  akta_results    <- reactiveVal(NULL)
  akta_raw_data   <- reactiveVal(NULL)   # store parsed UV data for integration
  akta_history    <- reactiveVal(list())
  akta_annotation <- reactiveVal(NULL)   # stores integration result for plot annotation

  # Format-aware UV trace extractor used by integration and CSV export.
  # Handles both UTF-16LE matrix format and UTF-8 columnar format.
  # Returns data.frame(vol, uv) or NULL on failure.
  parse_akta_uv_trace <- function(fp) {
    tryCatch({
      # Detect format by checking row 2 for known columnar trace-type labels
      first_lines <- readLines(fp, n = 3, encoding = "UTF-8", warn = FALSE)
      is_columnar <- length(first_lines) >= 2 &&
                     grepl("UV|Fraction|Cond", first_lines[2], ignore.case = FALSE)

      if (is_columnar) {
        raw         <- utils::read.csv(fp, header = FALSE, stringsAsFactors = FALSE,
                          check.names = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "")
        trace_types <- trimws(as.character(raw[2, ]))
        data_rows   <- raw[4:nrow(raw), ]
        uv_col      <- which(trace_types == "UV")[1]
        if (is.na(uv_col)) return(NULL)
        vols <- suppressWarnings(as.numeric(as.character(data_rows[[uv_col]])))
        uvs  <- suppressWarnings(as.numeric(as.character(data_rows[[uv_col + 1]])))
        ok   <- !is.na(vols) & !is.na(uvs)
        if (sum(ok) == 0) return(NULL)
        return(data.frame(vol = vols[ok], uv = uvs[ok]))
      }

      # Original matrix format (UTF-16LE tab-separated)
      raw    <- utils::read.delim(fp, header = FALSE, sep = "\t",
                  fileEncoding = "UTF-16LE", stringsAsFactors = FALSE, check.names = FALSE)
      aktlst <- t(as.matrix(raw))
      n_rows <- nrow(aktlst); n_cols <- ncol(aktlst)
      for (r in seq_len(n_rows)) {
        lbl <- trimws(as.character(aktlst[r, 2]))
        if (is.na(lbl) || lbl == "") next
        if (lbl %in% c("UV 1_280", "UV")) {
          x_v <- suppressWarnings(as.numeric(aktlst[r,     4:n_cols]))
          y_v <- suppressWarnings(as.numeric(aktlst[r + 1, 4:n_cols]))
          x_v <- x_v[!is.na(x_v)]; y_v <- y_v[!is.na(y_v)]
          m   <- min(length(x_v), length(y_v))
          if (m > 0) return(data.frame(vol = x_v[1:m], uv = y_v[1:m]))
        }
      }
      NULL
    }, error = function(e) NULL)
  }

  
  # Clear all ÄKTA data
  observeEvent(input$akta_clear, {
    akta_results(NULL)
    akta_raw_data(NULL)
    akta_history(list())
    akta_annotation(NULL)
    shinyjs::reset("akta_files")
    shinyjs::reset("akta_custom_names")
    shinyjs::reset("akta_vol_min")
    shinyjs::reset("akta_vol_max")
    shinyjs::reset("akta_mode")
    shinyjs::reset("akta_baseline_auto")
    showNotification("ÄKTA data cleared", type = "message", duration = 2)
  })

  output$akta_file_status <- renderUI({
    req(input$akta_files)
    n <- nrow(input$akta_files)
    div(class = "status-pill ready", div(class = "dot"),
      sprintf("%d file%s loaded", n, if (n > 1) "s" else ""))
  })

  output$akta_file_list_ui <- renderUI({
    req(input$akta_files)
    if (nrow(input$akta_files) <= 1) return(NULL)
    div(class = "lab-card",
      div(class = "lab-card-title", "📁  Loaded files (overlay order)"),
      lapply(seq_len(nrow(input$akta_files)), function(i) {
        div(style = "display:flex;align-items:center;gap:.6rem;padding:.3rem 0;",
          tags$span(style = "font-family:'JetBrains Mono',monospace;font-size:.72rem;background:#161E2E;color:#00C2FF;padding:.1rem .4rem;border-radius:4px;", as.character(i)),
          tags$span(input$akta_files$name[i], style = "font-size:.83rem;color:#E8F0FE;")
        )
      })
    )
  })

  output$akta_plot_placeholder <- renderUI({
    if (is.null(akta_results()))
      div(class = "plot-placeholder", div(class = "icon", "📊"), "Upload file(s) and click Generate Plot")
  })

  observeEvent(input$akta_run, {
    req(input$akta_files)
    akta_results(NULL)

    # Parse highlight fractions
    highlight <- NULL
    hl_raw <- trimws(input$akta_highlight)
    if (nchar(hl_raw) > 0) {
      raw_fracs <- trimws(strsplit(hl_raw, "[,;]+")[[1]])
      raw_fracs <- raw_fracs[nchar(raw_fracs) > 0]
      if (length(raw_fracs) > 0) {
        highlight <- unique(unlist(lapply(raw_fracs, function(f) {
          if (grepl("\\.", f)) return(f)
          m <- regexpr("^([A-Za-z]+)([0-9]+)$", f)
          if (m == -1) return(f)
          letter <- regmatches(f, regexpr("[A-Za-z]+", f))
          number <- regmatches(f, regexpr("[0-9]+", f))
          c(f, paste0(1:5, ".", letter, ".", number))
        })))
      }
    }

    # Volume range
    vol_range <- NULL
    if (!is.na(input$akta_vol_min) && !is.na(input$akta_vol_max))
      vol_range <- c(input$akta_vol_min, input$akta_vol_max)

    # Rename temp files to original names for legend
    renamed <- file.path(
      dirname(input$akta_files$datapath),
      tools::file_path_sans_ext(input$akta_files$name)
    )
    for (i in seq_along(renamed))
      if (!file.exists(renamed[i])) file.copy(input$akta_files$datapath[i], renamed[i])

    # Apply custom sample name overrides
    custom_names_raw <- trimws(input$akta_custom_names)
    if (nchar(custom_names_raw) > 0) {
      cnames <- trimws(strsplit(custom_names_raw, "\n")[[1]])
      cnames <- cnames[nchar(cnames) > 0]
      for (i in seq_along(cnames)) {
        if (i <= length(renamed)) {
          new_path <- file.path(dirname(renamed[i]), cnames[i])
          if (!file.exists(new_path)) file.copy(renamed[i], new_path)
          renamed[i] <- new_path
        }
      }
    }

    withProgress(message = "Generating plot…", value = 0, {
      tryCatch({
        incProgress(0.4, detail = "Reading files…")
        p <- plot_akta_improved(
          files               = renamed,
          volume_range        = vol_range,
          show_fractions      = input$akta_show_fractions,
          show_conductance    = input$akta_show_cond,
          show_uv260          = input$akta_show_uv260,
          show_percent_b      = input$akta_show_pctb,
          highlight_fractions = highlight,
          save_plot           = FALSE,
          theme               = input$akta_theme,
          line_width          = input$akta_linewidth
        )
        incProgress(0.5, detail = "Rendering…")
        akta_results(list(plot = p, file_names = input$akta_files$name, renamed = renamed))

        # Add to history
        entry <- list(
          time  = format(Sys.time(), "%H:%M:%S"),
          files = paste(input$akta_files$name, collapse = ", "),
          n     = nrow(input$akta_files),
          vol   = if (!is.null(vol_range)) sprintf("%.0f-%.0f mL", vol_range[1], vol_range[2]) else "full"
        )
        akta_history(c(akta_history(), list(entry)))

      }, error = function(e) {
        showNotification(paste("ÄKTA error:", conditionMessage(e)), type = "error", duration = 12)
      })
    })
  })

  output$akta_plot <- renderPlot({
    req(akta_results())
    p   <- akta_results()$plot
    ann <- akta_annotation()

    # Add annotation layer if user has pressed "Add to Plot"
    if (!is.null(ann)) {
      lbl <- sprintf("Ve = %.2f mL\n%.1f kDa", ann$centroid, ann$mw_kda)
      p <- p +
        geom_vline(xintercept = ann$centroid,
                   colour = "#FF7B47", linewidth = 0.7, linetype = "dashed") +
        annotate("label",
                 x = ann$centroid, y = Inf,
                 label = lbl, vjust = 1.3, size = 3.5,
                 colour = "#FF7B47", fill = "#0F1623",
                 label.size = 0.3, label.padding = unit(0.25, "lines"))
    }

    p + theme(
      plot.background  = element_rect(fill = "#0F1623", colour = NA),
      panel.background = element_rect(fill = "#0F1623", colour = NA),
      panel.grid.major = element_line(colour = "#1E2D45", linewidth = 0.4),
      panel.grid.minor = element_line(colour = "#161E2E", linewidth = 0.2),
      text             = element_text(colour = "#E8F0FE"),
      axis.text        = element_text(colour = "#7A8FAD"),
      axis.line        = element_line(colour = "#243652"),
      axis.ticks       = element_line(colour = "#243652"),
      plot.title       = element_text(face = "bold", colour = "#E8F0FE"),
      plot.subtitle    = element_text(colour = "#7A8FAD"),
      legend.background = element_rect(fill = "#0F1623", colour = "#1E2D45"),
      legend.text       = element_text(colour = "#E8F0FE"),
      legend.title      = element_text(colour = "#7A8FAD")
    )
  }, bg = "#0F1623")

  # ---- Peak Integration -----------------------------------------------------
  # ---- Peak Integration -------------------------------------------------------

  # Core reactive — parses UV, integrates, computes centroid + MW
  # Single source of truth used by display, actions, export, and annotation
  akta_integration <- reactive({
    req(input$akta_files)
    v_start <- input$akta_int_start
    v_end   <- input$akta_int_end
    if (is.na(v_start) || is.na(v_end) || v_start >= v_end) return(NULL)

    void_vol  <- if (is.na(input$akta_void_vol))  8.23  else input$akta_void_vol
    total_vol <- if (is.na(input$akta_total_vol)) 24.00 else input$akta_total_vol

    tryCatch({
      fp    <- input$akta_files$datapath[1]
      uv_df <- parse_akta_uv_trace(fp)
      if (is.null(uv_df)) return(list(error = "Could not read UV trace for integration."))

      roi   <- uv_df[uv_df$vol >= v_start & uv_df$vol <= v_end, ]
      total <- uv_df[uv_df$uv >= 0, ]
      if (nrow(roi) < 2) return(list(error = "Not enough data points in integration range."))

      # Trapezoidal area + purity
      area_roi   <- sum(diff(roi$vol) * (head(roi$uv, -1) + tail(roi$uv, -1))) / 2
      area_total <- sum(diff(total$vol) * (head(total$uv, -1) + tail(total$uv, -1))) / 2
      purity     <- if (area_total > 0) 100 * area_roi / area_total else NA

      # Centroid elution volume — UV-signal-weighted mean (same logic as CPM Tm centroid)
      uv_pos   <- pmax(roi$uv, 0)
      centroid <- if (sum(uv_pos) > 0) sum(roi$vol * uv_pos) / sum(uv_pos) else mean(c(v_start, v_end))

      # MW from SEC calibration formula (from column calibration Excel)
      norm_ve <- (centroid - void_vol) / (total_vol - void_vol)
      mw_da   <- 10 ^ (-3.22448969353114 * norm_ve + 5.92750021160459)

      list(
        error     = NULL,
        area      = area_roi,
        purity    = purity,
        centroid  = centroid,
        mw_da     = mw_da,
        mw_kda    = mw_da / 1000,
        v_start   = v_start,
        v_end     = v_end,
        n_points  = nrow(roi),
        void_vol  = void_vol,
        total_vol = total_vol
      )
    }, error = function(e) list(error = conditionMessage(e)))
  })

  # Results display — four badges
  output$akta_integration_result <- renderUI({
    res <- akta_integration()
    if (is.null(res)) return(NULL)
    if (!is.null(res$error))
      return(div(class = "warn-box", res$error))

    tagList(
      fluidRow(
        column(6,
          div(class = "result-badge",
            div(class = "result-label", "Peak Area"),
            div(class = "result-value", sprintf("%.1f", res$area))
          )
        ),
        column(6,
          div(class = "result-badge",
            div(class = "result-label", "Purity"),
            div(class = "result-value green", sprintf("%.1f%%", res$purity))
          )
        )
      ),
      fluidRow(
        column(6,
          div(class = "result-badge",
            div(class = "result-label", "Elution Volume"),
            div(class = "result-value orange", sprintf("%.2f mL", res$centroid))
          )
        ),
        column(6,
          div(class = "result-badge",
            div(class = "result-label", "Est. MW"),
            div(class = "result-value purple", sprintf("%.1f kDa", res$mw_kda))
          )
        )
      ),
      div(style = "font-size:0.72rem;color:var(--muted);margin-top:0.3rem;",
        sprintf("Range: %.1f\u2013%.1f mL  (%d points)", res$v_start, res$v_end, res$n_points))
    )
  })

  # Action buttons — only shown when valid results exist
  output$akta_integration_actions <- renderUI({
    res <- akta_integration()
    if (is.null(res) || !is.null(res$error)) return(NULL)
    tagList(
      actionButton("akta_annotate_plot", "\U1F4CC  Add to Plot", class = "btn-run",
        style = "font-size:0.78rem;padding:0.45rem 0.9rem;width:auto;"),
      " ",
      downloadButton("akta_int_csv", "\u2193 Export Table", class = "btn-download")
    )
  })

  # Annotate button — snapshot current integration result into annotation state
  observeEvent(input$akta_annotate_plot, {
    res <- akta_integration()
    if (!is.null(res) && is.null(res$error)) {
      akta_annotation(res)
      showNotification(
        "\u2713 Annotation set — regenerate the plot to see it on the chromatogram.",
        type = "message", duration = 4)
    }
  })

  # Integration CSV export
  output$akta_int_csv <- downloadHandler(
    filename = function() paste0("AKTA_integration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      res      <- akta_integration()
      req(!is.null(res) && is.null(res$error))
      run_name <- if (!is.null(input$akta_files)) input$akta_files$name[1] else "unknown"
      write.csv(data.frame(
        Run            = run_name,
        Range_start_mL = res$v_start,
        Range_end_mL   = res$v_end,
        Peak_area      = round(res$area,     3),
        Purity_pct     = round(res$purity,   2),
        Elution_vol_mL = round(res$centroid, 3),
        MW_kDa         = round(res$mw_kda,   2),
        MW_Da          = round(res$mw_da,    0),
        Void_vol_mL    = res$void_vol,
        Total_vol_mL   = res$total_vol,
        Date           = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ), file, row.names = FALSE)
      showNotification("\u2713 Integration table exported!", type = "message", duration = 3)
    }
  )

  # ---- AKTA Downloads -------------------------------------------------------
  output$akta_download_buttons <- renderUI({
    req(akta_results())
    tagList(
      downloadButton("akta_dl_png", "\u2193 PNG Plot",       class = "btn-download"), " ",
      downloadButton("akta_dl_pdf", "\u2193 PDF Plot",       class = "btn-download"), " ",
      downloadButton("akta_dl_csv", "\u2193 CSV Trace Data", class = "btn-download")
    )
  })

  dl_akta_plot <- function(file, device, bg) {
    req(akta_results())
    p   <- akta_results()$plot
    ann <- akta_annotation()

    if (!is.null(ann)) {
      lbl <- sprintf("Ve = %.2f mL\n%.1f kDa", ann$centroid, ann$mw_kda)
      p <- p +
        geom_vline(xintercept = ann$centroid,
                   colour = "#E07030", linewidth = 0.7, linetype = "dashed") +
        annotate("label",
                 x = ann$centroid, y = Inf,
                 label = lbl, vjust = 1.3, size = 3.5,
                 colour = "#E07030", fill = "white",
                 label.size = 0.3, label.padding = unit(0.25, "lines"))
    }

    p <- p + theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      text              = element_text(colour = "black"),
      axis.text         = element_text(colour = "black"),
      panel.grid.major  = element_line(colour = "grey90"),
      legend.background = element_rect(fill = "white"))
    ggsave(file, p, width = 12, height = 6, dpi = 300, device = device, bg = bg)
  }

  output$akta_dl_png <- downloadHandler(
    filename = function() paste0("AKTA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content  = function(f) dl_akta_plot(f, "png", "white"))
  output$akta_dl_pdf <- downloadHandler(
    filename = function() paste0("AKTA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content  = function(f) dl_akta_plot(f, "pdf", NULL))

  # CSV export: UV 280 trace for every uploaded file as a wide table
  output$akta_dl_csv <- downloadHandler(
    filename = function() paste0("AKTA_traces_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(akta_results())
      req(input$akta_files)
      
      files     <- input$akta_files$datapath
      run_names <- tools::file_path_sans_ext(input$akta_files$name)
      
      # Parse UV 280 trace from each file — handles both UNICORN 7 export formats
      traces <- lapply(seq_along(files), function(i) {
        parse_akta_uv_trace(files[i])
      })
      
      # Merge all traces on Volume using a common grid (nearest-neighbour join)
      valid   <- Filter(Negate(is.null), traces)
      vnames  <- run_names[!sapply(traces, is.null)]
      
      if (length(valid) == 0) {
        write.csv(data.frame(Error = "No UV traces could be parsed"), file, row.names = FALSE)
        return()
      }
      
      # Use the volume axis of the first run as reference; interpolate others onto it
      ref_vol <- valid[[1]]$vol
      out     <- data.frame(Volume_mL = ref_vol)
      
      for (i in seq_along(valid)) {
        uv_interp <- approx(valid[[i]]$vol, valid[[i]]$uv, xout = ref_vol, rule = 2)$y
        out[[paste0("UV280_", vnames[i])]] <- round(uv_interp, 4)
      }
      
      write.csv(out, file, row.names = FALSE)
      showNotification("\u2713 Trace data exported!", type = "message", duration = 3)
    }
  )

  # ---- AKTA History ---------------------------------------------------------
  output$akta_history_ui <- renderUI({
    h <- akta_history()
    if (length(h) == 0)
      return(p("No plots generated yet.", style = "color:var(--muted);font-size:0.8rem;"))
    rows <- lapply(rev(h), function(e) {
      div(class = "history-row",
        style = "grid-template-columns: 70px 1fr 80px 80px;",
        div(class = "history-time", e$time),
        div(style = "font-size:0.75rem;color:var(--txt);overflow:hidden;text-overflow:ellipsis;white-space:nowrap;", e$files),
        div(style = "font-size:0.75rem;color:var(--muted);", paste0(e$n, " file(s)")),
        div(style = "font-size:0.75rem;color:var(--muted);", e$vol)
      )
    })
    do.call(div, rows)
  })

  # ==========================================================================
  # Export All Report
  # ==========================================================================
  output$export_report <- downloadHandler(
    filename = function() paste0("CrichtonGroup_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content  = function(file) {
      tmp <- tempdir()
      files_to_zip <- character()

      # BCA
      if (!is.null(bca_results())) {
        r <- bca_results()
        bca_csv <- file.path(tmp, "BCA_results.csv")
        write.csv(data.frame(Concentration_mgmL = r$conc, Volume_mL = r$vol,
          Total_yield_mg = r$yield, R_squared = r$curve$r2, Date = Sys.time()),
          bca_csv, row.names = FALSE)
        files_to_zip <- c(files_to_zip, bca_csv)
        bca_png <- file.path(tmp, "BCA_standard_curve.png")
        ggsave(bca_png, r$curve$plot + theme(
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          text = element_text(colour = "black"), axis.text = element_text(colour = "black"),
          panel.grid.major = element_line(colour = "grey90")),
          width = 10, height = 6, dpi = 300, bg = "white")
        files_to_zip <- c(files_to_zip, bca_png)

        # Results table image
        if (!is.null(r$gt)) {
          tryCatch({
            zip_title <- if (nchar(trimws(input$bca_title)) > 0) trimws(input$bca_title) else "BCA Assay Protein Yield Summary"
            zip_params <- c("Concentration (mg/mL)", "Sample volume (mL)", "Total yield (mg)", "R\u00b2 (standard curve)")
            zip_vals   <- c(sprintf("%.2f", r$conc), sprintf("%.2f", r$vol), sprintf("%.2f", r$yield), sprintf("%.4f", r$curve$r2))
            n_zip      <- length(zip_params)
            rh         <- 1.0
            ys_zip     <- seq(n_zip - 1, 0, by = -1) * rh
            y_hdr_zip  <- n_zip * rh
            y_rule_zip <- y_hdr_zip + rh
            y_ttl_zip  <- y_rule_zip + 0.35
            tt         <- 0.65
            df_zip     <- data.frame(y = ys_zip, param = zip_params, val = zip_vals)
            p_tbl <- ggplot() +
              annotate("text", x = 0, y = y_ttl_zip, label = zip_title,
                       hjust = 0, vjust = 0, size = 4.5, fontface = "bold", colour = "black") +
              annotate("segment", x = 0, xend = 1, y = y_rule_zip, yend = y_rule_zip,
                       colour = "black", linewidth = 0.9) +
              annotate("text", x = 0, y = y_hdr_zip + tt, label = "PARAMETER",
                       hjust = 0, vjust = 1, size = 2.9, colour = "grey45") +
              annotate("text", x = 1, y = y_hdr_zip + tt, label = "VALUE",
                       hjust = 1, vjust = 1, size = 2.9, colour = "grey45") +
              annotate("segment", x = 0, xend = 1, y = y_hdr_zip, yend = y_hdr_zip,
                       colour = "grey65", linewidth = 0.4) +
              geom_text(data = df_zip, aes(x = 0, y = y + tt, label = param),
                        hjust = 0, vjust = 1, size = 3.3, colour = "black") +
              geom_text(data = df_zip, aes(x = 1, y = y + tt, label = val),
                        hjust = 1, vjust = 1, size = 3.3, fontface = "bold",
                        colour = "black", family = "mono") +
              geom_segment(data = df_zip, aes(x = 0, xend = 1, y = y, yend = y),
                           colour = "grey88", linewidth = 0.35) +
              scale_x_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
              scale_y_continuous(limits = c(-0.05, y_ttl_zip + 0.5), expand = c(0, 0)) +
              theme_void() +
              theme(plot.background = element_rect(fill = "white", colour = NA),
                    plot.margin = margin(16, 24, 16, 24))
            bca_tbl_png <- file.path(tmp, "BCA_results_table.png")
            ggsave(bca_tbl_png, p_tbl, width=7, height=3.5, dpi=300, bg="white")
            files_to_zip <- c(files_to_zip, bca_tbl_png)
          }, error = function(e) NULL)
        }
      }

      # CPM single
      if (!is.null(cpm_results())) {
        res <- cpm_results()
        cpm_png <- file.path(tmp, "CPM_result.png")
        p <- res$res$plot + theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          text = element_text(colour = "black"), axis.text = element_text(colour = "black"),
          panel.grid.major = element_line(colour = "grey90"))
        ggsave(cpm_png, p, width = 12, height = 6, dpi = 300, bg = "white")
        files_to_zip <- c(files_to_zip, cpm_png)
      }


      # AKTA
      if (!is.null(akta_results())) {
        akta_png <- file.path(tmp, "AKTA_chromatogram.png")
        p <- akta_results()$plot + theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          text = element_text(colour = "black"), axis.text = element_text(colour = "black"),
          panel.grid.major = element_line(colour = "grey90"),
          legend.background = element_rect(fill = "white"))
        ggsave(akta_png, p, width = 12, height = 6, dpi = 300, bg = "white")
        files_to_zip <- c(files_to_zip, akta_png)
      }

      if (length(files_to_zip) == 0) {
        # Write a placeholder
        placeholder <- file.path(tmp, "README.txt")
        writeLines("No analyses have been run yet.", placeholder)
        files_to_zip <- c(files_to_zip, placeholder)
      }

      zip::zip(file, files = files_to_zip, mode = "cherry-pick")
    }
  )

} # end server

# -- 5. Launch ----------------------------------------------------------------
shinyApp(ui = ui, server = server)

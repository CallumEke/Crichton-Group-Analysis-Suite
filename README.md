# Crichton Group Analysis Suite

Shiny app providing analysis tools for the Crichton Group:
ÄKTA chromatography, BCA protein quantification, CPM thermostability (peak
picker, QC, contour plotting), gel/Western annotation, and UCP1 proton
conductance.

## Quick start

1. Open `app.R` in RStudio.
2. Click **Run App** (top right of the editor).

On first launch, any missing R packages are installed automatically. This
can take a few minutes the first time and is silent thereafter.

You should see lines like the following in the R console as the app starts:

```
[helper] OK: .../inst/analytics/softmax_bca_improved.R (3 fns)
[helper] OK: .../inst/analytics/tm_analysis_functions.R (3 fns)
[helper] OK: .../inst/analytics/plot_akta_improved.R (1 fns)
```

If any line says `FAILED` or `NOT FOUND`, follow the message — it names the
exact file or package that's the problem.

## Project layout

```
.
├── app.R                  navbar + module wiring (~110 lines)
├── global.R               packages, helper sourcing, module loading
├── R/
│   ├── utils_theme.R      shared ggplot themes + UI primitives
│   ├── mod_home.R         landing page
│   ├── mod_bca.R          BCA protein quantification     (migrated)
│   ├── mod_cpm.R          CPM peak picker                (migrated)
│   ├── mod_akta.R         ÄKTA chromatography            (skeleton)
│   ├── mod_cpm_qc.R       CPM QC                         (skeleton)
│   ├── mod_cpm_contour.R  CPM contour plotting           (skeleton)
│   ├── mod_gel.R          gel & Western annotator        (skeleton)
│   ├── mod_ucp1.R         UCP1 proton conductance        (skeleton)
│   └── mod_export.R       cross-tool export bundle
├── inst/analytics/        unchanged analytic helper functions
│   ├── softmax_bca_improved.R
│   ├── tm_analysis_functions.R
│   └── plot_akta_improved.R
├── www/
│   ├── custom.css         611 lines of CSS, now a static asset
│   └── custom.js          drag-and-drop visual feedback
├── MIGRATION_GUIDE.md     how to migrate the remaining skeletons
└── README.md              this file
```

## Adding a tool

1. Create `R/mod_yourtool.R` following the pattern in `mod_bca.R`.
2. Add the `source(...)` line to the loop in `global.R`.
3. Add a `nav_panel("Your Tool", value = "YOURTOOL", yourtool_ui("yourtool"))`
   entry to `app.R`.
4. Call `yourtool_server("yourtool")` in the server function in `app.R`.

See `MIGRATION_GUIDE.md` for full detail.

## Deployment

`rsconnect::deployApp(".")` works as before. The deployment scanner picks up
all `library()` calls automatically; you don't need to maintain a separate
dependency list.

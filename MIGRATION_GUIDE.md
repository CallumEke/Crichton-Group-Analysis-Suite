# Crichton Group Analysis Suite — Modular Refactor Guide

The original `app_v10l.R` is 7,452 lines in a single file. This refactor splits
it into one file per tool, with a slim ~140-line `app.R` that only wires the
pieces together. Behaviour is unchanged.

## Project structure

```
crichton-analysis-suite/
├── app.R                       # navbar + module wiring  (~140 lines)
├── global.R                    # packages, analytic helpers, module sourcing
├── R/
│   ├── utils_theme.R           # shared ggplot themes & UI primitives
│   ├── mod_home.R              # landing tool gallery
│   ├── mod_bca.R               # ✅ MIGRATED  (worked example)
│   ├── mod_cpm.R               # ✅ MIGRATED  (worked example)
│   ├── mod_akta.R              # ⏳ skeleton
│   ├── mod_cpm_qc.R            # ⏳ skeleton
│   ├── mod_cpm_contour.R       # ⏳ skeleton
│   ├── mod_gel.R               # ⏳ skeleton
│   ├── mod_ucp1.R              # ⏳ skeleton
│   └── mod_export.R            # cross-tool zip bundle
├── inst/analytics/             # unchanged — your original analytic helpers
│   ├── softmax_bca_improved.R
│   ├── tm_analysis_functions.R
│   └── plot_akta_improved.R
└── www/
    ├── custom.css              # 611 lines extracted from app_v10l.R
    └── custom.js               # drag-and-drop visual feedback
```

## What changed

1. **CSS and JS moved to `www/`** — they're now served as static files and
   loaded with a normal `<link>` and `<script>` tag in `app.R`. No more
   600 lines of CSS-in-an-R-string.

2. **Shared theme code lives in `utils_theme.R`** — `theme_cg_dark()`
   replaces the dozen places that inlined the dark ggplot styling, and
   `theme_cg_publication()` replaces the white-theme overrides repeated
   in every download handler.

3. **Each tool is a Shiny module** with:
    - `<tool>_ui(id)` — the tab contents, namespaced with `ns(id)`
    - `<tool>_server(id)` — the reactives, called via `moduleServer(id, ...)`
    - A returned `reactive()` exposing current results, so the cross-tool
      Export Report zip can collect them without reaching into private state.

4. **Result-bundle export is its own module** (`mod_export.R`). When a new
   tool is added, plug its results reactive into the `tools = list(...)`
   passed to `export_server()` and add a stanza to the zip handler.

5. **UI primitives are functions, not copy-paste.** `lab_card()`,
   `step_title(n, label)`, `status_pill()`, `info_box()`, `result_badge()`,
   `plot_placeholder()` — defined once in `utils_theme.R`. Compare the
   readability of the BCA UI in `mod_bca.R` to lines 786–963 in the old app.

## How to migrate one of the skeleton modules

Each skeleton file has a header with the exact line ranges in `app_v10l.R`
to copy from. The transformation is mechanical:

### 1. UI

Take the `nav_panel(...)` body from `app_v10l.R` and paste it into the
module's `*_ui(id)` function as the body of the `tagList(...)`. Then:

```
# Before (in app_v10l.R, no namespace)
fileInput("akta_files",  NULL, ...)
plotOutput("akta_plot",  height = "500px")
conditionalPanel("input.akta_mode == 'overlay'", ...)

# After (inside akta_ui(id))
ns <- shiny::NS(id)
shiny::fileInput(ns("files"), NULL, ...)
shiny::plotOutput(ns("plot"), height = "500px")
shiny::conditionalPanel(sprintf("input['%s'] == 'overlay'", ns("mode")), ...)
```

Three rules:
- Strip the tool prefix from IDs (`akta_files` → `files`) — the namespace
  re-adds it.
- Wrap every input/output ID in `ns()`.
- `conditionalPanel` JS conditions need the namespaced id with bracket
  syntax: `sprintf("input['%s'] == ...", ns("X"))`. Dot syntax doesn't
  parse namespaced ids.

### 2. Server

Take the corresponding server block and paste it into `*_server(id)` inside
`moduleServer(id, function(input, output, session) { ... })`. Then:

```
# Before
input$akta_files       -> input$files
output$akta_plot       -> output$plot
observeEvent(input$akta_run, ...)  ->  observeEvent(input$run, ...)
shinyjs::reset("akta_files")        -> shinyjs::reset("files")
```

Drop the tool prefix from `input$` and `output$` references — they
automatically resolve within the module's namespace.

`shinyjs::reset()` likewise takes the bare id; it's namespaced via the
`session` argument that `moduleServer` injects.

### 3. Return a results reactive

At the bottom of `*_server`, return a `reactive()` containing the latest
results. The export module reads this:

```r
shiny::reactive(my_results())   # where my_results is the reactiveVal
```

### 4. Wire it into `app.R`

Capture the returned reactive and pass it into `export_server`:

```r
akta_results <- akta_server("akta")
# ...
export_server("export", tools = list(
  bca  = bca_results,
  cpm  = cpm_results,
  akta = akta_results,
  # ...
))
```

Then in `mod_export.R`, add a stanza to the zip handler:

```r
if (!is.null(tools$akta)) {
  r <- tools$akta()
  if (!is.null(r)) {
    # ggsave the plot, write any csv, append paths to files_to_zip
  }
}
```

## Deployment

`rsconnect::deployApp(".")` works exactly as before. The directory layout
above is compatible with shinyapps.io and Posit Connect Cloud — both will
pick up `global.R` automatically, source it, and run `app.R`. No
`rsconnect.json` changes needed.

## Suggested migration order

1. **mod_akta.R** (cleanest, similar pattern to BCA — small state, no
   sub-tabs). Good warm-up.
2. **mod_cpm_qc.R** (two sibling sub-modes; can keep one module with a
   mode selector).
3. **mod_cpm_contour.R** (largest control surface but linear flow).
4. **mod_ucp1.R** (multi-stage; pace yourself, ~500 lines of server).
5. **mod_gel.R** (heaviest — magick image state. Migrate last when
   you're comfortable with the pattern.)

Doing one tool per evening session is realistic. Each migration is
mechanical at this point; the architectural thinking is done.

## Optional next steps (after migration is complete)

- **Tests for the analytic functions.** `calculate_tm()`,
  `create_standard_curve()`, the RotorGene parser — all pure functions,
  trivial to test with `testthat`. Put fixtures in `tests/testthat/data/`.
  This is where bugs are most dangerous (wrong Tm = wrong science).
- **Package the project with `{golem}` or `{leprechaun}`** if you want a
  standard package layout. Not necessary, but it's free CI, documentation,
  and a clearer dependency story.
- **CI on GitHub.** `usethis::use_github_action_check_standard()` runs the
  tests on every push.

But none of these are blockers — once the modules are split, the app
keeps working exactly as it does today, and future tools can be added
as one new file in `R/` plus one line in `app.R`.

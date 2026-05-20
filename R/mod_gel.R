################################################################################
#  mod_gel.R  --  Gel Annotator + Western Blot (Shiny module)
################################################################################
#
#  Migrated from app_v10l.R:
#    UI:     lines 1409 - 1659
#    Server: lines 1830 - 2527  (gel)
#            lines 3267 - 3744  (western blot)
#            lines 3747 - 3851  (crop handlers, gel + western)
#
#  Two sub-tabs:
#    - Gel:     click-to-place ladder/well markers, optional crop, export
#               as labelled PNG/TIFF with positionable kDa labels and
#               rotatable well labels.
#    - Western: same load/crop/ladder-marker flow as gel, plus a "stitch"
#               export that stacks the gel above the western, with an
#               antibody label on the right.
#
#  Render code is parameterised via `.gel_render_annotated()` so the four
#  output paths (preview, PNG export, TIFF export, stitch PNG inner)
#  produce identical layouts. The original duplicated this code; here it
#  lives in one place.
#
#  All marker state is held in one `reactiveValues` bundle (`state`).
#  Markers carry an id so per-row delete buttons work via dynamic input
#  watchers.
#
################################################################################

# Ladder presets - molecular weights in kDa, in band order
.GEL_LADDER_PRESETS <- list(
  pageruler  = c(250, 130, 100, 70, 55, 35, 25, 15, 10),
  precision  = c(250, 150, 100, 75, 50, 37, 25, 20, 15, 10),
  prestained = c(245, 180, 135, 100, 75, 63, 48, 35, 25, 20, 17, 11),
  broad      = c(200, 116, 97, 66, 45, 31, 21, 14, 6),
  custom     = c()
)


# -- UI -----------------------------------------------------------------------
gel_ui <- function(id) {
  ns <- shiny::NS(id)

  # Mode-toggle JS - same pattern as CPM QC. Picks 'gel' or 'western'.
  mode_input   <- ns("tab_mode")
  btn_gel      <- ns("btn_gel")
  btn_western  <- ns("btn_western")
  js_gel <- sprintf(
    "Shiny.setInputValue('%s', 'gel', {priority: 'event'});
     document.getElementById('%s').classList.add('active');
     document.getElementById('%s').classList.remove('active');",
    mode_input, btn_gel, btn_western)
  js_western <- sprintf(
    "Shiny.setInputValue('%s', 'western', {priority: 'event'});
     document.getElementById('%s').classList.add('active');
     document.getElementById('%s').classList.remove('active');",
    mode_input, btn_western, btn_gel)
  cond_gel     <- sprintf("input['%s'] == 'gel' || input['%s'] == null || input['%s'] == undefined",
                          mode_input, mode_input, mode_input)
  cond_western <- sprintf("input['%s'] == 'western'", mode_input)

  shiny::tagList(
    shiny::div(style = "display: none;",
      shiny::actionButton(ns("clear"), "\U0001f504  Clear All Data", class = "btn-clear")
    ),

    shiny::div(style = "padding:0 1rem 0.5rem;",
      shiny::div(class = "qc-mode-toggle",
        shiny::tags$button("Gel",     id = btn_gel,     class = "qc-mode-btn active", onclick = js_gel),
        shiny::tags$button("Western", id = btn_western, class = "qc-mode-btn",        onclick = js_western)
      )
    ),

    # =====================================================================
    # GEL TAB
    # =====================================================================
    shiny::conditionalPanel(condition = cond_gel,
      shiny::div(class = "sticky-tool",
        shiny::fluidRow(
          # ----- Left column: workflow controls (scrolls) ---------------
          shiny::column(3,
            shiny::div(class = "workflow-col",
              lab_card(
                step_title(1, "Upload Gel Image"),
                info_box("Upload TIFF, PNG, or JPEG gel images. TIFF files are fully supported."),
                shiny::fileInput(ns("image"), NULL,
                                 accept = c(".tif",".tiff",".png",".jpg",".jpeg"),
                                 buttonLabel = "Browse\u2026",
                                 placeholder = "No file selected")
              ),
              lab_card(
                step_title(2, "Crop (Optional)"),
                info_box("Drag on the gel image to select crop area. Release, then click Apply to confirm."),
                shiny::fluidRow(
                  shiny::column(6, shiny::actionButton(ns("apply_crop"),
                    "\u2713 Apply Crop", class = "btn-run", style = "width:100%;")),
                  shiny::column(6, shiny::actionButton(ns("revert_crop"),
                    "\u21a9 Revert Original", class = "btn-secondary", style = "width:100%;"))
                ),
                shiny::uiOutput(ns("crop_status"))
              ),
              lab_card(
                step_title(3, "Mark Features"),
                info_box("Click on the image to mark ladder bands or sample wells."),
                shiny::selectInput(ns("mode"), "Marking Mode",
                  choices = c("Crop" = "crop", "Ladder Bands" = "ladder",
                              "Sample Wells" = "wells"),
                  selected = "crop")
              ),
              lab_card(
                step_title(4, "Ladder Type"),
                shiny::selectInput(ns("ladder_type"), NULL,
                  choices = c("PageRuler Plus" = "pageruler",
                              "Precision Plus" = "precision",
                              "Color Prestained" = "prestained",
                              "Broad Range" = "broad",
                              "Custom" = "custom"),
                  selected = "precision")
              ),
              lab_card(
                step_title(5, "Label Settings"),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput(ns("fontsize"), "Font Size",
                    value = 20, min = 10, max = 32, step = 2)),
                  shiny::column(6, shiny::checkboxInput(ns("bold"), "Bold Text", value = FALSE))
                ),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput(ns("ladder_offset"), "Ladder Offset",
                    value = 60, min = 40, max = 120, step = 10)),
                  shiny::column(6, shiny::numericInput(ns("well_offset"), "Well Offset",
                    value = 40, min = 20, max = 80, step = 10))
                ),
                shiny::selectInput(ns("text_angle"), "Well Label Orientation",
                  choices = c("Horizontal" = "0", "Diagonal" = "45", "Vertical" = "90"),
                  selected = "45")
              ),
              lab_card(
                step_title(6, "Export"),
                shiny::checkboxInput(ns("preview"), "Preview Mode (hide markers)", value = FALSE),
                shiny::selectInput(ns("bg_color"), "Export Background",
                  choices = c("Transparent" = "transparent", "White" = "white"),
                  selected = "transparent"),
                shiny::br(),
                shiny::downloadButton(ns("export"),      "\u2193 Download PNG",  class = "btn-download"),
                " ",
                shiny::downloadButton(ns("export_tiff"), "\u2193 Download TIFF", class = "btn-download"),
                shiny::br(), shiny::br(),
                shiny::actionButton(ns("clear_markers"),
                  "\U0001f5d1\ufe0f Clear All Markers", class = "btn-secondary")
              )
            )  # close workflow-col
          ),

          # ----- Centre column: image preview (sticky) ------------------
          shiny::column(7,
            shiny::div(class = "preview-col",
              lab_card(
                shiny::div(class = "lab-card-title", "\U0001f52c  Gel Image"),
                shiny::conditionalPanel(
                  condition = sprintf("output['%s']", ns("image_loaded")),
                  shiny::uiOutput(ns("preview_notice")),
                  shiny::div(style = "text-align: center;",
                    shiny::plotOutput(ns("plot"), height = "600px",
                      click = ns("click"),
                      brush = shiny::brushOpts(id = ns("crop_brush"),
                        fill = "#00C2FF", stroke = "#00C2FF",
                        opacity = 0.2, resetOnNew = TRUE)))
                ),
                shiny::conditionalPanel(
                  condition = sprintf("!output['%s']", ns("image_loaded")),
                  shiny::div(style = "text-align:center;padding:100px 20px;color:#7A8FAD;",
                    shiny::icon("image", style = "font-size:64px;margin-bottom:20px;"),
                    shiny::h4("No image loaded", style = "color:#7A8FAD;"),
                    shiny::p("Upload a gel image to begin"))
                )
              )
            )  # close preview-col
          ),

          # ----- Right column: marker panels (scrolls) ------------------
          # Uses workflow-col styling so this column scrolls independently
          # of the sticky preview. Typically the three panels are short
          # enough that no scrollbar shows up - but if the user adds
          # many ladder bands or wells, the column scrolls cleanly.
          shiny::column(2,
            shiny::div(class = "workflow-col",
              lab_card(
                shiny::div(class = "lab-card-title", "\U0001f3af Ladder Bands"),
                shiny::uiOutput(ns("ladder_list"))
              ),
              lab_card(
                shiny::div(class = "lab-card-title", "\U0001f9ea Sample Wells"),
                shiny::uiOutput(ns("wells_list"))
              ),
              lab_card(
                shiny::div(class = "lab-card-title", "\U0001f4ca Statistics"),
                shiny::uiOutput(ns("stats"))
              )
            )  # close workflow-col
          )
        )
      )
    ),

    # =====================================================================
    # WESTERN TAB
    # =====================================================================
    shiny::conditionalPanel(condition = cond_western,
      shiny::div(class = "sticky-tool",
        shiny::fluidRow(
          # ----- Left column: workflow controls (scrolls) ---------------
          shiny::column(3,
            shiny::div(class = "workflow-col",
              lab_card(
                step_title(1, "Upload Western Image"),
                info_box("Upload your western blot image. Use the preview panel to crop and label it."),
                shiny::fileInput(ns("western"), NULL,
                                 accept = c(".tif",".tiff",".png",".jpg",".jpeg"),
                                 buttonLabel = "Browse\u2026",
                                 placeholder = "No file selected"),
                shiny::uiOutput(ns("western_status"))
              ),
              lab_card(
                step_title(2, "Crop (Optional)"),
                info_box("Switch to Crop mode, then drag on the preview. Release, then click Apply."),
                shiny::fluidRow(
                  shiny::column(6, shiny::actionButton(ns("western_crop_apply"),
                    "\u2713 Apply Crop", class = "btn-run", style = "width:100%;")),
                  shiny::column(6, shiny::actionButton(ns("western_revert_crop"),
                    "\u21a9 Revert Original", class = "btn-secondary", style = "width:100%;"))
                )
              ),
              lab_card(
                step_title(3, "Mark Ladder Bands"),
                info_box("Click a band on the preview, enter its MW, then click Add."),
                shiny::selectInput(ns("western_mode"), "Preview Click Mode",
                  choices = c("Crop" = "crop", "Mark Ladder Bands" = "ladder"),
                  selected = "crop", width = "100%"),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'ladder'", ns("western_mode")),
                  shiny::br(),
                  shiny::actionButton(ns("western_clear_bands"),
                    "\U0001f5d1  Clear All Bands",
                    class = "btn-secondary",
                    style = "font-size:0.78rem;padding:0.35rem 0.7rem;width:100%;"))
              ),
              lab_card(
                step_title(4, "Antibody Label"),
                info_box("Text shown to the right of the western in the stitched export."),
                shiny::textInput(ns("western_antibody"), NULL,
                                 placeholder = "e.g. anti-HsUCP1")
              ),
              lab_card(
                step_title(5, "Enhance Contrast"),
                info_box("0 = no adjustment. Higher values darken bands and lighten background."),
                shiny::sliderInput(ns("western_contrast"), NULL,
                  min = 0, max = 15, value = 0, step = 1, width = "100%")
              ),
              lab_card(
                step_title(6, "Stitch & Export"),
                info_box("Combines the annotated gel (from the Gel tab) with the western below it."),
                shiny::numericInput(ns("gap_px"), "Gap (px)",
                  value = 10, min = 0, max = 200, step = 5),
                shiny::selectInput(ns("gap_color"), "Gap colour",
                  choices = c("Transparent" = "none", "Black" = "black", "White" = "white"),
                  selected = "none", width = "100%"),
                shiny::br(),
                shiny::downloadButton(ns("stitch_png"),  "\u2193 PNG (Gel + Western)",  class = "btn-download"),
                shiny::br(), shiny::br(),
                shiny::downloadButton(ns("stitch_tiff"), "\u2193 TIFF (Gel + Western)", class = "btn-download"),
                shiny::br(), shiny::br(),
                shiny::actionButton(ns("western_clear"),
                  "\u2715  Remove Western", class = "btn-secondary")
              )
            )  # close workflow-col
          ),

          # ----- Centre column: image preview (sticky) ------------------
          shiny::column(7,
            shiny::div(class = "preview-col",
              lab_card(
                shiny::div(class = "lab-card-title", "\U0001f9ec  Western Blot Preview"),
                shiny::conditionalPanel(
                  condition = sprintf("output['%s']", ns("western_loaded")),
                  info_box("Select 'Mark Ladder Bands' mode in Step 3 to click bands. Select 'Crop' mode to crop."),
                  shiny::div(style = "text-align:center;",
                    shiny::plotOutput(ns("western_preview"), height = "420px",
                      click = ns("western_click"),
                      brush = shiny::brushOpts(id = ns("western_crop_brush"),
                        fill = "#00C2FF", stroke = "#00C2FF",
                        opacity = 0.2, resetOnNew = TRUE)))),
                shiny::conditionalPanel(
                  condition = sprintf("!output['%s']", ns("western_loaded")),
                  shiny::div(style = "text-align:center;padding:80px 20px;color:#7A8FAD;",
                    shiny::icon("image", style = "font-size:64px;margin-bottom:20px;"),
                    shiny::h4("No western image loaded", style = "color:#7A8FAD;"),
                    shiny::p("Upload a western blot image to begin")))
              )
            )  # close preview-col
          ),

          # ----- Right column: marker panel (scrolls) -------------------
          shiny::column(2,
            shiny::div(class = "workflow-col",
              lab_card(
                shiny::div(class = "lab-card-title", "\U0001f3af Ladder Bands"),
                shiny::uiOutput(ns("western_ladder_list"))
              )
            )  # close workflow-col
          )
        )
      )
    )
  )
}


# -- Server -------------------------------------------------------------------
gel_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # All state held in one reactiveValues - many pieces update together
    state <- shiny::reactiveValues(
      image          = NULL, image_width = NULL, image_height = NULL,
      image_original = NULL, cropped     = FALSE,
      ladder_markers = .empty_markers("ladder"),
      well_markers   = .empty_markers("well"),
      crop_corners   = list(), crop_active = FALSE,
      gel_crop_pending = NULL,
      plot_trigger   = 0,

      # Western state
      western_image          = NULL, western_raw = NULL, western_original = NULL,
      western_crop_active    = FALSE, western_crop_corners = list(),
      western_ladder_markers = data.frame(x = numeric(), y = numeric(),
                                          mw = numeric(),
                                          stringsAsFactors = FALSE),
      western_pending_click  = NULL,
      western_plot_trigger   = 0,
      western_crop_pending   = NULL
    )

    # ====================================================================
    # GEL: image load, clear, crop
    # ====================================================================

    # Internal: set the gel image from any source (upload or example).
    # Resets all marker/crop state so loading a new image always gives
    # the user a clean canvas to work on.
    .load_gel_image <- function(img) {
      if (is.null(img)) return(invisible())
      info <- magick::image_info(img)
      state$image_original  <- img
      state$image           <- img
      state$image_width     <- info$width
      state$image_height    <- info$height
      state$cropped         <- FALSE
      state$ladder_markers  <- .empty_markers("ladder")
      state$well_markers    <- .empty_markers("well")
      state$crop_corners    <- list()
      state$crop_active     <- FALSE
      state$gel_crop_pending <- NULL
    }

    shiny::observeEvent(input$image, {
      shiny::req(input$image)
      tryCatch({
        .load_gel_image(magick::image_read(input$image$datapath))
        shiny::showNotification("\u2713 Image loaded successfully",
                                type = "message", duration = 2)
      }, error = function(e)
        shiny::showNotification(paste("Error loading image:", e$message),
                                type = "error", duration = 5))
    }, ignoreInit = TRUE)

    # Navbar Clear button: wipe both gel AND western state, then reload
    # both examples so the previews are never empty. Same "Clear =
    # reset to known-good state" semantics as the other tools.
    shiny::observeEvent(input$clear, {
      state$image          <- NULL
      state$image_width    <- NULL; state$image_height <- NULL
      state$ladder_markers <- .empty_markers("ladder")
      state$well_markers   <- .empty_markers("well")
      state$crop_corners   <- list()
      state$crop_active    <- FALSE; state$cropped <- FALSE
      state$gel_crop_pending <- NULL
      # Western state too
      state$western_image          <- NULL
      state$western_raw            <- NULL
      state$western_original       <- NULL
      state$western_crop_active    <- FALSE
      state$western_crop_corners   <- list()
      state$western_ladder_markers <- data.frame(
        x = numeric(), y = numeric(), mw = numeric(),
        stringsAsFactors = FALSE)
      state$western_pending_click  <- NULL
      state$western_plot_trigger   <- 0
      state$western_crop_pending   <- NULL
      tryCatch(.load_gel_example(),     error = function(e) NULL)
      tryCatch(.load_western_example(), error = function(e) NULL)
      shiny::showNotification("\u2713 All data cleared",
                              type = "message", duration = 2)
    })

    # The conditionalPanel uses this to decide whether to show the plot
    # or the "no image" placeholder.
    output$image_loaded <- shiny::reactive({ !is.null(state$image) })
    shiny::outputOptions(output, "image_loaded", suspendWhenHidden = FALSE)

    # ---- Crop ----------------------------------------------------------
    output$crop_status <- shiny::renderUI({
      if (!is.null(state$gel_crop_pending)) {
        b <- state$gel_crop_pending
        shiny::div(style = "color:#00C2FF;font-size:0.8rem;margin-top:0.5rem;",
          sprintf("~%d x %d px selected -- click Apply to confirm",
                  round(abs(b$xmax - b$xmin)), round(abs(b$ymax - b$ymin))))
      } else if (isTRUE(state$cropped)) {
        shiny::div(style = "color:#7A8FAD;font-size:0.8rem;margin-top:0.5rem;",
          "Image is cropped.")
      }
    })

    shiny::observeEvent(input$crop_brush, {
      shiny::req(state$image)
      state$gel_crop_pending <- input$crop_brush
    })

    shiny::observeEvent(input$apply_crop, {
      shiny::req(state$image, state$gel_crop_pending)
      b  <- state$gel_crop_pending
      iw <- state$image_width; ih <- state$image_height

      # The editing-mode plot draws the image at xlim=(0,iw), ylim=(0,ih),
      # asp=1, with no padding or scaling. Brush coordinates are therefore
      # in image-pixel units already. The only transform needed is the
      # y-axis flip: in base R plotting, y increases UPWARDS from the
      # origin at bottom-left; in magick, y increases DOWNWARDS from the
      # origin at top-left. So a brush selection at plot-y=[ymin,ymax]
      # selects magick-y=[ih-ymax, ih-ymin] in the source image.
      x_min <- max(0L, round(min(b$xmin, b$xmax)))
      x_max <- min(iw, round(max(b$xmin, b$xmax)))
      y_top    <- max(0L, round(ih - max(b$ymin, b$ymax)))
      y_bottom <- min(ih, round(ih - min(b$ymin, b$ymax)))
      cw <- max(0L, x_max - x_min); ch <- max(0L, y_bottom - y_top)
      if (cw < 10 || ch < 10) {
        shiny::showNotification("Selection too small.", type = "warning")
        state$gel_crop_pending <- NULL
        return()
      }
      tryCatch({
        state$image <- magick::image_crop(
          state$image, magick::geometry_area(cw, ch, x_min, y_top))
        state$image_width  <- cw
        state$image_height <- ch
        state$cropped      <- TRUE
        state$gel_crop_pending <- NULL
        state$ladder_markers <- .empty_markers("ladder")
        state$well_markers   <- .empty_markers("well")
        shiny::showNotification(sprintf("Cropped to %d x %d px", cw, ch),
                                type = "message", duration = 2)
      }, error = function(e)
        shiny::showNotification(paste("Crop error:", e$message), type = "error"))
    })

    shiny::observeEvent(input$revert_crop, {
      shiny::req(state$image_original)
      info <- magick::image_info(state$image_original)
      state$image        <- state$image_original
      state$image_width  <- info$width
      state$image_height <- info$height
      state$cropped      <- FALSE
      state$gel_crop_pending <- NULL
      state$ladder_markers <- .empty_markers("ladder")
      state$well_markers   <- .empty_markers("well")
      shiny::showNotification("Reverted to original image",
                              type = "message", duration = 2)
    })

    # ---- Click placement (ladder / wells) -------------------------------
    shiny::observeEvent(input$click, {
      shiny::req(state$image)
      mode <- input$mode %||% "crop"
      if (mode == "crop") return()   # brush handles crop
      cl <- input$click

      if (mode == "ladder") {
        preset_mws    <- .GEL_LADDER_PRESETS[[input$ladder_type %||% "precision"]]
        current_count <- nrow(shiny::isolate(state$ladder_markers))
        next_mw <- if (length(preset_mws) > current_count)
                     preset_mws[current_count + 1] else 0

        new_marker <- data.frame(
          x  = round(cl$x, 1), y = round(cl$y, 1),
          mw = next_mw,
          id = .make_marker_id("m"),
          stringsAsFactors = FALSE)
        state$ladder_markers <- rbind(shiny::isolate(state$ladder_markers),
                                      new_marker)
        state$plot_trigger   <- shiny::isolate(state$plot_trigger) + 1
      } else if (mode == "wells") {
        current_count <- nrow(shiny::isolate(state$well_markers))
        new_marker <- data.frame(
          x     = round(cl$x, 1), y = round(cl$y, 1),
          label = paste0("Sample ", current_count + 1),
          id    = .make_marker_id("w"),
          stringsAsFactors = FALSE)
        state$well_markers <- rbind(shiny::isolate(state$well_markers),
                                    new_marker)
        state$plot_trigger <- shiny::isolate(state$plot_trigger) + 1
      }
    })

    shiny::observeEvent(input$clear_markers, {
      state$ladder_markers <- .empty_markers("ladder")
      state$well_markers   <- .empty_markers("well")
      state$plot_trigger   <- shiny::isolate(state$plot_trigger) + 1
      shiny::showNotification("\u2713 All markers cleared",
                              type = "message", duration = 2)
    })

    # ====================================================================
    # GEL: main plot - editing OR preview mode
    # ====================================================================
    output$plot <- shiny::renderPlot({
      shiny::req(state$image)
      state$plot_trigger   # take dependency so re-renders fire

      ladder_markers <- shiny::isolate(state$ladder_markers)
      well_markers   <- shiny::isolate(state$well_markers)

      if (isTRUE(input$preview) &&
          (nrow(ladder_markers) > 0 || nrow(well_markers) > 0)) {
        # Annotated preview ------------------------------------------
        .gel_render_annotated(
          image          = state$image,
          image_width    = state$image_width,
          image_height   = state$image_height,
          ladder_markers = ladder_markers,
          well_markers   = well_markers,
          fontsize       = input$fontsize,
          bold           = isTRUE(input$bold),
          ladder_offset  = input$ladder_offset,
          well_offset    = input$well_offset,
          text_angle     = as.numeric(input$text_angle %||% "45"),
          bg_color       = if (isTRUE(input$preview)) "white"
                           else (input$bg_color %||% "transparent"),
          left_pad_use_fontsize_floor = TRUE,
          preview_fit    = TRUE
        )
      } else {
        # Editing mode - show clickable image + markers in place ------
        graphics::par(mar = c(0, 0, 0, 0))
        plot(1, type = "n",
             xlim = c(0, state$image_width),
             ylim = c(0, state$image_height),
             xlab = "", ylab = "", axes = FALSE, asp = 1)
        graphics::rasterImage(grDevices::as.raster(state$image),
          0, 0, state$image_width, state$image_height)

        # Click-corner crop fallback (legacy, kept for safety)
        if (isTRUE(state$crop_active) && length(state$crop_corners) > 0) {
          for (corner in state$crop_corners)
            graphics::points(corner$x, corner$y, pch = 19,
                             col = "#10b981", cex = 2)
          if (length(state$crop_corners) == 2) {
            x_vals <- c(state$crop_corners[[1]]$x, state$crop_corners[[2]]$x)
            y_vals <- c(state$crop_corners[[1]]$y, state$crop_corners[[2]]$y)
            graphics::rect(min(x_vals), min(y_vals),
                           max(x_vals), max(y_vals),
                           border = "#10b981", lwd = 3, lty = 2)
          }
        }

        if (nrow(ladder_markers) > 0) {
          graphics::points(ladder_markers$x, ladder_markers$y,
                           pch = 19, col = "#dc2626", cex = 2)
          graphics::text(ladder_markers$x, ladder_markers$y,
                         paste0(ladder_markers$mw, " kDa"),
                         pos = 4, col = "white", font = 2, cex = 0.9)
        }
        if (nrow(well_markers) > 0) {
          graphics::points(well_markers$x, well_markers$y,
                           pch = 19, col = "#2563eb", cex = 2)
          graphics::text(well_markers$x, well_markers$y,
                         well_markers$label,
                         pos = 3, col = "white", font = 2, cex = 0.9)
        }
      }
    }, bg = "white")

    # ====================================================================
    # GEL: marker list panels (with per-row delete + edit)
    # ====================================================================
    output$ladder_list <- shiny::renderUI({
      state$plot_trigger
      m <- shiny::isolate(state$ladder_markers)
      if (nrow(m) == 0)
        return(shiny::p("No ladder bands marked",
          style = "color:#7A8FAD;font-size:0.9rem;padding:0.5rem;"))

      rows <- lapply(seq_len(nrow(m)), function(i) {
        mid <- m$id[i]
        shiny::div(style = "background:#1E2D45;padding:8px;border-radius:4px;margin-bottom:8px;",
          shiny::div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
            shiny::span(paste("Band", i),
                        style = "color:#7A8FAD;font-size:0.85rem;"),
            shiny::actionButton(ns(paste0("del_ladder_", mid)), "\u2715",
              style = "background:#FF5C5C;color:white;border:none;padding:2px 8px;font-size:0.8rem;border-radius:3px;cursor:pointer;")),
          shiny::numericInput(ns(paste0("mw_", mid)), NULL,
            value = m$mw[i], min = 0, step = 1, width = "100%"))
      })
      shiny::div(style = "max-height:300px;overflow-y:auto;",
                 shiny::tagList(rows))
    })

    output$wells_list <- shiny::renderUI({
      state$plot_trigger
      m <- shiny::isolate(state$well_markers)
      if (nrow(m) == 0)
        return(shiny::p("No wells marked",
          style = "color:#7A8FAD;font-size:0.9rem;padding:0.5rem;"))

      rows <- lapply(seq_len(nrow(m)), function(i) {
        mid <- m$id[i]
        shiny::div(style = "background:#1E2D45;padding:8px;border-radius:4px;margin-bottom:8px;",
          shiny::div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
            shiny::span(paste("Well", i),
                        style = "color:#7A8FAD;font-size:0.85rem;"),
            shiny::actionButton(ns(paste0("del_well_", mid)), "\u2715",
              style = "background:#FF5C5C;color:white;border:none;padding:2px 8px;font-size:0.8rem;border-radius:3px;cursor:pointer;")),
          shiny::textInput(ns(paste0("label_", mid)), NULL,
            value = m$label[i], width = "100%",
            placeholder = "Sample name"))
      })
      shiny::div(style = "max-height:300px;overflow-y:auto;",
                 shiny::tagList(rows))
    })

    # Preview-mode notice: warn the user when the canvas padding required
    # for their current labels is large enough that the preview device
    # will visibly squeeze the text. The export, which uses an exactly
    # sized PNG/TIFF device, doesn't have this problem - so the notice
    # specifically tells the user the export will be fine even though the
    # preview looks cramped or shrunken.
    #
    # We don't know the preview device's exact pixel size, so we use a
    # heuristic on canvas aspect ratio: if the canvas (image + padding)
    # is wider than ~1.4x its height, the 600-px-tall Shiny container
    # will probably squeeze it. This catches both the long-well-label
    # case (right_pad bulks up canvas width) and very large fontsize
    # cases (left_pad bulks up canvas width).
    output$preview_notice <- shiny::renderUI({
      if (!isTRUE(input$preview)) return(NULL)
      lm <- state$ladder_markers
      wm <- state$well_markers
      if (nrow(lm) == 0 && nrow(wm) == 0) return(NULL)
      if (is.null(state$image_width) || is.null(state$image_height))
        return(NULL)

      dims <- .gel_compute_canvas(
        image_width    = state$image_width,
        image_height   = state$image_height,
        ladder_markers = lm,
        well_markers   = wm,
        ladder_offset  = input$ladder_offset %||% 60,
        well_offset    = input$well_offset   %||% 40,
        text_angle     = as.numeric(input$text_angle %||% "45"),
        fontsize       = input$fontsize %||% 20)

      canvas_aspect <- dims$total_width / max(dims$total_height, 1)
      img_aspect    <- (dims$img_width) / max(dims$img_height, 1)
      # Show notice when canvas aspect is meaningfully wider than image's
      # natural aspect (i.e. padding is significant), AND wider than the
      # preview can comfortably show without shrinking text.
      show <- canvas_aspect > 1.4 && canvas_aspect > img_aspect * 1.2
      if (!show) return(NULL)

      shiny::div(class = "info-box",
        style = "margin: 0 0 0.6rem 0; background: #1E2D45;
                 border-left: 3px solid #FFD93D;",
        shiny::tags$span(style = "color: #FFD93D; font-weight: 600;",
                         "\u2139 Preview note: "),
        "labels may appear clipped or smaller here because the preview ",
        "window has fixed dimensions. ",
        shiny::tags$b("The exported PNG/TIFF will render at full fontsize "),
        "with all labels intact \u2014 the canvas is sized to fit them.")
    })

    # Dynamic watchers: marker edit + delete. Run per row inside a single
    # observer that re-creates dependencies as the marker list changes.
    shiny::observe({
      if (nrow(state$ladder_markers) > 0) {
        for (i in seq_len(nrow(state$ladder_markers))) {
          mid    <- state$ladder_markers$id[i]
          new_val <- input[[paste0("mw_", mid)]]
          if (!is.null(new_val) && !is.na(new_val)) {
            cur <- state$ladder_markers$mw[state$ladder_markers$id == mid]
            if (length(cur) > 0 && new_val != cur)
              state$ladder_markers$mw[state$ladder_markers$id == mid] <- new_val
          }
        }
      }
      if (nrow(state$well_markers) > 0) {
        for (i in seq_len(nrow(state$well_markers))) {
          mid    <- state$well_markers$id[i]
          new_val <- input[[paste0("label_", mid)]]
          if (!is.null(new_val)) {
            cur <- state$well_markers$label[state$well_markers$id == mid]
            if (length(cur) > 0 && new_val != cur)
              state$well_markers$label[state$well_markers$id == mid] <- new_val
          }
        }
      }
    })

    shiny::observe({
      if (nrow(state$ladder_markers) > 0) {
        for (i in seq_len(nrow(state$ladder_markers))) {
          mid    <- state$ladder_markers$id[i]
          btn_id <- paste0("del_ladder_", mid)
          if (!is.null(input[[btn_id]]) && input[[btn_id]] > 0) {
            shiny::isolate({
              state$ladder_markers <-
                state$ladder_markers[state$ladder_markers$id != mid, ]
              state$plot_trigger <- state$plot_trigger + 1
            })
          }
        }
      }
      if (nrow(state$well_markers) > 0) {
        for (i in seq_len(nrow(state$well_markers))) {
          mid    <- state$well_markers$id[i]
          btn_id <- paste0("del_well_", mid)
          if (!is.null(input[[btn_id]]) && input[[btn_id]] > 0) {
            shiny::isolate({
              state$well_markers <-
                state$well_markers[state$well_markers$id != mid, ]
              state$plot_trigger <- state$plot_trigger + 1
            })
          }
        }
      }
    })

    output$stats <- shiny::renderUI({
      state$plot_trigger
      lm <- shiny::isolate(state$ladder_markers)
      wm <- shiny::isolate(state$well_markers)
      n_l <- nrow(lm); n_w <- nrow(wm)
      mw_range <- if (n_l >= 2)
                    paste0(min(lm$mw), "-", max(lm$mw), " kDa")
                  else "N/A"
      shiny::div(style = "padding:0.5rem;",
        shiny::div(style = "display:flex;justify-content:space-between;margin-bottom:8px;",
          shiny::span("Ladder Bands:", style = "color:#7A8FAD;"),
          shiny::span(n_l, style = "color:#00C2FF;font-weight:bold;")),
        shiny::div(style = "display:flex;justify-content:space-between;margin-bottom:8px;",
          shiny::span("Sample Wells:", style = "color:#7A8FAD;"),
          shiny::span(n_w, style = "color:#00C2FF;font-weight:bold;")),
        shiny::div(style = "display:flex;justify-content:space-between;",
          shiny::span("MW Range:", style = "color:#7A8FAD;"),
          shiny::span(mw_range, style = "color:#00C2FF;font-weight:bold;")))
    })

    # ====================================================================
    # GEL: PNG / TIFF exports
    # ====================================================================
    .render_to_device <- function(file, device = c("png","tiff")) {
      device <- match.arg(device)
      lm <- shiny::isolate(state$ladder_markers)
      wm <- shiny::isolate(state$well_markers)
      dims <- .gel_compute_canvas(
        image_width    = state$image_width,
        image_height   = state$image_height,
        ladder_markers = lm, well_markers = wm,
        ladder_offset  = input$ladder_offset,
        well_offset    = input$well_offset,
        text_angle     = as.numeric(input$text_angle %||% "45"),
        fontsize       = input$fontsize,
        left_pad_use_fontsize_floor = (device == "png")
      )
      bg_color <- if (input$bg_color == "white") "white"
                  else if (device == "tiff") "transparent" else NA

      if (device == "png") {
        grDevices::png(file,
          width = dims$total_width, height = dims$total_height,
          units = "px", bg = bg_color, res = 96)
      } else {
        grDevices::tiff(file,
          width = dims$total_width, height = dims$total_height,
          units = "px", bg = bg_color, res = 300,
          compression = "lzw")
      }
      .gel_render_annotated(
        image          = state$image,
        image_width    = state$image_width,
        image_height   = state$image_height,
        ladder_markers = lm, well_markers = wm,
        fontsize       = input$fontsize,
        bold           = isTRUE(input$bold),
        ladder_offset  = input$ladder_offset,
        well_offset    = input$well_offset,
        text_angle     = as.numeric(input$text_angle %||% "45"),
        bg_color       = input$bg_color,
        left_pad_use_fontsize_floor = (device == "png")
      )
      grDevices::dev.off()
    }

    output$export <- shiny::downloadHandler(
      filename = function() ts_filename("gel_labeled", "png"),
      content  = function(file) {
        shiny::req(state$image)
        .render_to_device(file, "png")
        shiny::showNotification("\u2713 Image exported successfully!",
                                type = "message", duration = 3)
      })

    output$export_tiff <- shiny::downloadHandler(
      filename = function() ts_filename("gel_labeled", "tiff"),
      content  = function(file) {
        shiny::req(state$image)
        .render_to_device(file, "tiff")
        shiny::showNotification("\u2713 TIFF exported successfully!",
                                type = "message", duration = 3)
      })

    # ====================================================================
    # WESTERN BLOT
    # ====================================================================

    output$western_loaded <- shiny::reactive({ !is.null(state$western_image) })
    shiny::outputOptions(output, "western_loaded", suspendWhenHidden = FALSE)

    # Internal: set the western image from any source (upload or example).
    # Mirrors .load_gel_image() but for the western state bucket.
    .load_western_image <- function(img) {
      if (is.null(img)) return(invisible())
      state$western_raw            <- img
      state$western_image          <- img
      state$western_original       <- img
      state$western_crop_active    <- FALSE
      state$western_crop_corners   <- list()
      state$western_ladder_markers <- data.frame(
        x = numeric(), y = numeric(), mw = numeric(),
        stringsAsFactors = FALSE)
      state$western_pending_click  <- NULL
      state$western_plot_trigger   <- 0
      state$western_crop_pending   <- NULL
    }

    shiny::observeEvent(input$western, {
      shiny::req(input$western)
      tryCatch({
        .load_western_image(magick::image_read(input$western$datapath))
        shiny::showNotification("\u2713 Western blot loaded",
                                type = "message", duration = 2)
      }, error = function(e)
        shiny::showNotification(paste("Error:", e$message), type = "error"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$western_clear, {
      state$western_image          <- NULL
      state$western_raw            <- NULL
      state$western_crop_active    <- FALSE
      state$western_crop_corners   <- list()
      state$western_ladder_markers <- data.frame(
        x = numeric(), y = numeric(), mw = numeric(),
        stringsAsFactors = FALSE)
      state$western_pending_click  <- NULL
      state$western_plot_trigger   <- 0
      shinyjs::reset("western")
      shiny::showNotification("Western removed",
                              type = "message", duration = 2)
    })

    output$western_status <- shiny::renderUI({
      shiny::req(state$western_image)
      info <- magick::image_info(state$western_image)
      status_pill("ready",
        sprintf("\u2713 %d \u00d7 %d px", info$width, info$height))
    })

    # ---- Western crop --------------------------------------------------
    shiny::observeEvent(input$western_crop_brush, {
      shiny::req(state$western_image)
      if (isTRUE(input$western_mode == "crop"))
        state$western_crop_pending <- input$western_crop_brush
    })

    shiny::observeEvent(input$western_crop_apply, {
      shiny::req(state$western_image)
      b <- state$western_crop_pending
      if (is.null(b)) {
        shiny::showNotification(
          "Switch to Crop mode and drag a selection first.",
          type = "warning"); return()
      }
      tryCatch({
        info <- magick::image_info(state$western_image)
        iw <- info$width; ih <- info$height
        x_min     <- max(0L, round(min(b$xmin, b$xmax)))
        x_max     <- min(iw, round(max(b$xmin, b$xmax)))
        y_min_img <- max(0L, round(ih - max(b$ymin, b$ymax)))
        y_max_img <- min(ih, round(ih - min(b$ymin, b$ymax)))
        cw <- x_max - x_min; ch <- y_max_img - y_min_img
        if (cw < 10 || ch < 10) {
          shiny::showNotification("Selection too small.", type = "warning")
          return()
        }
        state$western_image <- magick::image_crop(
          state$western_image, magick::geometry_area(cw, ch, x_min, y_min_img))
        state$western_crop_pending <- NULL
        state$western_ladder_markers <- data.frame(
          x = numeric(), y = numeric(), mw = numeric(),
          stringsAsFactors = FALSE)
        state$western_plot_trigger <- state$western_plot_trigger + 1
        shiny::showNotification(sprintf("Cropped to %d x %d px", cw, ch),
                                type = "message", duration = 2)
      }, error = function(e)
        shiny::showNotification(paste("Crop error:", e$message),
                                type = "error"))
    })

    shiny::observeEvent(input$western_revert_crop, {
      shiny::req(state$western_original)
      state$western_image          <- state$western_original
      state$western_crop_pending   <- NULL
      state$western_ladder_markers <- data.frame(
        x = numeric(), y = numeric(), mw = numeric(),
        stringsAsFactors = FALSE)
      state$western_plot_trigger <- state$western_plot_trigger + 1
      shiny::showNotification("Reverted to original western",
                              type = "message", duration = 2)
    })

    # ---- Western click placement (ladder mode) -------------------------
    shiny::observeEvent(input$western_click, {
      shiny::req(state$western_image)
      cl   <- input$western_click
      mode <- input$western_mode %||% "crop"
      if (mode == "crop" && isTRUE(state$western_crop_active)) {
        corners <- state$western_crop_corners
        corners[[length(corners) + 1]] <- list(x = cl$x, y = cl$y)
        state$western_crop_corners <- corners
        if (length(corners) >= 2) state$western_crop_active <- FALSE
      } else if (mode == "ladder") {
        state$western_ladder_markers <- rbind(state$western_ladder_markers,
          data.frame(x = cl$x, y = cl$y, mw = 0,
                     stringsAsFactors = FALSE))
        state$western_plot_trigger <- state$western_plot_trigger + 1
      }
    })

    shiny::observeEvent(input$western_clear_bands, {
      state$western_ladder_markers <- data.frame(
        x = numeric(), y = numeric(), mw = numeric(),
        stringsAsFactors = FALSE)
      state$western_plot_trigger <- state$western_plot_trigger + 1
      shiny::showNotification("Bands cleared",
                              type = "message", duration = 2)
    })

    # Per-row delete in the western ladder list. Uses a non-namespaced
    # external input set by inline JS - we route it through a namespaced
    # ID for safety.
    output$western_ladder_list <- shiny::renderUI({
      state$western_plot_trigger
      m <- shiny::isolate(state$western_ladder_markers)
      if (nrow(m) == 0)
        return(shiny::p("No bands marked yet.",
                        style = "color:#7A8FAD;font-size:0.9rem;"))
      rows <- lapply(seq_len(nrow(m)), function(i) {
        shiny::div(style = "background:#1E2D45;padding:8px;border-radius:4px;margin-bottom:6px;",
          shiny::div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;",
            shiny::span(paste("Band", i),
                        style = "color:#7A8FAD;font-size:0.85rem;"),
            shiny::tags$button("\u00d7",
              style = "background:#FF5C5C;color:white;border:none;padding:2px 8px;border-radius:3px;cursor:pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('%s',%d,{priority:'event'})",
                ns("western_rm"), i))),
          shiny::numericInput(ns(paste0("wmw_", i)), NULL,
            value = if (m$mw[i] == 0) NA else m$mw[i],
            min = 0, step = 1, width = "100%"))
      })
      shiny::div(style = "max-height:300px;overflow-y:auto;",
                 shiny::tagList(rows))
    })

    shiny::observeEvent(input$western_rm, {
      idx <- input$western_rm
      m   <- state$western_ladder_markers
      if (idx >= 1 && idx <= nrow(m)) {
        state$western_ladder_markers <- m[-idx, , drop = FALSE]
        state$western_plot_trigger   <- state$western_plot_trigger + 1
      }
    })

    # ---- Western preview render ---------------------------------------
    output$western_preview <- shiny::renderPlot({
      shiny::req(state$western_image)
      state$western_plot_trigger
      img  <- .western_processed(state$western_image, input$western_contrast)
      rast <- grDevices::as.raster(img)
      info <- magick::image_info(img)
      graphics::par(mar = c(0, 0, 0, 0), bg = "black")
      plot(NA, xlim = c(0, info$width), ylim = c(0, info$height),
           asp = 1, xlab = "", ylab = "", axes = FALSE)
      graphics::rasterImage(rast, 0, 0, info$width, info$height)
      for (cr in state$western_crop_corners)
        graphics::points(cr$x, cr$y, pch = 3, col = "#00C2FF", cex = 2.5, lwd = 2)
      m <- shiny::isolate(state$western_ladder_markers)
      if (nrow(m) > 0) for (i in seq_len(nrow(m))) {
        graphics::points(m$x[i], m$y[i], pch = 16, col = "#FF7B47", cex = 1.2)
        if (!is.na(m$mw[i]) && m$mw[i] > 0)
          graphics::text(m$x[i], m$y[i], paste0(m$mw[i], " kDa"),
                         pos = 3, col = "#FF7B47", cex = 0.75, font = 2)
      }
      if (!is.null(state$western_pending_click))
        graphics::points(state$western_pending_click$x,
                         state$western_pending_click$y,
                         pch = 16, col = "#00C2FF", cex = 2)
    }, bg = "black")

    # ====================================================================
    # GEL + WESTERN STITCH EXPORT
    # ====================================================================
    output$stitch_png <- shiny::downloadHandler(
      filename = function() ts_filename("gel_western", "png"),
      content  = function(file) {
        shiny::req(state$image)
        # First write the labelled gel as the base PNG
        .render_to_device(file, "png")
        # Then stack the western underneath
        tryCatch(
          .gel_stack_western(state = state, input = input,
                             gel_png_path = file, format = "png"),
          error = function(e)
            shiny::showNotification(paste("Export error:", e$message),
                                    type = "error", duration = 5))
        shiny::showNotification("\u2713 Gel+Western exported!",
                                type = "message", duration = 3)
      })

    output$stitch_tiff <- shiny::downloadHandler(
      filename = function() ts_filename("gel_western", "tiff"),
      content  = function(file) {
        shiny::req(state$image)
        .render_to_device(file, "tiff")
        tryCatch({
          wt <- .western_processed(shiny::isolate(state$western_image),
                                   shiny::isolate(input$western_contrast))
          if (!is.null(wt)) {
            gi  <- magick::image_read(file)
            gw  <- as.integer(magick::image_info(gi)$width)
            sw  <- as.integer(magick::image_info(wt)$width)
            sh  <- as.integer(magick::image_info(wt)$height)
            nh  <- max(1L, round(sh * gw / sw))
            wt  <- magick::image_resize(wt, sprintf("%dx%d!", gw, nh))
            gc2 <- shiny::isolate(input$gap_color)
            bg2 <- if (!is.null(gc2) && gc2 != "none") gc2 else "none"
            gp  <- magick::image_blank(gw,
              max(1L, as.integer(shiny::isolate(input$gap_px))), color = bg2)
            magick::image_write(magick::image_append(c(gi, gp, wt), stack = TRUE),
              path = file, format = "tiff",
              options = c("compression=lzw"))
          }
        }, error = function(e)
          shiny::showNotification(paste("TIFF stitch error:", e$message),
                                  type = "error", duration = 5))
        shiny::showNotification("\u2713 TIFF exported successfully!",
                                type = "message", duration = 3)
      })

    # ====================================================================
    # Session-start example loaders
    # ====================================================================
    # Two one-shot observers, one per mode. Same self-destruct pattern
    # used in AKTA/BCA/CPM Peak/CPM QC. Each loader pulls a small
    # bundled image from inst/examples/, decodes it via magick, and
    # routes it through the same .load_*_image() helper the upload
    # observers use - so the example and a user upload populate state
    # in identical ways.
    .load_gel_example <- function() {
      img <- .gel_example_image()
      if (!is.null(img)) .load_gel_image(img)
    }
    .load_western_example <- function() {
      img <- .western_example_image()
      if (!is.null(img)) .load_western_image(img)
    }
    .gel_example_obs <- shiny::observe({
      .gel_example_obs$destroy()
      tryCatch(.load_gel_example(), error = function(e)
        message("[Gel] gel example load failed: ", conditionMessage(e)))
    })
    .western_example_obs <- shiny::observe({
      .western_example_obs$destroy()
      tryCatch(.load_western_example(), error = function(e)
        message("[Gel] western example load failed: ", conditionMessage(e)))
    })

    # ---- Public reactive ------------------------------------------------
    shiny::reactive({
      if (is.null(state$image)) return(NULL)
      list(image          = state$image,
           ladder_markers = state$ladder_markers,
           well_markers   = state$well_markers,
           western_image  = state$western_image)
    })
  })
}


# -- Internal helpers ---------------------------------------------------------

# Empty marker data frame with the right columns
.empty_markers <- function(kind = c("ladder", "well")) {
  kind <- match.arg(kind)
  if (kind == "ladder")
    data.frame(x = numeric(), y = numeric(), mw = numeric(),
               id = character(), stringsAsFactors = FALSE)
  else
    data.frame(x = numeric(), y = numeric(), label = character(),
               id = character(), stringsAsFactors = FALSE)
}

# Unique marker id based on timestamp. Microsecond suffix is enough as long
# as users don't click 10 times in 1 microsecond (they don't).
.make_marker_id <- function(prefix) {
  paste0(prefix, gsub("[^0-9]", "",
                      format(Sys.time(), "%Y%m%d%H%M%OS6")))
}

# Apply western contrast pre-processing - 0 = no change; higher values
# repeatedly apply image_contrast() after a normalise.
.western_processed <- function(img, contrast_val) {
  if (is.null(img)) return(NULL)
  cv <- contrast_val
  if (!is.null(cv) && !is.na(cv) && cv > 0) {
    img <- magick::image_normalize(img)
    for (k in seq_len(min(as.integer(cv), 15)))
      img <- magick::image_contrast(img, sharpen = FALSE)
  }
  img
}

# Calculate canvas dimensions for the annotated gel.
#
# The padding required for ladder labels (left) and well labels (top + right)
# can't be estimated from character counts - 1.7x fontsize / 0.6 fontsize per
# character is an underestimate in practice, especially for "00 kDa" and
# similar 7-char strings with rounded letterforms. So we actually measure
# the rendered width of every label by opening a temporary off-screen
# device and calling strwidth() with the same cex that will be used to
# draw the text. This is the only reliable way - strwidth() needs an
# active device because text metrics depend on the device's DPI.

# Measure rendered widths of `labels` at the given cex, in display pixels
# (assuming 96 dpi output, which is what plotOutput and the export PNG
# device both use). Returns numeric() for empty input. The off-screen
# PDF device is what strwidth() needs to compute font metrics; the device
# is closed and the temp file removed before returning.
.gel_measure_widths_px <- function(labels, cex) {
  if (length(labels) == 0) return(numeric(0))
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp, width = 7, height = 7)
  graphics::par(mar = c(0, 0, 0, 0))
  plot.new()
  widths_in <- vapply(labels, function(s)
    graphics::strwidth(s, units = "inches", cex = cex),
    numeric(1))
  grDevices::dev.off()
  unlink(tmp)
  widths_in * 96
}

.gel_compute_canvas <- function(image_width, image_height,
                                ladder_markers, well_markers,
                                ladder_offset, well_offset,
                                text_angle, fontsize,
                                left_pad_use_fontsize_floor = TRUE) {
  scale_factor <- 0.75
  img_w <- image_width  * scale_factor
  img_h <- image_height * scale_factor

  cex_val <- fontsize / 12

  # ---- Left pad: max measured ladder label width + breathing room ------
  left_pad <- 0
  if (nrow(ladder_markers) > 0) {
    labels   <- paste0(ladder_markers$mw, " kDa")
    max_w_px <- max(.gel_measure_widths_px(labels, cex_val), 0)
    # Real width + a small margin + the user-controlled ladder offset.
    # The +12 is the gap between text and the tick segment.
    measured <- max_w_px + ladder_offset + 12
    if (isTRUE(left_pad_use_fontsize_floor))
      left_pad <- max(measured, ladder_offset + 20)
    else
      left_pad <- measured
  }

  # ---- Top pad + right pad: well labels (rotated) ----------------------
  top_pad   <- 0
  right_pad <- 0
  if (nrow(well_markers) > 0) {
    max_w_px <- max(.gel_measure_widths_px(well_markers$label, cex_val), 0)
    if (text_angle == 0) {
      # Horizontal: only need height clearance plus a tiny right margin
      # in case the rightmost label is wider than the gel image.
      top_pad   <- well_offset + 20 + cex_val * 14   # rough line height
      # If a horizontal label is centred on the rightmost well marker
      # (pos = 3, default centring), half its width extends past the
      # marker. We don't know exactly where the rightmost marker is in
      # canvas-x without applying the scale_factor, but using image_width
      # as a worst case is fine.
      right_pad <- max(0, ceiling(max_w_px / 2 - 4))
    } else if (text_angle == 90) {
      # Vertical: label rises upward from the marker.
      top_pad   <- well_offset + 20 + max_w_px
      right_pad <- ceiling(cex_val * 14 / 2)
    } else {
      # Diagonal (45): label rises up-and-right by sqrt(2)/2 of its
      # length on each axis.
      diag_proj <- max_w_px * sin(pi / 4)
      top_pad   <- well_offset + 20 + diag_proj
      # The rightmost well label is anchored at the rightmost marker's
      # x position; the text rotates up-and-right, so the rightmost edge
      # of the text extends `diag_proj` past the anchor. The marker is
      # at or before the image edge, so we need at least `diag_proj` of
      # right padding (plus a small buffer for the kerning of the last
      # letter).
      right_pad <- ceiling(diag_proj + 8)
    }
  }

  list(scale_factor = scale_factor,
       img_width    = img_w,    img_height   = img_h,
       left_pad     = left_pad, top_pad      = top_pad,
       right_pad    = right_pad,
       total_width  = img_w + left_pad + right_pad,
       total_height = img_h + top_pad)
}

# Render the annotated gel via base graphics into the currently open device
# (or the screen plot device, for preview). All four call sites share this.
#
# The `preview_fit` flag triggers a measure-and-shrink pass for the preview
# render path: with asp=1 plus a fixed-pixel Shiny device, the actual
# rendered text size in user units depends on how the canvas got squeezed
# to fit. We measure widths in user units AFTER plot() opens (so asp has
# applied) and shrink the cex if any text would overflow its allocated
# padding region. Export paths use exact-sized PNG/TIFF devices where 1
# user unit = 1 device pixel, so no fitting pass is needed - they render
# at the user's chosen fontsize verbatim.
.gel_render_annotated <- function(image, image_width, image_height,
                                  ladder_markers, well_markers,
                                  fontsize, bold,
                                  ladder_offset, well_offset,
                                  text_angle, bg_color,
                                  left_pad_use_fontsize_floor = TRUE,
                                  preview_fit = FALSE) {
  dims <- .gel_compute_canvas(image_width, image_height,
                              ladder_markers, well_markers,
                              ladder_offset, well_offset,
                              text_angle, fontsize,
                              left_pad_use_fontsize_floor)
  graphics::par(mar = c(0, 0, 0, 0))
  plot(1, type = "n",
       xlim = c(0, dims$total_width),
       ylim = c(0, dims$total_height),
       xlab = "", ylab = "", axes = FALSE, asp = 1)

  if (identical(bg_color, "white"))
    graphics::rect(0, 0, dims$total_width, dims$total_height,
                   col = "white", border = NA)

  img_raster <- grDevices::as.raster(image)
  graphics::rasterImage(img_raster, dims$left_pad, 0,
                        dims$left_pad + dims$img_width, dims$img_height)

  font_face <- if (isTRUE(bold)) 2 else 1
  cex_val   <- fontsize / 12

  # ---- Preview-only: shrink text to fit allocated padding -------------
  # On the Shiny preview device, asp=1 may have squeezed the canvas so
  # that user units shrink relative to the text's inches-based metrics.
  # Measure now and scale cex down if labels would overflow. Apply the
  # same shrinking factor to both ladder and well labels so they stay
  # visually consistent.
  ladder_cex <- cex_val
  well_cex   <- cex_val
  if (isTRUE(preview_fit)) {
    shrink <- 1
    if (nrow(ladder_markers) > 0 && dims$left_pad > 0) {
      labels   <- paste0(ladder_markers$mw, " kDa")
      meas_max <- max(vapply(labels, function(s)
        graphics::strwidth(s, units = "user", cex = cex_val), numeric(1)))
      # Available room is left_pad minus the gap (10) and tick (5)
      avail <- max(dims$left_pad - 15, 1)
      if (meas_max > avail) shrink <- min(shrink, avail / meas_max)
    }
    if (nrow(well_markers) > 0) {
      max_label <- well_markers$label[which.max(nchar(well_markers$label))]
      meas_w <- graphics::strwidth(max_label, units = "user", cex = cex_val)
      # Roughly how much room a well label has before falling off the
      # right edge of the canvas: distance from rightmost well marker to
      # right edge, accounting for the angle.
      rightmost_x <- if (nrow(well_markers) > 0)
        max(well_markers$x) * dims$scale_factor + dims$left_pad
        else dims$left_pad + dims$img_width
      avail_right <- dims$total_width - rightmost_x
      proj <- if (text_angle == 45) meas_w * sin(pi / 4)
              else if (text_angle == 90) graphics::strheight("Mg", units = "user",
                                                              cex = cex_val) / 2
              else meas_w / 2
      if (proj > avail_right && avail_right > 1)
        shrink <- min(shrink, avail_right / proj)
    }
    if (shrink < 1) {
      ladder_cex <- cex_val * shrink
      well_cex   <- cex_val * shrink
    }
  }

  # Ladder labels - kDa text on the left, tick line to the gel edge
  if (nrow(ladder_markers) > 0) {
    for (i in seq_len(nrow(ladder_markers))) {
      marker  <- ladder_markers[i, ]
      y_pos   <- marker$y * dims$scale_factor
      x_label <- dims$left_pad - 10
      graphics::text(x_label, y_pos, paste0(marker$mw, " kDa"),
                     pos = 2, cex = ladder_cex, font = font_face)
      graphics::segments(x_label + 5, y_pos, dims$left_pad, y_pos, lwd = 1)
    }
  }

  # Well labels - rotated text above the gel
  if (nrow(well_markers) > 0) {
    for (i in seq_len(nrow(well_markers))) {
      marker  <- well_markers[i, ]
      x_pos   <- marker$x * dims$scale_factor + dims$left_pad
      y_label <- dims$img_height + 10
      if (text_angle == 0) {
        graphics::text(x_pos, y_label, marker$label,
                       pos = 3, cex = well_cex, font = font_face, srt = 0)
      } else if (text_angle == 90) {
        graphics::text(x_pos, y_label, marker$label,
                       adj = c(0, 0.5), cex = well_cex,
                       font = font_face, srt = 90)
      } else {
        graphics::text(x_pos, y_label, marker$label,
                       adj = c(0, 0), cex = well_cex,
                       font = font_face, srt = 45)
      }
      graphics::segments(x_pos, y_label - 5, x_pos, dims$img_height, lwd = 1)
    }
  }
}

# Stitch the labelled western blot underneath an already-written gel PNG.
# Side effect: overwrites `gel_png_path` with the combined image. Mirrors
# the original `.gel_stack_western()` exactly.
.gel_stack_western <- function(state, input, gel_png_path,
                               format = c("png", "tiff")) {
  format   <- match.arg(format)
  west_src <- .western_processed(shiny::isolate(state$western_image),
                                 shiny::isolate(input$western_contrast))
  if (is.null(west_src)) return(gel_png_path)

  gap_px  <- max(0L, as.integer(shiny::isolate(input$gap_px)))
  gap_col <- shiny::isolate(input$gap_color)
  bg      <- if (!is.null(gap_col) && gap_col != "none") gap_col else "none"

  gel_img <- magick::image_read(gel_png_path)
  gel_w   <- as.integer(magick::image_info(gel_img)$width)

  gel_lad  <- shiny::isolate(state$ladder_markers)
  lad_off  <- if (!is.null(input$ladder_offset))
                shiny::isolate(input$ladder_offset) else 60L
  fsize    <- if (!is.null(input$fontsize))
                shiny::isolate(input$fontsize) else 20L
  cex_val  <- fsize / 12

  # Left pad on the western matches what the gel above already had. We
  # measure the gel's ladder labels (which is what the gel canvas was
  # padded for) so the western and gel stay aligned.
  left_pad <- if (nrow(gel_lad) > 0) {
    gel_labels <- paste0(gel_lad$mw, " kDa")
    measured   <- max(.gel_measure_widths_px(gel_labels, cex_val), 0)
    as.integer(max(measured + lad_off + 12, lad_off + 20L))
  } else 0L
  west_w <- max(1L, gel_w - left_pad)

  src_w <- as.integer(magick::image_info(west_src)$width)
  src_h <- as.integer(magick::image_info(west_src)$height)
  new_h <- max(1L, round(src_h * west_w / src_w))

  # Re-read MW values from the numeric inputs (user may have edited after marking)
  w_marks <- shiny::isolate(state$western_ladder_markers)
  if (nrow(w_marks) > 0) {
    for (i in seq_len(nrow(w_marks))) {
      v <- shiny::isolate(input[[paste0("wmw_", i)]])
      if (!is.null(v) && !is.na(v) && v > 0) w_marks$mw[i] <- v
    }
  }

  ab <- trimws(shiny::isolate(input$western_antibody) %||% "")
  # Antibody label column: measure the text width directly rather than
  # estimate from character count. The rotated/horizontal placement just
  # needs the rendered width plus a margin.
  ab_col_w <- if (nchar(ab) > 0) {
    ab_w <- .gel_measure_widths_px(ab, cex_val)
    as.integer(ceiling(ab_w + 16))
  } else 0L
  canvas_w <- gel_w + ab_col_w

  bg_col   <- if (!is.null(input$bg_color) && input$bg_color == "white")
                "white" else NA
  tmp_west <- tempfile(fileext = ".png")

  grDevices::png(tmp_west, width = canvas_w, height = new_h,
                 units = "px", bg = bg_col, res = 96)
  graphics::par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", xlim = c(0, canvas_w), ylim = c(0, new_h),
       xlab = "", ylab = "", axes = FALSE)

  graphics::rasterImage(grDevices::as.raster(west_src),
                        left_pad, 0, left_pad + west_w, new_h)

  if (nrow(w_marks) > 0) {
    for (i in seq_len(nrow(w_marks))) {
      mw_val <- w_marks$mw[i]
      if (!is.na(mw_val) && mw_val > 0) {
        # Scale click y (0=bottom in src_h space) to export y (0=bottom in new_h space)
        y_pos   <- w_marks$y[i] / src_h * new_h
        x_label <- left_pad - 10
        graphics::text(x_label, y_pos, paste0(mw_val, " kDa"),
                       pos = 2, cex = fsize / 12,
                       font = if (isTRUE(shiny::isolate(input$bold))) 2 else 1)
        graphics::segments(x_label + 5, y_pos, left_pad, y_pos, lwd = 1)
      }
    }
  }

  if (nchar(ab) > 0) {
    graphics::text(gel_w + 8, new_h / 2, ab, adj = c(0, 0.5),
                   cex = fsize / 12,
                   font = if (isTRUE(shiny::isolate(input$bold))) 2 else 1,
                   col = "black", srt = 0)
  }
  grDevices::dev.off()

  west_img <- magick::image_read(tmp_west)
  if (ab_col_w > 0) {
    pad     <- magick::image_blank(ab_col_w,
                magick::image_info(gel_img)$height, color = bg)
    gel_img <- magick::image_append(c(gel_img, pad), stack = FALSE)
  }
  gap <- magick::image_blank(canvas_w, max(1L, gap_px), color = bg)
  out <- magick::image_append(c(gel_img, gap, west_img), stack = TRUE)
  magick::image_write(out, path = gel_png_path, format = format)
  gel_png_path
}

# ---- Example image loaders -------------------------------------------------
# Both gel and western examples are bundled in inst/examples/ as small
# raster files (gel TIFF ~744 KB, western JPEG ~14 KB). We decode them
# via magick::image_read() and cache the resulting magick-image object
# in a per-loader environment so revisiting the tab doesn't re-decode.
#
# Magick can read directly from any file path (no compression hoops
# needed like AKTA's gzipped CSV), so the helpers are simple.
.gel_example_cache     <- new.env(parent = emptyenv())
.western_example_cache <- new.env(parent = emptyenv())

.gel_example_image <- function() {
  if (!is.null(.gel_example_cache$img)) return(.gel_example_cache$img)
  src <- .resolve_example_path("gel_example.tif")
  if (is.na(src)) return(NULL)
  tryCatch({
    img <- magick::image_read(src)
    .gel_example_cache$img <- img
    img
  }, error = function(e) NULL)
}

.western_example_image <- function() {
  if (!is.null(.western_example_cache$img)) return(.western_example_cache$img)
  src <- .resolve_example_path("western_example.jpg")
  if (is.na(src)) return(NULL)
  tryCatch({
    img <- magick::image_read(src)
    .western_example_cache$img <- img
    img
  }, error = function(e) NULL)
}

# Shared path resolver - same defensive pattern used elsewhere
.resolve_example_path <- function(filename) {
  app_dir_local <- if (exists("app_dir", envir = globalenv())) {
    get("app_dir", envir = globalenv())
  } else getwd()
  candidates <- unique(c(
    file.path(app_dir_local, "inst", "examples", filename),
    file.path(getwd(),       "inst", "examples", filename),
    file.path("inst", "examples", filename)
  ))
  candidates[file.exists(candidates)][1]
}

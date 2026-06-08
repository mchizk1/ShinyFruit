
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFiles
#' @noRd
app_ui <- function(request) {
  options(shiny.maxRequestSize = MAX_UPLOAD_BYTES)
  tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      title = tags$span(class = "sf-title", "ShinyFruit"),
      theme = bslib::bs_theme(
        bootswatch = "darkly",
        primary = "#7E57C2",
        "font-size-base" = "0.95rem"
      ),
      sidebar = bslib::sidebar(
        title = "Workflow",
        width = 380,
        class = "sf-sidebar",
        tags$p(
          class = "sf-subtitle",
          "Configure one sample image, then batch-apply settings to a folder."
        ),
        bslib::accordion(
          id = "workflow_accordion",
          open = FALSE,
          multiple = TRUE,
          bslib::accordion_panel(
            title = "1. Background & crop",
            icon = icon("image"),
            conditionalPanel(
              condition = "input.submitcrop < 1",
              fileInput(
                inputId = "sample_img",
                label = "Sample image",
                multiple = FALSE,
                accept = c("image/jpeg", "image/png", ".jpg", ".jpeg", ".png")
              ),
              shinyWidgets::prettySwitch(
                "preprocess", "Enhance contrast", value = TRUE, status = "primary"
              )
            ),
            conditionalPanel(
              condition = "output.fileUploaded && input.submitcrop < 1",
              shinyWidgets::prettySwitch(
                "standard", "Include size reference", value = FALSE, status = "primary"
              )
            ),
            conditionalPanel(
              condition = "input.standard > 0 && input.submitsize < 1",
              tags$p(class = "sf-hint", "Click two points on the image to measure a known distance."),
              fluidRow(
                column(8, verbatimTextOutput("line_len", placeholder = TRUE)),
                column(4, actionButton("clearclick", "Reset", class = "btn-sm btn-outline-light"))
              ),
              fluidRow(
                column(5, textInput("known_len", "Known length", placeholder = "e.g. 25")),
                column(4, shinyWidgets::pickerInput("units", "Units", UNITS)),
                column(3, actionButton("submitsize", "Set", class = "btn-sm btn-primary"))
              )
            ),
            conditionalPanel(
              condition = "output.fileUploaded && input.submitcrop < 1",
              tags$p(
                class = "sf-hint",
                "Brush over regions to exclude from analysis (e.g. a size reference or ruler)."
              )
            ),
            conditionalPanel(
              condition = "output.fileUploaded && input.submitcrop < 1",
              selectInput("col_space_bkg", "Background colorspace", COLORSPACES),
              sliderInput("channel1_bkg", "Channel 1", 0, 1, c(0, 1)),
              sliderInput("channel2_bkg", "Channel 2", 0, 1, c(0, 1)),
              sliderInput("channel3_bkg", "Channel 3", 0, 1, c(0, 1)),
              div(
                class = "sf-action-row",
                actionButton("clearcrop", "Reset crop", class = "btn-outline-light"),
                actionButton("submitcrop", "Continue to analysis", class = "btn-primary")
              )
            )
          ),
          bslib::accordion_panel(
            title = "2. Traits & thresholds",
            icon = icon("sliders"),
            conditionalPanel(
              condition = "input.submitcrop",
              shinyWidgets::checkboxGroupButtons(
                inputId = "variables",
                label = "Measurements to include",
                choices = TRAITS,
                status = "primary",
                size = "sm",
                checkIcon = list(yes = icon("check"))
              )
            ),
            conditionalPanel(
              condition = "input.variables.includes('Color-Based Feature')",
              tags$div(class = "sf-step-badge", "Color feature"),
              selectInput("col_space", "Feature colorspace", COLORSPACES),
              sliderInput("channel1", "Channel 1", 0, 1, c(0, 1)),
              sliderInput("channel2", "Channel 2", 0, 1, c(0, 1)),
              sliderInput("channel3", "Channel 3", 0, 1, c(0, 1)),
              fluidRow(
                column(
                  7,
                  checkboxGroupInput("CFops", "Display options", CF_OPS, selected = "Show Mask")
                ),
                column(5, selectInput("colfeature", "Mask color", MASK_COLORS))
              )
            ),
            conditionalPanel(
              condition = "input.variables.includes('Drupelet Count')",
              tags$div(class = "sf-step-badge", "Drupelet detection"),
              tags$p(
                class = "sf-hint",
                "Detects drupelets from surface edges (all colors). Raise edge threshold if over-counting; lower it if segments merge."
              ),
              sliderInput(
                "drp_sensitivity", "Edge threshold", 0.15, 0.85,
                DRUPELET_DEFAULTS$sensitivity, step = 0.05
              ),
              sliderInput(
                "drp_blur", "Smoothing (blur)", 0.5, 4,
                DRUPELET_DEFAULTS$blur_sigma, step = 0.5
              ),
              sliderInput(
                "drp_min_size", "Minimum drupelet size (px)", 1, 30,
                DRUPELET_DEFAULTS$min_size, step = 1
              )
            ),
            conditionalPanel(
              condition = "input.submitcrop",
              div(
                class = "sf-action-row",
                actionButton("step3", "Continue to batch", class = "btn-primary")
              )
            )
          ),
          bslib::accordion_panel(
            title = "3. Batch export",
            icon = icon("folder"),
            conditionalPanel(
              condition = "input.step3",
              shinyDirButton("folderbutton", "Select image folder", "Browse", class = "btn-primary"),
              shinyWidgets::prettySwitch(
                "imgbat", "Save QC images", value = FALSE, status = "primary"
              ),
              verbatimTextOutput("foldertxt", placeholder = TRUE),
              actionButton("runbutton", "Run batch analysis", class = "btn-primary btn-lg")
            )
          )
        )
      ),
      bslib::layout_column_wrap(
        width = 1,
        heights = c(2, 1),
        bslib::card(
          class = "sf-plot-card",
          full_screen = TRUE,
          bslib::card_header("Preview"),
          shinycssloaders::withSpinner(
            plotOutput(
              "image",
              click = clickOpts("img_click"),
              brush = brushOpts("img_crop", clip = FALSE, resetOnNew = FALSE),
              height = "460px",
              width = "100%"
            ),
            type = 4,
            color = "#7E57C2",
            hide.ui = TRUE
          )
        ),
        bslib::card(
          class = "sf-results-card",
          bslib::card_header("Live results"),
          tags$div(class = "sf-hint", "Updates after background removal and trait selection."),
          verbatimTextOutput("txtout", placeholder = TRUE)
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "ShinyFruit"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}


colorspaces <- c("RGB", "HSB", "Lab")
RGB <- c("Red", "Green", "Blue")
HSB <- c("Hue", "Saturation", "Lightness")
variableList <- c("Color-Based Feature", "Color Profile", "Size")

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFiles
#' @noRd
app_ui <- function(request) {
  options(shiny.maxRequestSize = 30*1024^2)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(bootswatch = "darkly"),
      titlePanel(
        h1("ShinyFruit Imaging Lab", align = "center")
      ),
      headerPanel(""),
      fluidRow(sidebarLayout(
        mainPanel(
          column(12, align = "center",
                        plotOutput("image", click = clickOpts("img_click", F),
                                          brush = brushOpts("img_crop", clip = F, resetOnNew = F),
                                          height = "58%", width = "100%")),
          verbatimTextOutput("debug")
          # ,conditionalPanel(condition = "output.fileUploaded",
          #
        ),
        sidebarPanel(
          style = "height: 90vh; overflow-y: auto; background-color: #2E2848;",
          h3("Control Panel"),
          conditionalPanel(condition = "input.submitcrop < 1",
                                  h4("Step 1: Apply Background Filters"),
                                  fileInput(inputId = "sample_img",
                                                   label= "Select Image",
                                                   multiple =T,
                                                   placeholder = NULL),
                                  shinyWidgets::prettySwitch("preprocess", "Enhance Contrast?", value = T)),
          conditionalPanel(condition = "output.fileUploaded && input.submitcrop < 1",
                                  shinyWidgets::prettySwitch("standard", "Do you have a size reference?")),
          conditionalPanel(condition = "input.standard > 0 && input.submitsize < 1",
                                  h4("Locate size reference."),
                                  fluidRow(column(8, verbatimTextOutput("line_len", placeholder = T)),
                                                  column(4, actionButton("clearclick", "Reset"))),
                                  fluidRow(column(5, textInput("known_len", NULL, placeholder = "Known?")),
                                                  column(3, shinyWidgets::pickerInput("units", NULL, c("mm", "cm", "inches"))),
                                                  column(4, actionButton("submitsize", "Submit")))),
          conditionalPanel(condition = "input.submitsize > 0 && input.submitcrop < 1",
                                  h4("Crop out size reference")),
          conditionalPanel(condition = "output.fileUploaded && input.submitcrop < 1",
                                  selectInput(inputId = "col_space_bkg",
                                                     label = "Select Colorspace",
                                                     choices = colorspaces),
                                  sliderInput(inputId = "channel1_bkg",
                                                     label = "Red",
                                                     min = 0, max = 1, value = c(0, 1)),
                                  sliderInput(inputId = "channel2_bkg",
                                                     label = "Blue",
                                                     min = 0, max = 1, value = c(0, 1)),
                                  sliderInput(inputId = "channel3_bkg",
                                                     label = "Green",
                                                     min = 0, max = 1, value = c(0, 1)),
                                  fluidRow(column(3, actionButton("clearcrop", "Reset")),
                                                  column(3, actionButton("submitcrop", "Proceed to Step 2")))),
          conditionalPanel(condition = "input.submitcrop",
                                  h4("Step 2: Customize Analysis"),
                                  checkboxGroupInput(inputId = "variables",
                                                            label = "Select Variables",
                                                            choices = variableList)),
          conditionalPanel(condition = "input.variables.includes('Color-Based Feature')",
                                  selectInput(inputId = "col_space",
                                                     label = "Select Colorspace",
                                                     choices = colorspaces),
                                  sliderInput(inputId = "channel1",
                                                     label = "Red",
                                                     min = 0, max = 1, value = c(0, 1)),
                                  sliderInput(inputId = "channel2",
                                                     label = "Blue",
                                                     min = 0, max = 1, value = c(0, 1)),
                                  sliderInput(inputId = "channel3",
                                                     label = "Green",
                                                     min = 0, max = 1, value = c(0, 1)),
                                  fluidRow(column(6, checkboxGroupInput("CFops", "Output Options",
                                                                                             choices = c("Despeckle", "Show Mean RGB",
                                                                                                         "Show Mask"),
                                                                                             selected = "Show Mask")),
                                                  column(6, selectInput("colfeature", "Mask Color",
                                                                                      c("red", "green", "blue", "black", "white"))))),
          conditionalPanel(condition = "input.submitcrop",
                                  actionButton("step3", "Proceed to Step 3")),
          conditionalPanel(condition = "input.step3",
                                  fluidRow(#column(6, actionButton("folderbutton", "Choose Input Directory")),
                                    column(6, shinyDirButton("folderbutton", "Choose Input Directory", "Upload")),
                                    column(6, shinyWidgets::prettySwitch("imgbat", "Include Images in Output"))),
                                  fluidRow(column(6, verbatimTextOutput("foldertxt", T)),
                                                  column(6, actionButton(inputId = "runbutton", label = "Run Batch")))),
          verbatimTextOutput("txtout")
        )
      )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinyFruit"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

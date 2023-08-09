colorspaces <- c("RGB", "HSB", "Lab")
RGB <- c("Red", "Green", "Blue")
HSB <- c("Hue", "Saturation", "Lightness")
variableList <- c("Color-Based Feature", "Color Profile", "Size")
options(shiny.maxRequestSize = 30*1024^2)
ui <- shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  shiny::titlePanel(
    shiny::h1("ShinyFruit Imaging Lab", align = "center")
  ),
  shiny::headerPanel(""),
  shiny::fluidRow(shiny::sidebarLayout(
    shiny::mainPanel(
      shiny::column(12, align = "center",
                    shiny::plotOutput("image", click = shiny::clickOpts("img_click", F),
                                      brush = shiny::brushOpts("img_crop", clip = F, resetOnNew = F),
                                      height = "58%", width = "100%")),
      shiny::verbatimTextOutput("debug")
      # ,shiny::conditionalPanel(condition = "output.fileUploaded",
      #
    ),
    shiny::sidebarPanel(
      style = "height: 90vh; overflow-y: auto; background-color: #2E2848;",
      shiny::h3("Control Panel"),
      shiny::conditionalPanel(condition = "input.submitcrop < 1",
                              shiny::h4("Step 1: Apply Background Filters"),
                              shiny::fileInput(inputId = "sample_img",
                                               label= "Select Image",
                                               multiple =T,
                                               placeholder = NULL),
                              shinyWidgets::prettySwitch("preprocess", "Enhance Contrast?", value = T)),
      shiny::conditionalPanel(condition = "output.fileUploaded && input.submitcrop < 1",
                              shinyWidgets::prettySwitch("standard", "Do you have a size reference?")),
      shiny::conditionalPanel(condition = "input.standard > 0 && input.submitsize < 1",
                              shiny::h4("Locate size reference."),
                              shiny::fluidRow(shiny::column(8, shiny::verbatimTextOutput("line_len", placeholder = T)),
                                              shiny::column(4, shiny::actionButton("clearclick", "Reset"))),
                              shiny::fluidRow(shiny::column(5, shiny::textInput("known_len", NULL, placeholder = "Known?")),
                                              shiny::column(3, shinyWidgets::pickerInput("units", NULL, c("mm", "cm", "inches"))),
                                              shiny::column(4, shiny::actionButton("submitsize", "Submit")))),
      shiny::conditionalPanel(condition = "input.submitsize > 0 && input.submitcrop < 1",
                              shiny::h4("Crop out size reference")),
      shiny::conditionalPanel(condition = "output.fileUploaded && input.submitcrop < 1",
                              shiny::selectInput(inputId = "col_space_bkg",
                                                 label = "Select Colorspace",
                                                 choices = colorspaces),
                              shiny::sliderInput(inputId = "channel1_bkg",
                                                 label = "Red",
                                                 min = 0, max = 1, value = c(0, 1)),
                              shiny::sliderInput(inputId = "channel2_bkg",
                                                 label = "Blue",
                                                 min = 0, max = 1, value = c(0, 1)),
                              shiny::sliderInput(inputId = "channel3_bkg",
                                                 label = "Green",
                                                 min = 0, max = 1, value = c(0, 1)),
                              shiny::fluidRow(shiny::column(3, shiny::actionButton("clearcrop", "Reset")),
                                              shiny::column(3, shiny::actionButton("submitcrop", "Proceed to Step 2")))),
      shiny::conditionalPanel(condition = "input.submitcrop",
                              shiny::h4("Step 2: Customize Analysis"),
                              shiny::checkboxGroupInput(inputId = "variables",
                                                        label = "Select Variables",
                                                        choices = variableList)),
      shiny::conditionalPanel(condition = "input.variables.includes('Color-Based Feature')",
                              shiny::selectInput(inputId = "col_space",
                                                 label = "Select Colorspace",
                                                 choices = colorspaces),
                              shiny::sliderInput(inputId = "channel1",
                                                 label = "Red",
                                                 min = 0, max = 1, value = c(0, 1)),
                              shiny::sliderInput(inputId = "channel2",
                                                 label = "Blue",
                                                 min = 0, max = 1, value = c(0, 1)),
                              shiny::sliderInput(inputId = "channel3",
                                                 label = "Green",
                                                 min = 0, max = 1, value = c(0, 1)),
                              shiny::fluidRow(shiny::column(6, shiny::checkboxGroupInput("CFops", "Output Options",
                                                                                         choices = c("Despeckle", "Show Mean RGB",
                                                                                                     "Show Mask"),
                                                                                         selected = "Show Mask")),
                                              shiny::column(6, shiny::selectInput("colfeature", "Mask Color",
                                                                                  c("red", "green", "blue", "black", "white"))))),
      shiny::conditionalPanel(condition = "input.submitcrop",
                              shiny::actionButton("step3", "Proceed to Step 3")),
      shiny::conditionalPanel(condition = "input.step3",
                              shiny::fluidRow(#shiny::column(6, shiny::actionButton("folderbutton", "Choose Input Directory")),
                                shiny::column(6, shinyFiles::shinyDirButton("folderbutton", "Choose Input Directory", "Upload")),
                                shiny::column(6, shinyWidgets::prettySwitch("imgbat", "Include Images in Output"))),
                              shiny::fluidRow(shiny::column(6, shiny::verbatimTextOutput("foldertxt", T)),
                                              shiny::column(6, shiny::actionButton(inputId = "runbutton", label = "Run Batch")))),
      shiny::verbatimTextOutput("txtout")
    )
  )
  )
)

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyFiles
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = MAX_UPLOAD_BYTES)

  step1_state <- mod_step1_background_server(input, output, session)
  step2_state <- mod_step2_analysis_server(input, output, session, step1_state)
  mod_step3_batch_server(input, output, session, step1_state, step2_state)
  mod_image_plot_server(input, output, session, step1_state, step2_state)

  shiny::outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
}

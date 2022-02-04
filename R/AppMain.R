#' GUI for Interactive Image Batch Processing and Analysis
#'
#' Running this function with no arguments will launch a general user interface for
#' running batch image analyses on images of small fruits.
#'
#' @examples
#' ShinyFruit()
#'
#' @export

ShinyFruit <- function(){
  colorspaces <- c("RGB", "HSB", "Lab")
  RGB <- c("Red", "Green", "Blue")
  HSB <- c("Hue", "Saturation", "Lightness")
  variableList <- c("RDR", "Drupelet Count", "Size")
  options(shiny.maxRequestSize = 30*1024^2)
  shiny::shinyApp(ui = ui, server = server)
}

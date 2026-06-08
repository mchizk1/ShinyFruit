#' Shared application constants
#' @noRd
COLORSPACES <- c("RGB", "HSB", "Lab")

#' @noRd
TRAITS <- c("Color-Based Feature", "Color Profile", "Size", "Drupelet Count")

#' @noRd
CF_OPS <- c("Despeckle", "Show Mean RGB", "Show Mask")

#' @noRd
MASK_COLORS <- c("red", "green", "blue", "black", "white")

#' @noRd
UNITS <- c("mm", "cm", "inches")

#' @noRd
PLOT_WIDTH <- 650

#' @noRd
PLOT_HEIGHT <- 433

#' @noRd
MAX_UPLOAD_BYTES <- 30 * 1024^2

#' Max dimension for interactive preview (analysis still uses ANALYSIS_MAX_DIM)
#' @noRd
PREVIEW_MAX_DIM <- 1200L

#' @noRd
ANALYSIS_MAX_DIM <- 1500L

#' Slider debounce delay in milliseconds (step 1 background preview)
#' @noRd
DEBOUNCE_MS <- 450L

#' Debounce for step 2 analysis sliders (single layer — avoids spinner stutter)
#' @noRd
ANALYSIS_DEBOUNCE_MS <- 450L

#' Defaults for boundary-based drupelet detection
#' @noRd
DRUPELET_DEFAULTS <- list(
  min_size = 4L,
  blur_sigma = 1.5,
  sensitivity = 0.5
)

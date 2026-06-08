# Script to create internal package datasets
# Run from package root: Rscript data-raw/create_data.R

out_dir <- file.path("data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Colorspace channel labels used by slider UI
cs_labs <- data.frame(
  RGB = c("Red", "Green", "Blue"),
  HSB = c("Hue", "Saturation", "Value"),
  Lab = c("L*", "a*", "b*"),
  row.names = c("channel1", "channel2", "channel3"),
  stringsAsFactors = FALSE
)

# RHS color lookup table for nearest-color matching
if (requireNamespace("ColorNameR", quietly = TRUE)) {
  rhs_vals <- ColorNameR::rhs_color_values_2007
  rhs_names <- ColorNameR::rhs_color_names_2015
  rgb2rhs <- merge(
    rhs_vals[, c("RHS", "R", "G", "B")],
    rhs_names[, c("RHS", "english")],
    by = "RHS",
    all.x = TRUE
  )
  rgb2rhs$english[is.na(rgb2rhs$english)] <- rgb2rhs$RHS[is.na(rgb2rhs$english)]
  rgb2rhs <- rgb2rhs[, c("R", "G", "B", "english")]
} else {
  rgb2rhs <- data.frame(
    R = c(128L, 200L, 80L, 40L, 220L),
    G = c(128L, 50L, 150L, 20L, 180L),
    B = c(128L, 50L, 60L, 10L, 40L),
    english = c("grey", "red", "green", "brown", "yellow"),
    stringsAsFactors = FALSE
  )
}

# Output variable dictionary (matches README)
variable_desc <- data.frame(
  Variable = c(
    "File", "Colorspace", "BkgCh1Threshold", "BkgCh2Threshold", "BkgCh3Threshold",
    "MeanRGB", "RHSDarkColor", "RHSMidColor", "RHSLightColor",
    "FtColorSpace", "FtCh1Threshold", "FtCh2Threshold", "FtCh3Threshold",
    "Feature_prop", "FeatureRGB", "FeatureDarkRHS", "FeatureMidRHS", "FeatureLightRHS",
    "BerryCount", "Length", "Width", "Size", "DrupeletsPerBerry"
  ),
  Context = c(
    rep("General", 5),
    rep("Color profile", 4),
    rep("Color feature analysis", 9),
    rep("Size analysis", 4),
    "Drupelet analysis"
  ),
  Description = c(
    "The name of the file analyzed",
    "Color space chosen to remove background area",
    "Value set for color channel 1 in background removal",
    "Value set for color channel 2 in background removal",
    "Value set for color channel 3 in background removal",
    "Mean RGB value of the image (after background removal)",
    "Nearest RHS color to the darkest value of the image",
    "Nearest RHS color to the median value of the image",
    "Nearest RHS color to the lightest value of the image",
    "Color space chosen to analyze a colored feature",
    "Value set for color channel 1 in color feature analysis",
    "Value set for color channel 2 in color feature analysis",
    "Value set for color channel 3 in color feature analysis",
    "Proportion of the image occupied by the color feature",
    "Median RGB value of the extracted color feature",
    "Nearest RHS color to the darkest value of the color feature",
    "Nearest RHS color to the median value of the color feature",
    "Nearest RHS color to the lightest value of the color feature",
    "Number of fruit detected after removing background area",
    "Mean length of fruit detected",
    "Mean width of fruit detected",
    "Mean area of fruit detected",
    "Mean number of drupelets per berry"
  ),
  stringsAsFactors = FALSE
)

save(cs_labs, file = file.path(out_dir, "cs_labs.rda"))
save(rgb2rhs, file = file.path(out_dir, "rgb2rhs.rda"))
save(variable_desc, file = file.path(out_dir, "variable_desc.rda"))

cat("Created data files in", out_dir, "\n")

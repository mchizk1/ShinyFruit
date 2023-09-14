# ShinyFruit

An R shiny application for interactive image phenotyping of fruits and vegetables. The main goal of
creating this app was to provide a reasonably simple and tailored solution for breeders and horticulturalists
to efficiently collect data about size, color, and damage across large numbers of images.  The user 
simply sets parameters for a single sample image and those settings are applied to all images in a given
directory. In its present state, one can use ShinyFruit to measure the following traits:

* Length
* Width
* Area
* Fruit Count
* Proportions of Discolored Regions (i.e. Red Drupelet Reversion in blackberry)
* Median RGB values
* RHS descriptive color profiles

Please feel free to share any feedback, bugs, or new feature ideas with me so that I can continue
to improve this tool!

## Installation

    install.packages("devtools")
    devtools::install_github("mchizk1/ShinyFruit", build_vignettes = T)
    
## Check out the vignette for a full tutorial

    vignette("ShinyFruit-Tutorial")
    
## Or just jump right in by booting up the app!

    ShinyFruit::run_app()

## What's happening behind the scenes?

When the user loads up an image, it is automatically edited with the magick package in a few ways to optimize 
for speed and feature detection:

* Images are downsized so that the maximum dimension is capped at 1500 pixels (for speed).
* Contrast is increased by normalizing pixel values to span the full color range.
* Differences in color intensity are sharpened
* Images are enhanced to reduce noise or inconsistencies

After the image is loaded, and the user moves on to set thresholds for background removal, where images are automatically
despeckled to remove small, unimportant islands of pixels.  This is important because the user will not want to count all 
the spots of dirt or juice when it's time for analysis. This process is achieved by first shrinking and then swelling 
detected islands of pixels.  In the end, only the larger objects will remain. Despeckling becomes optional when using
the "Color-Based Feature" trait in the analysis stage.

## Output data dictionary

The csv output file produced by ShinyFruit can contain a number of different 
variable columns, which are described in the table below:

| Variable        | Context                | Description                                         |
| --------------- | -----------------------| --------------------------------------------------- |
| File            | General                | The name of the file analyzed                       |
| Colorspace      | General                | Color space chosen to remove background area        |
| BkgCh1Threshold | General                | Value set for color channel 1 in background removal |
| BkgCh2Threshold | General                | Value set for color channel 2 in background removal |
| BkgCh3Threshold | General                | Value set for color channel 3 in background removal |
| MeanRGB         | Color profile          | Mean RGB value of the image (after background removal) |
| RHSDarkColor    | Color profile          | Nearest Royal Horticultural Society color value to the darkest value of the image (after background removal) |
| RHSMidColor     | Color profile          | Nearest Royal Horticultural Society color value to the median value of the image (after background removal) |
| RHSLightColor   | Color profile          | Nearest Royal Horticultural Society color value to the lightest value of the image (after background removal) |
| FtColorSpace    | Color feature analysis | Color space chosen to analyze a colored feature |
| FtCh1Threshold  | Color feature analysis | Value set for color channel 1 in color feature analysis |
| FtCh2Threshold  | Color feature analysis | Value set for color channel 2 in color feature analysis |
| FtCh3Threshold  | Color feature analysis | Value set for color channel 3 in color feature analysis |
| Feature_prop    | Color feature analysis | Proportion of the the image occupied by the color feature (after background removal) |
| FeatureRGB      | Color feature analysis | Median RGB value of the extracted color feature |
| FeatureDarkRHS  | Color feature analysis | Nearest Royal Horticultural Society color value to the darkest value of the color feature |
| FeatureMidRHS   | Color feature analysis | Nearest Royal Horticultural Society color value to the median value of the color feature |
| FeatureLightRHS | Color feature analysis | Nearest Royal Horticultural Society color value to the lightest value of the color feature |
| BerryCount      | Size analysis          | Number of fruit detected after removing background area |
| Length          | Size analysis          | Mean length of fruit detected |
| Width           | Size analysis          | Mean width of fruit detected |
| Size            | Size analysis          | Mean area of fruit detected |

**To print this table in R** simpley run the function `show_variables()`

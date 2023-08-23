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


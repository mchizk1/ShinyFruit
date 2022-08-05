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

## Installation

    install.packages("devtools")
    devtools::install_github("mchizk1/ShinyFruit", build_vignettes = T)
    
## Check out the vignette for a full tutorial

    vignette("ShinyFruit-Tutorial")
    
## Or just jump right in by booting up the app!

    ShinyFruit::ShinyFruit()
    
Please feel free to share any feedback, bugs, or new feature ideas with me so that I can continue
to improve this tool!

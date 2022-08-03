# ShinyFruit

An R shiny application for interactive image phenotyping of small fruits. This app
is currently optimized for handling blackberry images, but future updates will
focus on expanding flexible data collection options over a wider range of horticultural 
crops. In its present state, one can use ShinyFruit to measure the following traits:

* Length
* Width
* Area
* Fruit Count
* Drupelet Count
* Discolored Regions (i.e. Red Drupelet Reversion)

## Installation

    install.packages("devtools")
    devtools::install_github("mchizk1/ShinyFruit")
    
## To run the app locally...

    ShinyFruit::ShinyFruit()
    
Please feel free to share any feedback, bugs, or new feature ideas with me so that I can continue
to improve this tool!

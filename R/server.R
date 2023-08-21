colorspaces <- c("RGB", "HSB", "Lab")
RGB <- c("Red", "Green", "Blue")
HSB <- c("Hue", "Saturation", "Lightness")
variableList <- c("Color-Based Feature", "Color Profile", "Size")
options(shiny.maxRequestSize = 30*1024^2)
server <- function(input, output, session){

  Values <- reactiveValues(old="start")
  session$onFlush(once=FALSE, function(){
    shiny::isolate({ Values$old<-input$col_space })
  })

  output$fileUploaded <- shiny::reactive({
    output$fileUploaded <- shiny::renderText(input$sample_img$name)
  })

  #This observer updates the image when the user selects a file
  image1 <- shiny::eventReactive(list(input$sample_img$datapath, input$preprocess), {
    image1 <- bkb_process(input$sample_img$datapath, input$preprocess)
    return(image1)})
  cs_cimg <- shiny::reactive({switchspace(img_na(), input$col_space)})
  cs_bkg  <- shiny::reactive({switchspace(image1(), input$col_space_bkg)})
  # shiny::observeEvent(input$sample_img, {
  #   output$image <- shiny::renderPlot(
  #     {image1() %>%
  #         plot()},
  #     width = 800, height = 533
  #   )
  # })

  click <- reactiveVal(0)
  click1 <- reactiveVal(NULL)
  click2 <- reactiveVal(NULL)
  crop <- reactiveVal(c(0,0,0,0))
  sz_conv <- reactiveVal(1)
  line_len <- reactiveVal(NULL)
  output$line_len <- renderText({"Click any two points to measure"})
  shiny::observeEvent(input$img_click, {
    click(isolate(click()+1))
    if(click() == 1){
      click1(c(round(input$img_click$x), round(input$img_click$y)))
    } else if (click() == 2){
      click2(c(round(input$img_click$x), round(input$img_click$y)))
      line_len(pythag(click1(), click2()))
      output$line_len <- shiny::renderText({paste0(round(line_len(), 1), " pixels")})
    }
  })
  shiny::observeEvent(input$img_crop, {
    if(input$submitsize > 0 & input$submitcrop < 1){
      crop(c(round(input$img_crop$xmin),
             round(input$img_crop$xmax),
             round(input$img_crop$ymin),
             round(input$img_crop$ymax)))
    }
  })
  shiny::observeEvent(input$submitsize, {
    if(!is.null(click2()) & !is.null(input$known_len)){
      sz_conv(as.numeric(input$known_len)/line_len())
    }
  })
  shiny::observeEvent(input$clearclick, {
    click(0)
    click1(NULL)
    click2(NULL)
    line_len(NULL)
  })
  shiny::observeEvent(input$clearcrop, {
    crop(NULL)
  })

  #This observer plots a distribution of colors for the RDR sliders
  cs <- shiny::reactive({
    list(input$col_space, input$variables)
  })

  shiny::observeEvent(cs(), {
    if("Color-Based Feature" %in% input$variables){
      labs <- ShinyFruit::cs_labs[,colnames(ShinyFruit::cs_labs) == input$col_space]
      shiny::updateSliderInput(session, inputId = "channel1", label = labs[1],
                               min = floor(min(imager::R(cs_cimg()), na.rm = T))-0.001,
                               max = ceiling(max(imager::R(cs_cimg()), na.rm = T))+0.001,
                               value = c(floor(min(imager::R(cs_cimg()), na.rm = T))-0.001,
                                         ceiling(max(imager::R(cs_cimg()), na.rm = T))+0.001))
      shiny::updateSliderInput(session, inputId = "channel2", label = labs[2],
                               min = floor(min(imager::G(cs_cimg()), na.rm = T))-0.001,
                               max = ceiling(max(imager::G(cs_cimg()), na.rm = T))+0.001,
                               value = c(floor(min(imager::G(cs_cimg()), na.rm = T))-0.001,
                                         ceiling(max(imager::G(cs_cimg()), na.rm = T))+0.001))
      shiny::updateSliderInput(session, inputId = "channel3", label = labs[3],
                               min = floor(min(imager::B(cs_cimg()), na.rm = T))-0.001,
                               max = ceiling(max(imager::B(cs_cimg()), na.rm = T))+0.001,
                               value = c(floor(min(imager::B(cs_cimg()), na.rm = T))-0.001,
                                         ceiling(max(imager::B(cs_cimg()), na.rm = T))+0.001))
      output$cs_hist <- shiny::renderPlot({
        layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
        par(mar = c(10, 0, 0, 0))
        hist(imager::R(cs_cimg()), col=rgb(1,0,0,0.5),
             xlab = NULL, ylab = NULL, main = NULL, nclass = 50)
        hist(imager::G(cs_cimg()), col=rgb(0,1,0,0.5),
             xlab = NULL, ylab = NULL, main = NULL, nclass = 50)
        hist(imager::B(cs_cimg()), col=rgb(0,0,1,0.5),
             xlab = NULL, ylab = NULL, main = NULL, nclass = 50)
      }, width = 600)
    }
  }, ignoreInit = T)

  #This observer plots a distribution of colors for the background sliders

  shiny::observeEvent(list(input$col_space_bkg, input$sample_img), {
    if(!is.null(input$sample_img)){
      labs <- ShinyFruit::cs_labs[,colnames(ShinyFruit::cs_labs) == input$col_space_bkg]
      shiny::updateSliderInput(session, inputId = "channel1_bkg", label = labs[1],
                               min = floor(min(imager::R(cs_bkg()), na.rm = T))-0.001,
                               max = ceiling(max(imager::R(cs_bkg()), na.rm = T))+0.001,
                               value = c(floor(min(imager::R(cs_bkg()), na.rm = T))-0.001,
                                         ceiling(max(imager::R(cs_bkg()), na.rm = T))+0.001))
      shiny::updateSliderInput(session, inputId = "channel2_bkg", label = labs[2],
                               min = floor(min(imager::G(cs_bkg()), na.rm = T))-0.001,
                               max = ceiling(max(imager::G(cs_bkg()), na.rm = T))+0.001,
                               value = c(floor(min(imager::G(cs_bkg()), na.rm = T))-0.001,
                                         ceiling(max(imager::G(cs_bkg()), na.rm = T))+0.001))
      shiny::updateSliderInput(session, inputId = "channel3_bkg", label = labs[3],
                               min = floor(min(imager::B(cs_bkg()), na.rm = T))-0.001,
                               max = ceiling(max(imager::B(cs_bkg()), na.rm = T))+0.001,
                               value = c(floor(min(imager::B(cs_bkg()), na.rm = T))-0.001,
                                         ceiling(max(imager::B(cs_bkg()), na.rm = T))+0.001))
      output$cs_hist <- shiny::renderPlot({
        layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
        par(mar = c(10, 0, 0, 0))
        hist(imager::R(cs_bkg()), col=rgb(1,0,0,0.5),
             xlab = NULL, ylab = NULL, main = NULL, nclass = 50)
        hist(imager::G(cs_bkg()), col=rgb(0,1,0,0.5),
             xlab = NULL, ylab = NULL, main = NULL, nclass = 50)
        hist(imager::B(cs_bkg()), col=rgb(0,0,1,0.5),
             xlab = NULL, ylab = NULL, main = NULL, nclass = 50)
      }, width = 600)
    }
  }, ignoreInit = T)

  # Creating stats outputs from checkbox inputs
  BerOut <- shiny::eventReactive(img_na(), {
    ber_num <- BerSummary(img_na(), stats=F)
    return(ber_num)
  })
  DrpOut <- shiny::reactive({
    if("Drupelet Count" %in% input$variables){
      DrpSummary(img_na())
    }
  })
  SzOut <- shiny::reactive({
    if("Size" %in% input$variables){
      BerSummary(img_na())
    }
  })
  ColOut <- shiny::reactive({
    if("Color Profile" %in% input$variables){
      ColProfile(img_na(), input$col_space)
    }
  })
  RDR_px <- shiny::reactive({
    if("Color-Based Feature" %in% input$variables){
      RedDrupe(cs_cimg(), input$channel1, input$channel2,
               input$channel3, ("Despeckle" %in% input$CFops),
               T)
    }
  })
  CfOut <- shiny::reactive({
    if("Color-Based Feature" %in% input$variables){
      ColProfile(cf_na(), input$col_space)
    }
  })

  # Rendering reactive text output
  BerTxt <- reactive({ paste0("Berry Count: ", BerOut()) })
  ColTxt <- reactive({ if (is.list(ColOut())){
    paste0("\nMid RGB Value: (", round(ColOut()$red),
           ", ", round(ColOut()$green), ", ", round(ColOut()$blue), ")",
           "\nDarkest RHS Color: ", ColOut()$dark_color,
           "\nMid RHS Color: ", ColOut()$mid_color,
           "\nLightest RHS Color: ", ColOut()$light_color)
  } else {
    paste0("")
  }
  })
  SzTxt <- reactive({ if(is.data.frame(SzOut())){
    paste0("\nMean Length: ", round(mean(SzOut()$L*sz_conv()))," ",input$units,
           "\nMean Width: ", round(mean(SzOut()$W*sz_conv()))," ",input$units,
           "\nMean Size: ", round(mean(SzOut()$Size*(sz_conv()^2))), " ", input$units, " squared")
  } else {
    paste0("")
  }
  })
  DrpTxt <- reactive({ if(is.data.frame(DrpOut())){
    paste0("\nDrupelets/Berry: ", round(nrow(DrpOut())/BerOut()))
  } else {
    paste0("")
  }
  })
  RdrTxt <- reactive({ if (imager::is.pixset(RDR_px()) &
                          "Show Mean RGB" %in% input$CFops){
    red_px <- sum(RDR_px())
    black_px <-  sum(!imager::px.na(imager::R(cs_cimg())))
    paste0("\nFeature Detected: ", round(100*(red_px/black_px), 2), "%",
           "\nMean Feature RGB: (", round(CfOut()$red),
           ", ", round(CfOut()$green), ", ", round(CfOut()$blue), ")",
           "\nFeature Darkest RHS: ", CfOut()$dark_color,
           "\nFeature Mid RHS: ", CfOut()$mid_color,
           "\nFeature Lightest RHS: ", CfOut()$light_color)
  } else if (imager::is.pixset(RDR_px())){
    red_px <- sum(RDR_px())
    black_px <-  sum(!imager::px.na(imager::R(cs_cimg())))
    paste0("\nFeature Detected: ", round(100*(red_px/black_px), 2), "%")
  } else {
    paste0("")
  }
  })
  output$txtout <- shiny::renderText({
    paste0(BerTxt(), SzTxt(), ColTxt(), DrpTxt(), RdrTxt())
  })

  step1 <- shiny::reactive({
    list(input$channel1_bkg, input$channel2_bkg, input$channel3_bkg)
  })

  img_step2 <- shiny::eventReactive(input$submitcrop, {
    if(is.numeric(crop())){
      bkb_background(image1(), crop(), F, input$col_space_bkg, input$channel1_bkg, input$channel2_bkg, input$channel3_bkg)
    }
  })
  img_na <- shiny::eventReactive(input$submitcrop, {
    if(is.numeric(crop())){
      bkb_background(image1(), crop(), T, input$col_space_bkg, input$channel1_bkg, input$channel2_bkg, input$channel3_bkg)
    }
  })
  cf_na <- shiny::eventReactive(step2(), {
    if(imager::is.pixset(RDR_px())){
      tmp_step2 <- img_step2()
      tmp_step2[imager::as.pixset(1-RDR_px())] <- NA
      return(tmp_step2)
    }
  })
  shiny::observeEvent(img_step2(), {
    output$image <- renderPlot({
      img_step2() %>%
        imager::cimg2magick() %>%
        magick::image_flop() %>%
        magick::image_ggplot()
    }, width = 650, height = 433)
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  })


  # Masks for visual quality checking

  drp_px <- shiny::reactive({ DrpPlot(img_na(), DrpOut()) })

  step2 <- shiny::reactive({
    list(input$channel1, input$channel2, input$channel3, ("Despeckle" %in% input$CFops),
         input$variables, input$colfeature)
  })

  shiny::observeEvent(step2(), {
    image_mask <- img_step2()
    if("Drupelet Count" %in% input$variables){
      image_mask <- imager::colorise(image_mask, drp_px(), col = "green")
    }
    if("Color-Based Feature" %in% input$variables &
       imager::is.pixset(RDR_px()) & "Show Mask" %in% input$CFops){
      image_mask <- imager::colorise(image_mask, RDR_px(),
                                     col = col2rgb(input$colfeature), alpha = 0.5)
    }
    output$image <- shiny::renderPlot({
      image_mask %>%
        imager::cimg2magick() %>%
        magick::image_flop() %>%
        magick::image_ggplot()
    }, width = 650, height = 433)
  }, ignoreInit = T)

  # Initial ggplot layer specifications (mostly off of the plot area for null-ish values)
  fruit_img <- shiny::reactiveValues(
    main = magick::image_ggplot(magick::image_read(system.file("blackberry.png", package = "ShinyFruit"))),
                            oneclk = ggplot2::geom_blank(),
                            twoclk = ggplot2::geom_blank(),
                            lineref = ggplot2::geom_blank(),
                            crop = ggplot2::geom_blank(),
                            background = ggplot2::geom_blank())

  # Initial layer - just the fruit image
  shiny::observeEvent(input$sample_img, {
    fruit_img$main <- image1() %>%
      imager::cimg2magick() %>%
      magick::image_flop() %>%
      magick::image_ggplot()
  })
  # first click layer (red dot)
  shiny::observeEvent(click1(), {
    fruit_img$oneclk <- ggplot2::geom_point(ggplot2::aes(click1()[1],
                                                         click1()[2]),
                                            color = "red")
  }, ignoreInit = T)
  # second click layer (red dot and line)
  shiny::observeEvent(click2(), {
    fruit_img$twoclk <- ggplot2::geom_point(ggplot2::aes(click2()[1],
                                                         click2()[2]),
                                            color = "red")
    fruit_img$lineref <- ggplot2::geom_segment(ggplot2::aes(click1()[1],
                                                            click1()[2],
                                                            xend = click2()[1],
                                                            yend = click2()[2]),
                                               color = "red")
  }, ignoreInit = T)
  # cropping layer
  shiny::observeEvent(crop(), {
    fruit_img$crop <- ggplot2::geom_rect(ggplot2::aes(xmin=crop()[1],
                                                      xmax=crop()[2],
                                                      ymin=crop()[3],
                                                      ymax=crop()[4]),
                                         color = "red", alpha = 0.5, fill = "red")
  }, ignoreInit = T)
  # background pixset updates main
  shiny::observeEvent(step1(), {
    fruit_img$main <- imager::colorise(image1(),
                                       RedDrupe(cs_bkg(), input$channel1_bkg,
                                                input$channel2_bkg, input$channel3_bkg, T),
                                       col = "white") %>%
      imager::cimg2magick() %>%
      magick::image_flop() %>%
      magick::image_ggplot()
  }, ignoreInit = T)

  output$image <- shiny::renderPlot(
    {plot(fruit_img$main) +
        fruit_img$oneclk +
        fruit_img$twoclk +
        fruit_img$lineref +
        fruit_img$crop},
    width = 650, height = 433
  )

  roots <- c(home = normalizePath("~/.."))
  shinyFiles::shinyDirChoose(
    input,
    'folderbutton',
    roots = roots,
    filetypes = c("", "jpeg", "JPEG", "jpg", "JPG")
  )
  output$foldertxt <- shiny::renderPrint({
    str(
      folderselected$datapath
    )
  })
  folderselected <- reactiveValues(datapath = getwd())
  observeEvent(input$folderbutton, {
    try(folderselected$datapath <- as.character(shinyFiles::parseDirPath(roots, input$folderbutton)))
  }, ignoreNULL = T)
  observeEvent(folderselected$datapath, {
    try(
      if(dir.exists(folderselected$datapath)){
        setwd(folderselected$datapath)
      }
    )
  }, ignoreNULL = T, ignoreInit = T)
  observeEvent(input$runbutton, {
    bkg <- list(input$col_space_bkg, input$channel1_bkg, input$channel2_bkg, input$channel3_bkg)
    indir <- getwd()
    drp <- ("Drupelet Count" %in% input$variables)
    ber <- ("Size" %in% input$variables)
    col <- ("Color Profile" %in% input$variables)
    rdr <- if("Color-Based Feature" %in% input$variables){
      list(input$col_space, input$channel1, input$channel2, input$channel3, ("Despeckle" %in% input$CFops))
    } else {
      NULL
    }
    if(!is.null(crop())){
      batch_crop <- crop()
    } else {
      batch_crop <- c(0,0,0,0)
    }
    shiny::withProgress(message = "Analyzing Images",{
      RunBatch(indir, input$imgbat, col, drp, ber, rdr, sz_conv(), batch_crop, bkg,
               input$colfeature, preprocess=input$preprocess)
    })
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
}

colorspaces <- c("RGB", "HSB", "Lab")
RGB <- c("Red", "Green", "Blue")
HSB <- c("Hue", "Saturation", "Lightness")
variableList <- c("RDR", "Drupelet Count", "Size")
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
  image1 <- shiny::eventReactive(input$sample_img$datapath, {
    image1 <- bkb_process(input$sample_img$datapath)
    return(image1)})
  cs_cimg <- shiny::reactive({switchspace(img_na(), input$col_space)})
  shiny::observeEvent(input$sample_img, {
    output$image <- shiny::renderPlot(
      {image1() %>%
          plot()},
      width = 900, height = 600
    )
  })

  click <- reactiveVal(0)
  click1 <- reactiveVal(NULL)
  click2 <- reactiveVal(NULL)
  crop <- reactiveVal(NULL)
  sz_conv <- reactiveVal(1)
  line_len <- reactiveVal(NULL)
  output$line_len <- renderText({"Measured Length? (pixels)"})
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

  #This observer plots a distribution of colors for the selected colorspace
  cs <- shiny::reactive({
    list(input$col_space, input$variables)
  })

  shiny::observeEvent(cs(), {
    if("RDR" %in% input$variables){
      labs <- BlackMagick::cs_labs[,colnames(BlackMagick::cs_labs) == input$col_space]
      shiny::updateSliderInput(session, inputId = "channel1", label = labs[1],
                               min = floor(min(imager::R(cs_cimg()), na.rm = T)),
                               max = ceiling(max(imager::R(cs_cimg()), na.rm = T)),
                               value = c(floor(min(imager::R(cs_cimg()), na.rm = T)),
                                         ceiling(max(imager::R(cs_cimg()), na.rm = T))))
      shiny::updateSliderInput(session, inputId = "channel2", label = labs[2],
                               min = floor(min(imager::G(cs_cimg()), na.rm = T)),
                               max = ceiling(max(imager::G(cs_cimg()), na.rm = T)),
                               value = c(floor(min(imager::G(cs_cimg()), na.rm = T)),
                                         ceiling(max(imager::G(cs_cimg()), na.rm = T))))
      shiny::updateSliderInput(session, inputId = "channel3", label = labs[3],
                               min = floor(min(imager::B(cs_cimg()), na.rm = T)),
                               max = ceiling(max(imager::B(cs_cimg()), na.rm = T)),
                               value = c(floor(min(imager::B(cs_cimg()), na.rm = T)),
                                         ceiling(max(imager::B(cs_cimg()), na.rm = T))))
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
  RDR_px <- shiny::reactive({
    if("RDR" %in% input$variables){
      RedDrupe(cs_cimg(), input$channel1, input$channel2,
               input$channel3, input$despeckle)
    }
  })

  # Rendering reactive text output
  BerTxt <- reactive({ paste0("Berry Count: ", BerOut()) })
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
  RdrTxt <- reactive({ if(imager::is.pixset(RDR_px())){
    red_px <- sum(RDR_px())
    black_px <-  sum(!imager::px.na(imager::R(cs_cimg())))
    paste0("\nRDR detected: ", round(100*(red_px/black_px), 2), "%")
  } else {
    paste0("")
  }
  })
  output$txtout <- shiny::renderText({
    paste0(BerTxt(), SzTxt(), DrpTxt(), RdrTxt())
  })

  step1 <- shiny::reactive({
    list(input$img_click, input$clearclick, input$img_crop, input$clearcrop)
  })

  shiny::observeEvent(step1(), {
    if(input$submitcrop < 1){
      image_mask <- image1()
      if(!is.null(click1())){
        image_mask <- imager::draw_circle(image_mask, click1()[1], click1()[2], 5, "red")
      }
      if(!is.null(click2())){
        image_mask <- imager::draw_circle(image_mask, click2()[1], click2()[2], 5, "red") %>%
          imager::implot(lines(c(click1()[1], click2()[1]), c(click1()[2], click2()[2]), col="red",lwd=4))
      }
      if(!is.null(crop()) & input$submitsize > 0){
        image_mask <- imager::draw_rect(image_mask, crop()[1], crop()[3], crop()[2], crop()[4],
                                        color = "red", filled = T, opacity = 0.5)
      }
      output$image <- shiny::renderPlot({
        image_mask %>% plot()
      }, width = 900, height = 600)
      outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    }
  })

  img_step2 <- shiny::eventReactive(input$submitcrop, {
    if(is.numeric(crop())){
      bkb_background(image1(), crop(), F)
    }
  })
  img_na <- shiny::eventReactive(input$submitcrop, {
    if(is.numeric(crop())){
      bkb_background(image1(), crop(), T)
    }
  })
  shiny::observeEvent(img_step2(), {
    output$image <- renderPlot({
      img_step2() %>% plot()
    }, width = 900, height = 600)
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  })


  # Masks for visual quality checking

  drp_px <- shiny::reactive({ DrpPlot(img_na(), DrpOut()) })

  step2 <- shiny::reactive({
    list(input$channel1, input$channel2, input$channel3, input$despeckle,
         input$variables)
  })

  shiny::observeEvent(step2(), {
    image_mask <- img_step2()
    if("Drupelet Count" %in% input$variables){
      image_mask <- imager::colorise(image_mask, drp_px(), col = "green")
    }
    if("RDR" %in% input$variables & imager::is.pixset(RDR_px())){
      image_mask <- imager::colorise(image_mask, RDR_px(), col = "red", alpha = 0.5)
    }
    output$image <- shiny::renderPlot({
      image_mask %>% plot()
    }, width = 900, height = 600)
  }, ignoreInit = T)

  output$image <- shiny::renderPlot(
    {plot(PlaceHolder)},
    width = 900, height = 600
  )
  observeEvent(input$folderbutton, {
    setwd(choose.dir())
    output$foldertxt <- shiny::renderText({getwd()})
  })
  observeEvent(input$runbutton, {
    indir <- getwd()
    drp <- ("Drupelet Count" %in% input$variables)
    ber <- ("Size" %in% input$variables)
    rdr <- if("RDR" %in% input$variables){
      list(input$col_space, input$channel1, input$channel2, input$channel3, input$despeckle)
    } else {
      NULL
    }
    if(!is.null(crop())){
      batch_crop <- crop()
    } else {
      batch_crop <- c(0,0,0,0)
    }
    shiny::withProgress(message = "Analyzing Images",{
      RunBatch(indir, input$imgbat, drp, ber, rdr, sz_conv(), batch_crop)
    })
  })
  output$testing <- renderTable(SzOut())
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
}

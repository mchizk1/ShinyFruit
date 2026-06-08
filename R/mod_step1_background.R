#' Step 1 server logic: upload, size reference, crop, background sliders
#' @noRd
mod_step1_background_server <- function(input, output, session) {
  output$fileUploaded <- shiny::renderText({
    shiny::req(input$sample_img)
    input$sample_img$name
  })

  image1 <- shiny::eventReactive(
    list(input$sample_img$datapath, input$preprocess),
    {
      shiny::req(input$sample_img$datapath)
      tryCatch(
        bkb_process(input$sample_img$datapath, input$preprocess),
        error = function(e) {
          shiny::showNotification(
            paste0("Could not read image: ", basename(input$sample_img$name)),
            type = "error"
          )
          NULL
        }
      )
    },
    ignoreNULL = TRUE
  )

  cs_bkg <- shiny::reactive({
    shiny::req(image1())
    switchspace(image1(), input$col_space_bkg)
  })

  click <- shiny::reactiveVal(0)
  click1 <- shiny::reactiveVal(NULL)
  click2 <- shiny::reactiveVal(NULL)
  crop <- shiny::reactiveVal(c(0, 0, 0, 0))
  sz_conv <- shiny::reactiveVal(1)
  line_len <- shiny::reactiveVal(NULL)

  output$line_len <- shiny::renderText({
    if (is.null(line_len())) {
      "Click any two points to measure"
    } else {
      paste0(round(line_len(), 1), " pixels")
    }
  })

  shiny::observeEvent(input$img_click, {
    shiny::req(image1())
    click(shiny::isolate(click() + 1))
    pt <- c(
      round(input$img_click$x),
      round(input$img_click$y)
    )
    if (click() == 1) {
      click1(pt)
    } else if (click() == 2) {
      click2(pt)
      preview <- scale_cimg_for_preview(image1())
      plot_scale <- dim(image1())[1] / dim(preview)[1]
      line_len(pythag(click1(), click2()) * plot_scale)
    }
  })

  shiny::observeEvent(input$img_crop, {
    if (input$submitcrop < 1) {
      shiny::req(image1(), input$img_crop)
      crop(normalize_plot_rect(
        input$img_crop$xmin,
        input$img_crop$xmax,
        input$img_crop$ymin,
        input$img_crop$ymax
      ))
    }
  })

  shiny::observeEvent(input$submitsize, {
    if (!is.null(click2()) && nzchar(input$known_len)) {
      known <- suppressWarnings(as.numeric(input$known_len))
      if (!is.na(known) && !is.null(line_len()) && line_len() > 0) {
        sz_conv(known / line_len())
      }
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

  shiny::observeEvent(list(input$col_space_bkg, input$sample_img), {
    shiny::req(input$sample_img, image1())
    update_channel_sliders(session, cs_bkg(), input$col_space_bkg, prefix = "_bkg")
  }, ignoreInit = TRUE)

  step1 <- shiny::reactive({
    list(input$channel1_bkg, input$channel2_bkg, input$channel3_bkg)
  })

  img_step2 <- shiny::eventReactive(input$submitcrop, {
    shiny::req(image1())
    crop_vals <- crop()
    if (!is.numeric(crop_vals)) {
      crop_vals <- c(0, 0, 0, 0)
    }
    bkb_background(
      image1(), crop_vals, FALSE, input$col_space_bkg,
      input$channel1_bkg, input$channel2_bkg, input$channel3_bkg,
      plot_dims = crop_plot_dims(image1())
    )
  })

  img_na <- shiny::eventReactive(input$submitcrop, {
    shiny::req(image1())
    crop_vals <- crop()
    if (!is.numeric(crop_vals)) {
      crop_vals <- c(0, 0, 0, 0)
    }
    bkb_background(
      image1(), crop_vals, TRUE, input$col_space_bkg,
      input$channel1_bkg, input$channel2_bkg, input$channel3_bkg,
      plot_dims = crop_plot_dims(image1())
    )
  })

  list(
    image1 = image1,
    cs_bkg = cs_bkg,
    click1 = click1,
    click2 = click2,
    crop = crop,
    sz_conv = sz_conv,
    step1 = step1,
    img_step2 = img_step2,
    img_na = img_na
  )
}

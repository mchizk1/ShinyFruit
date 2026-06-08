#' Unified image plot server logic
#' @noRd
mod_image_plot_server <- function(input, output, session, step1_state, step2_state) {
  image1 <- step1_state$image1
  click1 <- step1_state$click1
  click2 <- step1_state$click2
  crop <- step1_state$crop
  step1 <- step1_state$step1
  img_step2 <- step2_state$img_step2
  img_na <- step2_state$img_na
  RDR_px <- step2_state$RDR_px
  DrpSegmentation <- step2_state$DrpSegmentation
  drp_labels_scaled <- step2_state$drp_labels_scaled

  fruit_img <- shiny::reactiveValues(
    main = magick::image_ggplot(
      magick::image_read(system.file("blackberry.png", package = "ShinyFruit"))
    ),
    oneclk = ggplot2::geom_blank(),
    twoclk = ggplot2::geom_blank(),
    lineref = ggplot2::geom_blank(),
    crop_layer = ggplot2::geom_blank()
  )

  shiny::observeEvent(input$sample_img, {
    shiny::req(image1())
    fruit_img$main <- cimg_to_ggplot(scale_cimg_for_preview(image1()))
  })

  shiny::observeEvent(click1(), {
    shiny::req(click1(), image1())
    fruit_img$oneclk <- ggplot2::geom_point(
      ggplot2::aes(click1()[1], click1()[2]),
      color = "red"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(click2(), {
    shiny::req(click1(), click2(), image1())
    fruit_img$twoclk <- ggplot2::geom_point(
      ggplot2::aes(click2()[1], click2()[2]),
      color = "red"
    )
    fruit_img$lineref <- ggplot2::geom_segment(
      ggplot2::aes(
        click1()[1], click1()[2],
        xend = click2()[1], yend = click2()[2]
      ),
      color = "red"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(crop(), {
    shiny::req(crop(), image1())
    if (!crop_rect_is_valid(crop())) {
      fruit_img$crop_layer <- ggplot2::geom_blank()
      return()
    }
    fruit_img$crop_layer <- ggplot2::geom_rect(
      ggplot2::aes(
        xmin = crop()[1], xmax = crop()[2],
        ymin = crop()[3], ymax = crop()[4]
      ),
      color = "red", alpha = 0.5, fill = "red"
    )
  }, ignoreInit = TRUE)

  step1_inputs <- shiny::debounce(step1, millis = DEBOUNCE_MS)

  shiny::observeEvent(step1_inputs(), {
    shiny::req(image1())
    fruit_img$main <- background_preview(
      image1(), input$col_space_bkg,
      input$channel1_bkg, input$channel2_bkg, input$channel3_bkg
    ) %>% cimg_to_ggplot()
  }, ignoreInit = TRUE)

  step2_preview <- shiny::reactive({
    step2_state$step2()
    shiny::req(input$submitcrop > 0, img_step2())
    image_mask <- scale_cimg_for_preview(img_step2())

    if ("Drupelet Count" %in% input$variables) {
      seg <- DrpSegmentation()
      if (!is.null(seg$label_im) && nrow(seg$drupelets) > 0) {
        lab_preview <- scale_label_im_for_preview(seg$label_im, img_na(), image_mask)
        ids <- if (length(seg$label_ids) > 0) {
          seg$label_ids
        } else {
          unique(seg$drupelets$Drupelet)
        }
        image_mask <- overlay_drupelet_mask(image_mask, lab_preview, ids)
      }
    }

    if ("Color-Based Feature" %in% input$variables &&
        imager::is.pixset(RDR_px()) &&
        "Show Mask" %in% input$CFops) {
      image_mask <- overlay_feature_mask(
        image_mask, RDR_px(), input$colfeature, alpha = 0.55
      )
    }

    list(
      image = image_mask,
      labels = drp_labels_scaled()
    )
  })

  output$image <- shiny::renderPlot({
    tryCatch({
      if (input$submitcrop > 0) {
        preview <- step2_preview()
        shiny::req(preview)
        p <- preview$image %>% cimg_to_ggplot()
        if (!is.null(preview$labels) && nrow(preview$labels) > 0) {
          p <- p +
            ggplot2::geom_point(
              data = preview$labels,
              ggplot2::aes(x = X, y = Y, size = MarkerSize),
              shape = 1, color = "#00E676", stroke = 1.1
            ) +
            ggplot2::scale_size_identity() +
            ggplot2::geom_point(
              data = preview$labels,
              ggplot2::aes(x = X, y = Y),
              shape = 16, color = "#FFD700", size = 1.6
            ) +
            ggplot2::geom_label(
              data = preview$labels,
              ggplot2::aes(x = X, y = Y, label = Drupelet),
              color = "white", fill = rgb(0, 0, 0, 0.6),
              size = 2.4, fontface = "bold", label.size = 0,
              vjust = -1.1
            )
        }
        p
      } else {
        plot(fruit_img$main) +
          fruit_img$oneclk +
          fruit_img$twoclk +
          fruit_img$lineref +
          fruit_img$crop_layer
      }
    }, error = function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.5,
          label = paste("Preview error:", conditionMessage(e)),
          size = 4, color = "white"
        ) +
        ggplot2::theme_void()
    })
  }, width = PLOT_WIDTH, height = PLOT_HEIGHT)
}

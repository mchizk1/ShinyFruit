#' Build drupelet detection settings from Shiny inputs
#' @noRd
drp_settings_from_input <- function(input) {
  list(
    min_size = safe_positive_int(input$drp_min_size, DRUPELET_DEFAULTS$min_size),
    blur_sigma = safe_numeric(input$drp_blur, DRUPELET_DEFAULTS$blur_sigma),
    sensitivity = safe_numeric(input$drp_sensitivity, DRUPELET_DEFAULTS$sensitivity)
  )
}

#' Step 2 server logic: trait selection, stats, and preview inputs
#' @noRd
mod_step2_analysis_server <- function(input, output, session, step1_state) {
  img_na <- step1_state$img_na
  img_step2 <- step1_state$img_step2
  sz_conv <- step1_state$sz_conv
  cf_sliders_seeded <- shiny::reactiveVal(FALSE)

  cs_cimg <- shiny::reactive({
    shiny::req(img_na())
    switchspace(img_na(), input$col_space)
  })

  seed_feature_sliders <- function() {
    shiny::req(cs_cimg())
    update_channel_sliders(session, cs_cimg(), input$col_space)
    cf_sliders_seeded(TRUE)
  }

  shiny::observeEvent(img_na(), {
    cf_sliders_seeded(FALSE)
    if ("Color-Based Feature" %in% isolate(input$variables)) {
      seed_feature_sliders()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$col_space, {
    if ("Color-Based Feature" %in% isolate(input$variables)) {
      seed_feature_sliders()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$variables, {
    if ("Color-Based Feature" %in% input$variables && !cf_sliders_seeded()) {
      seed_feature_sliders()
    }
  }, ignoreInit = TRUE)

  analysis_inputs <- shiny::debounce(
    shiny::reactive({
      list(
        channel1 = input$channel1,
        channel2 = input$channel2,
        channel3 = input$channel3,
        drp_min_size = input$drp_min_size,
        drp_blur = input$drp_blur,
        drp_sensitivity = input$drp_sensitivity,
        cf_ops = input$CFops,
        colfeature = input$colfeature,
        col_space = input$col_space
      )
    }),
    millis = ANALYSIS_DEBOUNCE_MS
  )

  safe_ber_summary <- function(img, stats = TRUE) {
    tryCatch(
      BerSummary(img, stats = stats),
      error = function(e) {
        shiny::showNotification(
          paste("Size analysis failed:", conditionMessage(e)),
          type = "error",
          duration = 5
        )
        NULL
      }
    )
  }

  BerOut <- shiny::eventReactive(img_na(), {
    shiny::req(img_na())
    safe_ber_summary(img_na(), stats = FALSE)
  })

  DrpSegmentation <- shiny::reactive({
    input$variables
    if (!"Drupelet Count" %in% input$variables) {
      return(list(drupelets = empty_drp_df(), label_im = NULL, label_ids = integer()))
    }
    analysis_inputs()
    shiny::req(img_na())
    cfg <- drp_settings_from_input(input)
    tryCatch(
      DrpSegment(
        img_na(),
        min_size = cfg$min_size,
        blur_sigma = cfg$blur_sigma,
        sensitivity = cfg$sensitivity
      ),
      error = function(e) {
        shiny::showNotification(
          paste("Drupelet analysis failed:", conditionMessage(e)),
          type = "error",
          duration = 5
        )
        list(drupelets = empty_drp_df(), label_im = NULL, label_ids = integer())
      }
    )
  })

  DrpOut <- shiny::reactive({
    DrpSegmentation()$drupelets
  })

  drp_labels_scaled <- shiny::reactive({
    seg <- DrpSegmentation()
    if (nrow(seg$drupelets) == 0) {
      return(NULL)
    }
    preview <- scale_cimg_for_preview(img_na())
    scale_coords_for_preview(seg$drupelets, img_na(), preview)
  })

  SzOut <- shiny::reactive({
    input$variables
    if ("Size" %in% input$variables) {
      shiny::req(img_na())
      safe_ber_summary(img_na(), stats = TRUE)
    }
  })

  ColOut <- shiny::reactive({
    input$variables
    if ("Color Profile" %in% input$variables) {
      shiny::req(img_na())
      ColProfile(img_na(), input$col_space)
    }
  })

  RDR_px <- shiny::reactive({
    analysis_inputs()
    if ("Color-Based Feature" %in% input$variables) {
      shiny::req(cs_cimg())
      RedDrupe(
        cs_cimg(), input$channel1, input$channel2, input$channel3,
        ("Despeckle" %in% input$CFops), TRUE
      )
    }
  })

  step2 <- shiny::reactive({
    input$variables
    analysis_inputs()
  })

  cf_na <- shiny::reactive({
    analysis_inputs()
    if ("Color-Based Feature" %in% input$variables &&
        imager::is.pixset(RDR_px())) {
      tmp_step2 <- img_step2()
      tmp_step2[imager::as.pixset(1 - RDR_px())] <- NA
      return(tmp_step2)
    }
    NULL
  })

  CfOut <- shiny::reactive({
    input$variables
    if ("Color-Based Feature" %in% input$variables && !is.null(cf_na())) {
      ColProfile(cf_na(), input$col_space)
    }
  })

  results_text <- shiny::reactive({
    input$variables
    analysis_inputs()

    ber_txt <- if (is.null(BerOut())) "" else paste0("Berry count: ", BerOut())

    sz_txt <- if (is.data.frame(SzOut()) && nrow(SzOut()) > 0) {
      paste0(
        "\nMean length: ", round(mean(SzOut()$L * sz_conv())), " ", input$units,
        "\nMean width: ", round(mean(SzOut()$W * sz_conv())), " ", input$units,
        "\nMean area: ", round(mean(SzOut()$Size * (sz_conv()^2))), " ", input$units, " squared"
      )
    } else if ("Size" %in% input$variables) {
      "\nSize analysis could not be computed for this image."
    } else {
      ""
    }

    col_txt <- if (is.list(ColOut())) {
      paste0(
        "\nMid RGB: (", round(ColOut()$red),
        ", ", round(ColOut()$green), ", ", round(ColOut()$blue), ")",
        "\nRHS (dark / mid / light): ", ColOut()$dark_color,
        " / ", ColOut()$mid_color, " / ", ColOut()$light_color
      )
    } else {
      ""
    }

    drp_txt <- if (is.data.frame(DrpOut()) && nrow(DrpOut()) > 0) {
      mean_drp <- DrpMeanPerBerry(DrpOut())
      n_berries <- length(unique(DrpOut()$Berry))
      paste0(
        "\nDrupelets detected: ", nrow(DrpOut()),
        "\nBerries with drupelets: ", n_berries,
        "\nMean drupelets/berry: ", round(mean_drp, 2)
      )
    } else if ("Drupelet Count" %in% input$variables) {
      "\nNo drupelets detected — try lowering edge threshold or minimum size."
    } else {
      ""
    }

    rdr_txt <- if (imager::is.pixset(RDR_px())) {
      red_px <- sum(RDR_px())
      black_px <- sum(fruit_pixset(cs_cimg()))
      pct <- if (black_px > 0) round(100 * (red_px / black_px), 2) else 0
      if ("Show Mean RGB" %in% input$CFops && is.list(CfOut())) {
        paste0(
          "\nFeature coverage: ", pct, "%",
          "\nFeature RGB: (", round(CfOut()$red),
          ", ", round(CfOut()$green), ", ", round(CfOut()$blue), ")",
          "\nFeature RHS: ", CfOut()$dark_color,
          " / ", CfOut()$mid_color, " / ", CfOut()$light_color
        )
      } else {
        paste0("\nFeature coverage: ", pct, "%")
      }
    } else {
      ""
    }

    paste0(ber_txt, sz_txt, col_txt, drp_txt, rdr_txt)
  })

  output$txtout <- shiny::renderText({
    results_text()
  })

  list(
    cs_cimg = cs_cimg,
    img_step2 = img_step2,
    img_na = img_na,
    RDR_px = RDR_px,
    DrpSegmentation = DrpSegmentation,
    drp_labels_scaled = drp_labels_scaled,
    step2 = step2,
    analysis_inputs = analysis_inputs,
    drp_settings = shiny::reactive(drp_settings_from_input(input))
  )
}

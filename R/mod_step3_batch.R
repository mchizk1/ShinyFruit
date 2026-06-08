#' Step 3 server logic: folder selection and batch processing
#' @noRd
mod_step3_batch_server <- function(input, output, session, step1_state, step2_state) {
  crop <- step1_state$crop
  image1 <- step1_state$image1
  sz_conv <- step1_state$sz_conv

  if (.Platform$OS.type == "windows") {
    roots <- c(home = normalizePath("~/.."))
  } else {
    roots <- c(home = normalizePath("~"))
  }

  shinyFiles::shinyDirChoose(
    input,
    "folderbutton",
    roots = roots,
    filetypes = c("", "jpeg", "JPEG", "jpg", "JPG", "png", "PNG")
  )

  folderselected <- shiny::reactive({
    if (length(input$folderbutton) > 1) {
      as.character(shinyFiles::parseDirPath(roots, input$folderbutton))
    } else {
      getwd()
    }
  })

  output$foldertxt <- shiny::renderText({
    indir <- folderselected()
    n_imgs <- length(list.files(
      indir,
      pattern = "\\.(jpe?g|png|JPE?G|PNG)$",
      ignore.case = TRUE
    ))
    paste0(n_imgs, " image(s) in\n", indir)
  })

  shiny::observeEvent(input$runbutton, {
    indir <- folderselected()
    n_imgs <- length(list.files(
      indir,
      pattern = "\\.(jpe?g|png|JPE?G|PNG)$",
      ignore.case = TRUE
    ))

    if (n_imgs == 0) {
      shiny::showNotification(
        "No JPEG or PNG files found in the selected folder.",
        type = "error"
      )
      return()
    }

    bkg <- list(
      input$col_space_bkg, input$channel1_bkg,
      input$channel2_bkg, input$channel3_bkg
    )
    drp <- ("Drupelet Count" %in% input$variables)
    ber <- ("Size" %in% input$variables)
    col <- ("Color Profile" %in% input$variables)
    rdr <- if ("Color-Based Feature" %in% input$variables) {
      list(
        input$col_space, input$channel1, input$channel2, input$channel3,
        ("Despeckle" %in% input$CFops)
      )
    } else {
      NULL
    }

    batch_crop <- if (!is.null(crop()) && is.numeric(crop()) && crop_rect_is_valid(crop())) {
      scale_crop_to_image(crop(), image1())
    } else {
      c(0, 0, 0, 0)
    }

    result <- tryCatch(
      {
        shiny::withProgress(
          message = "Analyzing Images",
          value = 0,
          {
            RunBatch(
              indir, input$imgbat, col, drp, ber, rdr, sz_conv(),
              batch_crop, bkg, input$colfeature, preprocess = input$preprocess,
              drp_settings = if (drp) step2_state$drp_settings() else NULL
            )
          }
        )
        outdir <- file.path(indir, "ShinyFruit_Out")
        shiny::showNotification(
          paste0("Batch complete. Output saved to ", outdir),
          type = "message",
          duration = 8
        )
        TRUE
      },
      error = function(e) {
        shiny::showNotification(
          paste0("Batch failed: ", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        FALSE
      }
    )
    invisible(result)
  })

  list(folderselected = folderselected)
}

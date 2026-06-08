test_that("bkb_process reads and resizes an image", {
  skip_if_not_installed("magick")
  skip_if_not_installed("imager")
  fixture <- testthat::test_path("..", "fixtures", "blackberry.png")
  skip_if_not(file.exists(fixture), "fixture image missing")
  result <- bkb_process(fixture, preprocess = FALSE)
  expect_s3_class(result, "cimg")
  expect_true(max(dim(result)[1:2]) <= 1500)
})

test_that("BerSummary counts labeled regions", {
  skip_if_not_installed("imager")
  img <- imager::imfill(20, 20, 1, val = c(0.8, 0.1, 0.1))
  img[1:10, 1:10, ] <- NA
  count <- BerSummary(img, stats = FALSE)
  expect_true(count >= 1)
})

test_that("DrpPlot handles empty coordinates", {
  skip_if_not_installed("imager")
  img <- imager::imfill(10, 10, 1, val = c(0.5, 0.5, 0.5))
  px <- DrpPlot(img, data.frame(X = integer(), Y = integer()))
  expect_true(imager::is.pixset(px))
  expect_equal(sum(px), 0)
})

test_that("ColProfile returns RHS color names", {
  skip_if_not_installed("imager")
  data("rgb2rhs", package = "ShinyFruit", envir = environment())
  img <- imager::imfill(10, 10, 1, val = c(0.6, 0.2, 0.1))
  profile <- ColProfile(img, "RGB")
  expect_type(profile$red, "double")
  expect_true(profile$mid_color %in% rgb2rhs$english)
})

test_that("RunBatch filters non-image files", {
  skip_if_not_installed("magick")
  skip_if_not_installed("imager")
  tmp <- tempfile("sf_batch_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  fixture <- testthat::test_path("..", "fixtures", "blackberry.png")
  skip_if_not(file.exists(fixture), "fixture image missing")
  file.copy(fixture, file.path(tmp, "sample.jpg"))
  writeLines("not an image", file.path(tmp, "notes.txt"))
  bkg <- list("RGB", c(0, 1), c(0, 1), c(0, 1))
  expect_error(
    RunBatch(tmp, include_img = FALSE, col = FALSE, drp = FALSE, ber = FALSE,
             rdr = NULL, convert = 1, crop = c(0, 0, 0, 0), bkg = bkg,
             preprocess = FALSE),
    NA
  )
  out_files <- list.files(file.path(tmp, "ShinyFruit_Out"), pattern = "\\.csv$")
  expect_length(out_files, 1)
})

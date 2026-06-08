test_that("RedDrupe invert mode selects in-range pixels", {
  skip_if_not_installed("imager")
  img <- imager::imfill(5, 5, 1, val = c(0.5, 0.5, 0.5))
  px <- RedDrupe(img, c(0.4, 0.6), c(0.4, 0.6), c(0.4, 0.6), despeckle = FALSE, invert = TRUE)
  expect_true(imager::is.pixset(px))
  expect_equal(sum(px), 25)
})

test_that("RedDrupe exclude mode removes in-range pixels", {
  skip_if_not_installed("imager")
  img <- imager::imfill(5, 5, 1, val = c(0.5, 0.5, 0.5))
  px <- RedDrupe(img, c(0.4, 0.6), c(0.4, 0.6), c(0.4, 0.6), despeckle = FALSE, invert = FALSE)
  expect_equal(sum(px), 0)
})

test_that("overlay_feature_mask tints preview image", {
  skip_if_not_installed("imager")
  img <- imager::imfill(40, 40, 1, val = c(0.2, 0.2, 0.15))
  px <- RedDrupe(img, c(0, 0.3), c(0, 1), c(0, 1), despeckle = FALSE, invert = TRUE)
  preview <- scale_cimg_for_preview(img)
  out <- overlay_feature_mask(preview, px, "red", alpha = 0.55)
  expect_equal(dim(out), dim(preview))
  expect_true(any(imager::R(out) > imager::R(preview)))
})

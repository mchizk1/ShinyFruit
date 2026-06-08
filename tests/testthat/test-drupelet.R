test_that("DrpMeanPerBerry averages per-berry counts", {
  drp_df <- data.frame(
    Berry = c(1, 1, 1, 2, 2),
    X = c(1, 2, 3, 4, 5),
    Y = c(1, 2, 3, 4, 5),
    Drupelet = c(1, 2, 3, 4, 5)
  )
  expect_equal(DrpMeanPerBerry(drp_df), 2.5)
})

test_that("DrpSummary returns empty frame for blank image", {
  skip_if_not_installed("imager")
  img <- imager::imfill(20, 20, 1, val = c(0.1, 0.8, 0.1))
  img[, , ] <- NA
  result <- DrpSummary(img)
  expect_equal(nrow(result), 0)
})

test_that("DrpSegment handles NULL slider defaults", {
  skip_if_not_installed("imager")
  img <- imager::imfill(40, 40, 1, val = c(0.5, 0.2, 0.15))
  img[1:5, , ] <- NA
  result <- DrpSegment(img, min_size = NULL, blur_sigma = NULL, sensitivity = NULL)
  expect_true(is.list(result))
  expect_true("drupelets" %in% names(result))
})

test_that("fruit_pixset uses single-channel mask aligned with grayscale", {
  skip_if_not_installed("imager")
  img <- imager::imfill(20, 20, 1, val = c(0.5, 0.2, 0.15))
  img[1:5, , ] <- NA
  px <- fruit_pixset(img)
  gray <- imager::grayscale(img)
  expect_equal(dim(px)[4], 1L)
  expect_equal(dim(px)[1:2], dim(gray)[1:2])
})

test_that("DrpSegment works on NA-masked fruit images", {
  skip_if_not_installed("imager")
  img <- imager::imfill(50, 50, 1, val = c(0.5, 0.2, 0.15))
  img[1:8, , ] <- NA
  img[, 1:8, ] <- NA
  arr <- as.array(img)
  noise <- matrix(rnorm(42 * 42, sd = 0.04), nrow = 42, ncol = 42)
  arr[9:50, 9:50, 1, 1] <- arr[9:50, 9:50, 1, 1] + noise
  img <- imager::as.cimg(arr)
  img[1:8, , ] <- NA
  img[, 1:8, ] <- NA
  seg <- DrpSegment(img, min_size = 3, blur_sigma = 1, sensitivity = 0.55)
  expect_true(length(seg$label_ids) > 0 || nrow(seg$drupelets) > 0)
})

test_that("map_labeled_rows combines rows without purrr", {
  stat_fn <- function(im, id) data.frame(value = id * 2)
  result <- map_labeled_rows(matrix(1), c(2, 5), stat_fn, "Label")
  expect_equal(result$value, c(4, 10))
  expect_equal(result$Label, c(2L, 5L))
})

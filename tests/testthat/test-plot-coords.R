test_that("plot and cimg coordinates round-trip with flopped display", {
  width <- 100
  height <- 80
  plot_pt <- c(25, 30)
  cimg_pt <- plot_xy_to_cimg(plot_pt[1], plot_pt[2], width, height)
  back <- cimg_xy_to_plot(cimg_pt$cimg_x, cimg_pt$cimg_y, width, height)
  expect_equal(back$x, plot_pt[1], tolerance = 1)
  expect_equal(back$y, plot_pt[2], tolerance = 1)
})

test_that("plot_rect_to_cimg and cimg_rect_to_plot are inverses", {
  width <- 120
  height <- 90
  plot_rect <- c(10, 40, 15, 55)
  cimg_rect <- plot_rect_to_cimg(
    plot_rect[1], plot_rect[2], plot_rect[3], plot_rect[4],
    width, height
  )
  expect_equal(cimg_rect_to_plot(cimg_rect, width, height), plot_rect)
})

test_that("apply_crop_rect marks the brushed plot columns", {
  skip_if_not_installed("imager")
  width <- 100L
  height <- 80L
  img <- imager::imfill(width, height, 1, val = c(0.8, 0.2, 0.2))
  plot_crop <- c(10L, 30L, 20L, 50L)
  out <- apply_crop_rect(img, plot_crop)
  marked <- as.matrix(imager::B(out)[, , 1, 1]) > 0.9
  expect_gt(sum(marked[10:30, , drop = FALSE]), 0)
  expect_equal(sum(marked[31:100, , drop = FALSE]), 0)
})

test_that("plot_coords_to_preview matches full-size plot mapping", {
  skip_if_not_installed("imager")
  source <- imager::imfill(100, 80, 1, val = 1)
  expected <- cimg_xy_to_plot(25, 30, 100, 80)
  pt <- plot_coords_to_preview(25, 30, source, source)
  expect_equal(pt[1], expected$x, tolerance = 1)
  expect_equal(pt[2], expected$y, tolerance = 1)
})

test_that("scale_rect_between_cimgs preserves relative size", {
  rect <- c(10L, 50L, 20L, 80L)
  scaled <- scale_rect_between_cimgs(rect, from_dims = c(100, 100), to_dims = c(200, 200))
  expect_equal(scaled, c(20L, 100L, 40L, 160L))
})

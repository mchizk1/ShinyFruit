test_that("pythag computes hypotenuse", {
  expect_equal(pythag(c(0, 0), c(3, 4)), 5)
  expect_equal(pythag(c(1, 1), c(4, 5)), 5)
})

test_that("switchspace converts colorspaces", {
  skip_if_not_installed("imager")
  img <- imager::imfill(10, 10, 1, val = c(0.5, 0.2, 0.8))
  hsb <- switchspace(img, "HSB")
  lab <- switchspace(img, "Lab")
  rgb <- switchspace(img, "RGB")
  expect_equal(dim(hsb), dim(img))
  expect_equal(dim(lab), dim(img))
  expect_equal(imager::R(rgb), imager::R(img))
})

test_that("new_dims caps longest side at 1500", {
  expect_equal(new_dims(c(3000, 1500)), "1500x750")
  expect_equal(new_dims(c(800, 2000)), "600x1500")
  expect_equal(new_dims(c(1000, 1000)), "1500x1500")
})

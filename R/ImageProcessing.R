# Sets all background pixels to NA
bkb_process <- function(img, preprocess){
  img_path <- if (length(img) > 1) img[1] else img
  img_og <- magick::image_read(img_path)
  if(preprocess){
    img_new <- magick::image_resize(img_og, new_dims(magick::image_info(img_og)[2:3])) %>%
      magick::image_normalize() %>%
      magick::image_contrast(sharpen = 10) %>%
      magick::image_enhance() %>%
      imager::magick2cimg()
  } else {
    img_new <- magick::image_resize(img_og, new_dims(magick::image_info(img_og)[2:3])) %>%
      imager::magick2cimg()
  }
  return(img_new)
}

new_dims <- function(old_dims, max_dim = ANALYSIS_MAX_DIM){
  if(old_dims[1] >= old_dims[2]){
    dims <- c(max_dim, round((max_dim * old_dims[2]) / old_dims[1]))
  } else {
    dims <- c(round((max_dim * old_dims[1]) / old_dims[2]), max_dim)
  }
  return(paste0(dims[1], "x", dims[2]))
}

bkb_background <- function(img, crop, setNA, cs="RGB", c1, c2, c3, plot_dims = NULL){
  px <- RedDrupe(switchspace(img, cs), c1, c2, c3, T)
  img_out <- imager::colorise(img, px, c(0, 0, 1))
  img_out <- apply_crop_rect(img_out, crop, plot_dims = plot_dims)
  if(setNA == T){
    img_out <- imager::colorise(img_out,
                                (imager::R(img_out==0)&imager::G(img_out==0)&imager::B(img_out==1)),
                                NA)
  } else {
    img_out <- imager::colorise(img_out,
                                (imager::R(img_out==0)&imager::G(img_out==0)&imager::B(img_out==1)),
                                "white")
  }
  gc()
  return(img_out)
}

# Computes berry statistics
BerStat <- function(ber_cimg, ber_index){
  Size <- sum(ber_cimg == ber_index)/3
  bst_coord <- imager::get.locations(ber_cimg, function(x) x == ber_index)
  L = max(bst_coord$y) - min(bst_coord$y)
  W = max(bst_coord$x) - min(bst_coord$x)
  bst_df <- data.frame(Size = Size, L = L, W = W)
  return(bst_df)
}

# Computes coordinates of all drupelets identified
DrpStat <- function(ber_cimg, ber_index){
  dst_coord <- imager::get.locations(ber_cimg, function(x) x == ber_index)
  if (nrow(dst_coord) == 0) {
    return(data.frame(X = NA_integer_, Y = NA_integer_, Pixels = NA_integer_))
  }
  n_px <- sum(as.array(ber_cimg)[, , 1, 1] == ber_index, na.rm = TRUE)
  data.frame(
    X = as.integer(round(mean(dst_coord$x))),
    Y = as.integer(round(mean(dst_coord$y))),
    Pixels = as.integer(n_px)
  )
}

#' Read a single label value from a labeled cimg
#' @noRd
label_at <- function(lab_im, x, y) {
  if (length(x) == 0 || length(y) == 0 || is.na(x) || is.na(y)) {
    return(0L)
  }
  x <- as.integer(x)
  y <- as.integer(y)
  dims <- dim(lab_im)
  if (x < 1L || y < 1L || x > dims[1] || y > dims[2]) {
    return(0L)
  }
  arr <- as.array(lab_im)
  if (x > nrow(arr) || y > ncol(arr)) {
    return(0L)
  }
  val <- arr[x, y, 1, 1]
  if (length(val) == 0 || is.na(val)) {
    return(0L)
  }
  as.integer(val)
}

#' Keep labels that contain at least min_pixels
#' @noRd
filter_label_ids <- function(lab_im, label_ids, min_pixels) {
  min_pixels <- safe_positive_int(min_pixels, 1L)
  if (length(label_ids) == 0) {
    return(integer())
  }
  label_ids <- as.integer(label_ids)
  label_ids <- label_ids[!is.na(label_ids) & label_ids > 0L]
  if (length(label_ids) == 0) {
    return(integer())
  }
  label_ids[vapply(
    label_ids,
    function(id) {
      n <- sum(as.array(lab_im == id), na.rm = TRUE)
      isTRUE(n >= min_pixels)
    },
    logical(1)
  )]
}

#' Build pixset covering all drupelet label regions
#' @noRd
DrpRegionMask <- function(drp_lab, label_ids) {
  if (length(label_ids) == 0 || is.null(drp_lab)) {
    return(NULL)
  }
  lab_arr <- round(as.array(drp_lab)[, , 1, 1])
  mask <- lab_arr %in% as.integer(label_ids)
  if (!any(mask)) {
    return(NULL)
  }
  imager::as.pixset(array(mask, dim = c(dim(drp_lab)[1], dim(drp_lab)[2], 1, 1)))
}

# Runs berry stats over each berry in RGB colorspace
BerSummary <- function(background_cimg, stats=T){
  ber_lab <- fruit_pixset(background_cimg) %>%
    imager::mclosing_square(2) %>%
    imager::label()
  ber_lst <- unique(ber_lab)[-1]
  if(stats){
    ber_df <- map_labeled_rows(ber_lab, ber_lst, BerStat, "Berry")
  } else {
    ber_df <- length(ber_lst)
  }
  return(ber_df)
}

#' Segment drupelets from texture boundaries (color-independent)
#' @noRd
DrpSegment <- function(
    background_cimg,
    min_size = DRUPELET_DEFAULTS$min_size,
    blur_sigma = DRUPELET_DEFAULTS$blur_sigma,
    sensitivity = DRUPELET_DEFAULTS$sensitivity) {
  min_size <- safe_positive_int(min_size, DRUPELET_DEFAULTS$min_size)
  blur_sigma <- safe_numeric(blur_sigma, DRUPELET_DEFAULTS$blur_sigma)
  sensitivity <- safe_numeric(sensitivity, DRUPELET_DEFAULTS$sensitivity)

  mask <- fruit_pixset(background_cimg)
  if (sum(mask) == 0) {
    return(list(drupelets = empty_drp_df(), label_im = NULL, label_ids = integer()))
  }

  ber_lab <- mask %>%
    imager::mclosing_square(2) %>%
    imager::label()

  gray <- imager::grayscale(background_cimg)
  gray_fill <- gray_na_filled(gray)
  smoothed <- imager::isoblur(gray_fill, blur_sigma)

  tex_arr <- abs(as.array(gray) - as.array(smoothed))
  tex_arr[is.na(tex_arr)] <- 0
  grad <- imager::imgradient(gray_fill)
  grad_arr <- sqrt(as.array(grad$x)^2 + as.array(grad$y)^2)
  edge_arr <- tex_arr + grad_arr
  edge <- imager::as.cimg(edge_arr)

  texture_vals <- pixset_values(edge, mask)
  texture_vals <- texture_vals[!is.na(texture_vals) & texture_vals > 0]
  if (length(texture_vals) == 0) {
    return(list(drupelets = empty_drp_df(), label_im = NULL, label_ids = integer()))
  }

  thresh <- stats::quantile(texture_vals, sensitivity, na.rm = TRUE)
  boundaries <- imager::as.pixset(as.array(edge) >= thresh) & mask
  interiors <- (!boundaries) & mask
  interiors <- imager::clean(interiors, min_size)

  drp_lab <- imager::label(interiors)
  label_vec <- filter_label_ids(drp_lab, label_ids_from_cimg(drp_lab), min_size)
  if (length(label_vec) == 0) {
    return(list(drupelets = empty_drp_df(), label_im = drp_lab, label_ids = integer()))
  }

  drp_df <- map_labeled_rows(drp_lab, label_vec, DrpStat, "Drupelet")
  if (nrow(drp_df) == 0) {
    return(list(drupelets = empty_drp_df(), label_im = drp_lab, label_ids = label_vec))
  }

  drp_df <- drp_df[!is.na(drp_df$X) & !is.na(drp_df$Y), , drop = FALSE]
  if (nrow(drp_df) == 0) {
    return(list(drupelets = empty_drp_df(), label_im = drp_lab, label_ids = label_vec))
  }

  drp_df$Berry <- vapply(
    seq_len(nrow(drp_df)),
    function(i) label_at(ber_lab, drp_df$X[i], drp_df$Y[i]),
    integer(1)
  )
  drp_df <- drp_df[drp_df$Berry > 0L, , drop = FALSE]

  list(drupelets = drp_df, label_im = drp_lab, label_ids = label_vec)
}

#' Detect drupelets from texture boundaries (color-independent)
#' @noRd
DrpSummary <- function(
    background_cimg,
    min_size = DRUPELET_DEFAULTS$min_size,
    blur_sigma = DRUPELET_DEFAULTS$blur_sigma,
    sensitivity = DRUPELET_DEFAULTS$sensitivity) {
  DrpSegment(
    background_cimg,
    min_size = min_size,
    blur_sigma = blur_sigma,
    sensitivity = sensitivity
  )$drupelets
}

#' Summarize drupelet counts per berry
#' @noRd
DrpPerBerry <- function(drp_df) {
  if (nrow(drp_df) == 0) {
    return(data.frame(Berry = integer(), DrupeletCount = integer()))
  }
  counts <- as.data.frame(table(drp_df$Berry), stringsAsFactors = FALSE)
  names(counts) <- c("Berry", "DrupeletCount")
  counts$Berry <- as.integer(counts$Berry)
  counts$DrupeletCount <- as.integer(counts$DrupeletCount)
  counts
}

#' Mean drupelets per berry from detection results
#' @noRd
DrpMeanPerBerry <- function(drp_df) {
  per_berry <- DrpPerBerry(drp_df)
  if (nrow(per_berry) == 0) {
    return(NA_real_)
  }
  mean(per_berry$DrupeletCount)
}

# Applies a three-channel color threshold in an arbitrary colorspace
RedDrupe <- function(cs_cimg, channel1, channel2, channel3, despeckle=F, invert = F){
  if(invert){
    RDR_px <- (imager::R(cs_cimg) >= channel1[1] & imager::R(cs_cimg) <= channel1[2] &
                 imager::G(cs_cimg) >= channel2[1] & imager::G(cs_cimg) <= channel2[2] &
                 imager::B(cs_cimg) >= channel3[1] & imager::B(cs_cimg) <= channel3[2])
  } else {
    RDR_px <- (imager::R(cs_cimg) <= channel1[1] | imager::R(cs_cimg) >= channel1[2] |
                 imager::G(cs_cimg) <= channel2[1] | imager::G(cs_cimg) >= channel2[2] |
                 imager::B(cs_cimg) <= channel3[1] | imager::B(cs_cimg) >= channel3[2])
  }
  if(despeckle){
    RDR_px <- imager::clean(RDR_px, 10) & (!imager::px.na(RDR_px))
    RDR_px <- imager::fill(RDR_px, 20) & (!imager::px.na(RDR_px))
  } else {
    RDR_px <- RDR_px & (!imager::px.na(RDR_px))
  }
  return(RDR_px)
}

# converts coordinates to a pixel set for image masking
DrpPlot <- function(img, coord){
  if (nrow(coord) == 0) {
    empty <- array(FALSE, dim = c(dim(img)[1], dim(img)[2], 1, 1))
    return(imager::as.pixset(empty))
  }
  drp_img <- imager::colorise(img, imager::as.pixset(is.na(img)), col = 0)
  drp_img[coord$X, coord$Y, 1] <- 1
  drp_img[coord$X, coord$Y, 2] <- 0
  drp_img[coord$X, coord$Y, 3] <- 0
  drp_px <- (drp_img == 1) %>%
    imager::grow(imager::px.diamond(5))
  return(drp_px)
}

# creates a color profile of non-NA pixels in a cimg
ColProfile <- function(img, cs){
  if(cs != "RGB"){
    img = switchspace(img, "RGB")
  }
  mid <- c(median(imager::R(img), na.rm = T)*255,
            median(imager::G(img), na.rm = T)*255,
            median(imager::B(img), na.rm = T)*255)
  gray <- imager::grayscale(img)
  boxstats <- boxplot(gray, plot = F)$stats
  minloc <- imager::get.locations(gray, function(x) x == boxstats[1])
  maxloc <- imager::get.locations(gray, function(x) x == boxstats[5])
  dark <- c(as.matrix(imager::R(img))[minloc$x[1], minloc$y[1]]*255,
            as.matrix(imager::G(img))[minloc$x[1], minloc$y[1]]*255,
            as.matrix(imager::B(img))[minloc$x[1], minloc$y[1]]*255)
  light <- c(as.matrix(imager::R(img))[maxloc$x[1],maxloc$y[1]]*255,
             as.matrix(imager::G(img))[maxloc$x[1],maxloc$y[1]]*255,
             as.matrix(imager::B(img))[maxloc$x[1],maxloc$y[1]]*255)
  score_mid <- abs(ShinyFruit::rgb2rhs$R-mid[1]) + abs(ShinyFruit::rgb2rhs$G-mid[2]) + abs(ShinyFruit::rgb2rhs$B-mid[3])
  score_dark <- abs(ShinyFruit::rgb2rhs$R-dark[1]) + abs(ShinyFruit::rgb2rhs$G-dark[2]) + abs(ShinyFruit::rgb2rhs$B-dark[3])
  score_light <- abs(ShinyFruit::rgb2rhs$R-light[1]) + abs(ShinyFruit::rgb2rhs$G-light[2]) + abs(ShinyFruit::rgb2rhs$B-light[3])
  return(list(red = mid[1], green = mid[2], blue = mid[3],
              mid_color = ShinyFruit::rgb2rhs$english[which.min(score_mid)],
              dark_color = ShinyFruit::rgb2rhs$english[which.min(score_dark)],
              light_color = ShinyFruit::rgb2rhs$english[which.min(score_light)]))
}

#' Downscale a cimg for faster interactive preview
#' @noRd
scale_cimg_for_preview <- function(cimg, max_dim = PREVIEW_MAX_DIM) {
  dims <- dim(cimg)
  if (max(dims[1], dims[2]) <= max_dim) {
    return(cimg)
  }
  scale <- max_dim / max(dims[1], dims[2])
  new_w <- max(1L, round(dims[1] * scale))
  new_h <- max(1L, round(dims[2] * scale))
  imager::resize(cimg, size_x = new_w, size_y = new_h)
}

#' Fast background-removal preview mask on a downscaled image
#' @noRd
background_preview <- function(cimg, col_space, c1, c2, c3) {
  preview <- scale_cimg_for_preview(cimg)
  imager::colorise(
    preview,
    RedDrupe(switchspace(preview, col_space), c1, c2, c3, despeckle = TRUE),
    col = "white"
  )
}

#' Scale detection coordinates to match a preview-sized image
#' @noRd
scale_coords_for_preview <- function(coord, source_img, preview_img) {
  if (nrow(coord) == 0) {
    return(coord)
  }
  sx <- dim(preview_img)[1] / dim(source_img)[1]
  sy <- dim(preview_img)[2] / dim(source_img)[2]
  scale_xy <- sqrt(sx * sy)
  marker_size <- if ("Pixels" %in% names(coord)) {
    pmax(1.8, pmin(7, sqrt(coord$Pixels / pi) * scale_xy * 0.09))
  } else {
    rep(3, nrow(coord))
  }
  plot_pts <- Map(
    function(x, y) {
      plot_coords_to_preview(x, y, source_img, preview_img)
    },
    coord$X,
    coord$Y
  )
  plot_x <- vapply(plot_pts, `[[`, numeric(1), 1)
  plot_y <- vapply(plot_pts, `[[`, numeric(1), 2)
  data.frame(
    Berry = coord$Berry,
    X = plot_x,
    Y = plot_y,
    Pixels = if ("Pixels" %in% names(coord)) coord$Pixels else NA_integer_,
    Drupelet = coord$Drupelet,
    MarkerSize = marker_size,
    stringsAsFactors = FALSE
  )
}

#' Resize a pixset to match a target cimg dimensions
#' @noRd
resize_pixset_to_cimg <- function(px, target_cimg) {
  if (!imager::is.pixset(px)) {
    return(px)
  }
  if (dim(px)[1] == dim(target_cimg)[1] && dim(px)[2] == dim(target_cimg)[2]) {
    return(px)
  }
  imager::resize(px, size_x = dim(target_cimg)[1], size_y = dim(target_cimg)[2])
}

#' Apply a semi-transparent feature mask overlay
#' @noRd
overlay_feature_mask <- function(base_img, feature_px, color_name, alpha = 0.5) {
  if (!imager::is.pixset(feature_px)) {
    return(base_img)
  }
  px <- resize_pixset_to_cimg(feature_px, base_img) & fruit_pixset(base_img)
  if (sum(px) == 0) {
    return(base_img)
  }
  imager::colorise(base_img, px, col = mask_color_rgb(color_name), alpha = alpha)
}

#' Apply drupelet region and centroid overlays
#' @noRd
scale_label_im_for_preview <- function(lab_im, source_img, preview_img) {
  if (is.null(lab_im)) {
    return(NULL)
  }
  if (dim(lab_im)[1] == dim(preview_img)[1] && dim(lab_im)[2] == dim(preview_img)[2]) {
    return(lab_im)
  }
  resized <- imager::resize(
    lab_im,
    size_x = dim(preview_img)[1],
    size_y = dim(preview_img)[2],
    interpolation_type = 1L
  )
  arr <- round(as.array(resized))
  imager::as.cimg(arr)
}

#' Boundary pixels of a pixset (for outline-only overlays)
#' @noRd
pixset_boundary <- function(px) {
  arr <- as.logical(as.array(px))[, , 1, 1]
  n <- nrow(arr)
  m <- ncol(arr)
  if (n < 3L || m < 3L) {
    return(px)
  }
  edge <- arr[2:(n - 1), 2:(m - 1)] & (
    !arr[1:(n - 2), 2:(m - 1)] |
      !arr[3:n, 2:(m - 1)] |
      !arr[2:(n - 1), 1:(m - 2)] |
      !arr[2:(n - 1), 3:m]
  )
  full <- matrix(FALSE, nrow = n, ncol = m)
  full[2:(n - 1), 2:(m - 1)] <- edge
  imager::as.pixset(array(full, dim = c(n, m, 1, 1)))
}

#' Light fill plus bright outlines so the base image stays visible
#' @noRd
overlay_drupelet_mask <- function(base_img, drp_lab, label_ids, fill_alpha = 0.12) {
  px <- DrpRegionMask(drp_lab, label_ids)
  if (is.null(px)) {
    return(base_img)
  }
  px <- resize_pixset_to_cimg(px, base_img)
  px <- px & fruit_pixset(base_img)
  if (sum(px) == 0) {
    return(base_img)
  }
  out <- imager::colorise(base_img, px, col = c(0.15, 0.75, 0.35), alpha = fill_alpha)
  boundary <- pixset_boundary(px)
  imager::colorise(out, boundary, col = c(0.1, 1, 0.4), alpha = 0.95)
}

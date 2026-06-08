#' Apply a per-label function and return a combined data frame
#' @noRd
map_labeled_rows <- function(label_im, label_ids, stat_fn, id_col) {
  if (length(label_ids) == 0) {
    return(data.frame())
  }
  label_ids <- as.integer(label_ids)
  label_ids <- label_ids[!is.na(label_ids) & label_ids > 0L]
  if (length(label_ids) == 0) {
    return(data.frame())
  }
  rows <- lapply(label_ids, function(id) stat_fn(label_im, id))
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame())
  }
  out[[id_col]] <- as.integer(label_ids)
  rownames(out) <- NULL
  out
}

#' Empty drupelet result frame
#' @noRd
empty_drp_df <- function() {
  data.frame(
    Berry = integer(),
    X = integer(),
    Y = integer(),
    Pixels = integer(),
    Drupelet = integer()
  )
}

#' @noRd
safe_positive_int <- function(x, default) {
  val <- suppressWarnings(as.integer(x)[1])
  if (length(val) == 0 || is.na(val) || val < 1L) {
    return(as.integer(default))
  }
  val
}

#' @noRd
safe_numeric <- function(x, default) {
  val <- suppressWarnings(as.numeric(x)[1])
  if (length(val) == 0 || is.na(val)) {
    return(default)
  }
  val
}

#' Extract positive label ids from a labeled cimg
#' @noRd
label_ids_from_cimg <- function(lab_im) {
  arr <- as.array(lab_im)
  vals <- unique(as.integer(arr))
  vals[!is.na(vals) & vals > 0L]
}

#' Single-channel pixset of non-NA fruit pixels
#' @noRd
fruit_pixset <- function(cimg) {
  imager::as.pixset(!is.na(imager::R(cimg)))
}

#' Extract numeric pixel values from a cimg at pixset locations
#' @noRd
pixset_values <- function(cimg, px) {
  vals <- as.numeric(as.array(cimg))
  mask <- as.logical(as.array(px))
  if (length(mask) != length(vals)) {
    depth <- length(vals) / length(mask)
    if (depth > 1 && depth == round(depth)) {
      vals <- vals[seq_len(length(mask))]
    }
  }
  vals[mask]
}

#' Grayscale array with NA replaced for blur/gradient ops
#' @noRd
gray_na_filled <- function(gray) {
  arr <- as.array(gray)
  arr[is.na(arr)] <- 0
  imager::as.cimg(arr)
}

#' RGB color vector (0-1) for imager::colorise
#' @noRd
mask_color_rgb <- function(color_name) {
  as.numeric(grDevices::col2rgb(color_name, alpha = FALSE) / 255)
}

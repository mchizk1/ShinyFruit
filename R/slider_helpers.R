#' Update three channel sliders from an imager cimg

#' @noRd

update_channel_sliders <- function(session, img, col_space, prefix = "", id_prefix = "channel") {

  labs <- ShinyFruit::cs_labs[, colnames(ShinyFruit::cs_labs) == col_space, drop = TRUE]

  channels <- list(

    list(id = paste0(id_prefix, "1", prefix), channel = imager::R(img)),

    list(id = paste0(id_prefix, "2", prefix), channel = imager::G(img)),

    list(id = paste0(id_prefix, "3", prefix), channel = imager::B(img))

  )

  for (i in seq_along(channels)) {

    ch <- channels[[i]]

    shiny::updateSliderInput(

      session,

      inputId = ch$id,

      label = labs[i],

      min = floor(min(ch$channel, na.rm = TRUE)) - 0.001,

      max = ceiling(max(ch$channel, na.rm = TRUE)) + 0.001,

      value = c(

        floor(min(ch$channel, na.rm = TRUE)) - 0.001,

        ceiling(max(ch$channel, na.rm = TRUE)) + 0.001

      )

    )

  }

}



#' Plot/display coordinates for a cimg pixel (matches cimg_to_ggplot + image_flop)

#' Horizontal: plot x equals cimg column (flop is already in the display pipeline).

#' Vertical: ggplot y=0 is bottom; imager y=1 is top.

#' @noRd

cimg_pixel_to_plot <- function(cimg_x, cimg_y, width, height) {

  list(

    x = as.numeric(cimg_x),

    y = as.numeric(height - cimg_y + 1)

  )

}



#' Convert ggplot/Shiny plot coordinates to imager cimg indices

#' @noRd

plot_xy_to_cimg <- function(plot_x, plot_y, width, height) {

  list(

    x = as.numeric(plot_x),

    y = as.numeric(plot_y),

    cimg_x = as.integer(round(plot_x)),

    cimg_y = as.integer(round(height - plot_y + 1))

  )

}



#' Convert imager cimg indices to ggplot/Shiny plot coordinates

#' @noRd

cimg_xy_to_plot <- function(cimg_x, cimg_y, width, height) {

  cimg_pixel_to_plot(cimg_x, cimg_y, width, height)

}



#' Normalize a brushed plot rectangle c(xmin, xmax, ymin, ymax)

#' @noRd

normalize_plot_rect <- function(xmin, xmax, ymin, ymax) {

  c(

    as.integer(round(min(xmin, xmax))),

    as.integer(round(max(xmin, xmax))),

    as.integer(round(min(ymin, ymax))),

    as.integer(round(max(ymin, ymax)))

  )

}



#' Convert a plot brush rectangle to imager cimg bounds c(xmin, xmax, ymin, ymax)

#' @noRd

plot_rect_to_cimg <- function(xmin, xmax, ymin, ymax, width, height) {

  c(

    as.integer(round(xmin)),

    as.integer(round(xmax)),

    as.integer(round(height - ymax + 1)),

    as.integer(round(height - ymin + 1))

  )

}



#' Convert imager crop bounds back to plot coordinates for overlays

#' @noRd

cimg_rect_to_plot <- function(cimg_rect, width, height) {

  c(

    cimg_rect[1],

    cimg_rect[2],

    height - cimg_rect[4] + 1,

    height - cimg_rect[3] + 1

  )

}



#' Plot dimensions used for interactive display (preview-sized)

#' @noRd

crop_plot_dims <- function(img) {

  preview <- scale_cimg_for_preview(img)

  c(dim(preview)[1], dim(preview)[2])

}



#' Scale a crop rectangle from display plot coords to a target image

#' @noRd

scale_crop_to_image <- function(crop, img) {

  if (!crop_rect_is_valid(crop)) {

    return(crop)

  }

  plot_dims <- crop_plot_dims(img)

  full_dims <- c(dim(img)[1], dim(img)[2])

  if (identical(as.integer(plot_dims), as.integer(full_dims))) {

    return(crop)

  }

  scale_rect_between_cimgs(crop, from_dims = plot_dims, to_dims = full_dims)

}



#' Map a cimg point to preview plot coordinates (flip at source, then scale)

#' @noRd

plot_coords_to_preview <- function(cimg_x, cimg_y, source_img, preview_img) {

  fw <- dim(source_img)[1]

  fh <- dim(source_img)[2]

  pw <- dim(preview_img)[1]

  ph <- dim(preview_img)[2]

  pt <- cimg_xy_to_plot(cimg_x, cimg_y, fw, fh)

  c(

    as.numeric(round(pt$x * pw / fw)),

    as.numeric(round(pt$y * ph / fh))

  )

}



#' Draw an exclusion rectangle stored in ggplot plot coordinates

#' Crop bounds c(xmin, xmax, ymin, ymax) match the brush on the flopped display.

#' When plot_dims is set, crop is on the preview display and is scaled before apply.

#' @noRd

apply_crop_rect <- function(img, crop, plot_dims = NULL) {

  if (!crop_rect_is_valid(crop)) {

    return(img)

  }

  w <- dim(img)[1]

  h <- dim(img)[2]

  crop_full <- crop

  if (!is.null(plot_dims)) {

    crop_full <- scale_rect_between_cimgs(crop, from_dims = plot_dims, to_dims = c(w, h))

  }

  cimg_crop <- plot_rect_to_cimg(

    crop_full[1], crop_full[2], crop_full[3], crop_full[4],

    w, h

  )

  imager::draw_rect(

    img,

    cimg_crop[1], cimg_crop[3],

    cimg_crop[2], cimg_crop[4],

    color = c(0, 0, 1)

  )

}



#' Scale a cimg rectangle from one image size to another

#' @noRd

scale_rect_between_cimgs <- function(rect, from_dims, to_dims) {

  c(

    as.integer(round(rect[1] * to_dims[1] / from_dims[1])),

    as.integer(round(rect[2] * to_dims[1] / from_dims[1])),

    as.integer(round(rect[3] * to_dims[2] / from_dims[2])),

    as.integer(round(rect[4] * to_dims[2] / from_dims[2]))

  )

}



#' @noRd

crop_rect_is_valid <- function(crop) {

  is.numeric(crop) &&

    length(crop) == 4 &&

    !any(is.na(crop)) &&

    crop[2] > crop[1] &&

    crop[4] > crop[3]

}



#' @noRd

cimg_to_ggplot <- function(cimg) {

  cimg %>%

    imager::cimg2magick() %>%

    magick::image_flop() %>%

    magick::image_ggplot()

}



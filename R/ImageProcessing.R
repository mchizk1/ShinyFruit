# Sets all background pixels to NA
bkb_process <- function(img){
  for(i in img){
    filename <- paste0(stringr::str_extract(i, "(?<=/)IMG(.*)(?=\\.JPG$)"),".jpg")
    img_og <- magick::image_read(i)
    img_new <- magick::image_resize(img_og, new_dims(magick::image_info(img_og)[2:3])) %>%
      magick::image_normalize() %>%
      magick::image_contrast(sharpen = 10) %>%
      magick::image_enhance() %>%
      imager::magick2cimg()
    return(img_new)
  }
}

new_dims <- function(old_dims){
  if(old_dims[1] >= old_dims[2]){
    dims <- c(1500, round((1500*old_dims[2])/old_dims[1]))
  } else {
    dims <- c(round((1500*old_dims[1])/old_dims[2]), 1500)
  }
  return(paste0(dims[1],"x",dims[2]))
}

bkb_background <- function(img, crop, setNA, cs="RGB", c1, c2, c3){
  px <- RedDrupe(switchspace(img, cs), c1, c2, c3, T)
  img_out <- imager::colorise(img, px, c(0,0,1)) %>%
    imager::mirror("y") %>%
    imager::draw_rect(crop[1], crop[3], crop[2], crop[4], color = c(0, 0, 1)) %>%
    imager::mirror("y")
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

# Computes coordinates of all drupelets  identified
DrpStat <- function(ber_cimg, ber_index){
  dst_coord <- imager::get.locations(ber_cimg, function(x) x == ber_index)
  X = mean(dst_coord$x) %>%
    round()
  Y = mean(dst_coord$y) %>%
    round()
  dst_df <- data.frame(X = X, Y = Y)
  return(dst_df)
}

# Runs berry stats over each berry in RGB colorspace
BerSummary <- function(background_cimg, stats=T){
  ber_lab <- (!is.na(background_cimg)) %>%
    imager::as.pixset() %>%
    imager::mclosing_square(2) %>%
    imager::label()
  ber_lst <- unique(ber_lab)[-1]
  if(stats){
    ber_df <- purrr::map_dfr(.x = ber_lst, ~ BerStat(ber_lab, .x) , .id = "Berry")
  } else {
    ber_df <- length(ber_lst)
  }
  return(ber_df)
}

# Runs drupelet stats across all drupelets in RGB colorspace
DrpSummary <- function(background_cimg){
  drp_lab <- switchspace(background_cimg, "Lab") %>%
    RedDrupe(c(31.66,100), c(-15,100), c(-100,100), T) %>%
    imager::label()
  label_vec <- unique(drp_lab)[-1]
  drp_df <- purrr::map_dfr(label_vec, ~ DrpStat(drp_lab, .x) , .id = "Drupelet")
  return(drp_df)
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
  drp_img <- imager::colorise(img, imager::as.pixset(is.na(img)), col = 0)
  for (i in 1:nrow(coord)) {
    imager::color.at(drp_img, coord$X[i], coord$Y[i]) <- c(1, 0, 0)
  }
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

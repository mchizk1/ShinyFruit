# Converting colorspaces
switchspace <- function(current_img, col_space){
  out_img <- current_img
  if(col_space == "HSB"){
    out_img <- imager::RGBtoHSV(current_img)
  } else if (col_space == "Lab"){
    out_img <- imager::RGBtoLab(current_img)
  }
  return(out_img)
}

# Pythagorean theorem for measuring size reference line
pythag <- function(click1, click2){
  a <- diff(c(click1[1], click2[1])) %>% abs()
  b <- diff(c(click1[2], click2[2])) %>% abs()
  c2 <- a^2 + b^2
  c <- sqrt(c2)
  return(c)
}

# Runs image analysis over a batch of images applying current settings
RunBatch <- function(indir, include_img=F, col=F,
                     drp=F, ber=F, rdr=NULL, convert=1,
                     crop=c(0,0,0,0), bkg, mask_col="red",
                     preprocess=T){
  imgs <- list.files(path = indir, full.names = T)
  outdir <- file.path(indir, "ShinyFruit_Out")
  if(!dir.exists(outdir)){
    dir.create(outdir)
  }
  Out_df <- c()
  for(i in imgs){
    Out_vec <- data.frame(File = stringr::str_extract(i, "(?<=/)([^/]*)$"),
                          ColorSpace = bkg[[1]],
                          BkgCh1Threshold= paste0("(",bkg[[2]][1],":", bkg[[2]][2],")"),
                          BkgCh2Threshold= paste0("(",bkg[[3]][1],":", bkg[[3]][2],")"),
                          BkgCh3Threshold= paste0("(",bkg[[4]][1],":", bkg[[4]][2],")"))
    # Set up image needed for analysis
    img_out <- bkb_process(i, preprocess) %>%
      bkb_background(crop=crop, setNA=T, bkg[[1]], bkg[[2]], bkg[[3]], bkg[[4]])
    # Get data for spreadsheet output
    if(ber){
      ber_df <- BerSummary(img_out)
      ber_out <- data.frame(BerryCount = nrow(ber_df), Length = mean(ber_df$L)*convert,
                 Width = mean(ber_df$W)*convert, Size = mean(ber_df$Size)*(convert^2))
      Out_vec <- cbind(Out_vec, ber_out)
    }
    if(col){
      col_lst <- ColProfile(img_out, bkg[[1]])
      col_out <- data.frame(MeanRGB = paste0("(",round(col_lst$red),":",
                                             round(col_lst$green),":",
                                             round(col_lst$blue),")"),
                            RHSDarkColor = col_lst$dark_color,
                            RHSMidColor = col_lst$mid_color,
                            RHSLightColor = col_lst$light_color)
      Out_vec <- cbind(Out_vec, col_out)
    }
    if(drp){
      drp_df <- DrpSummary(img_out)
      drp_num <- nrow(drp_df)
      if(ber == F){
        ber_num <- BerSummary(img_out, stats=F)
      }
      drp_out <- data.frame(DrupeletsPerBerry = round((drp_num/nrow(ber_df)), digits = 2))
      Out_vec <- cbind(Out_vec, drp_out)
    }
    if(!is.null(rdr)){
      if(rdr[1] == "RGB"){
        img_cs <- img_out
      } else if (rdr[[1]] == "HSB"){
        img_cs <- switchspace(img_out, "HSB")
      } else if (rdr[[1]] == "Lab"){
        img_cs <- switchspace(img_out, "Lab")
      }
      rdr_px <- RedDrupe(img_cs, rdr[[2]], rdr[[3]], rdr[[4]], rdr[[5]], T)
      rdr_sum <- sum(rdr_px, na.rm = T)
      bkb_sum <- sum(!imager::px.na(imager::R(img_out)))
      tmp_img <- img_out
      tmp_img[imager::as.pixset(1-rdr_px)] <- NA
      rdr_col <- ColProfile(tmp_img, bkg[[1]])
      RDR_out <- data.frame(FtColorSpace = rdr[[1]],
                            FtCh1Threshold = paste0("(",rdr[[2]][1],":", rdr[[2]][2],")"),
                            FtCh2Threshold = paste0("(",rdr[[3]][1],":", rdr[[3]][2],")"),
                            FtCh3Threshold = paste0("(",rdr[[4]][1],":", rdr[[4]][2],")"),
                            Feature_prop = (rdr_sum/bkb_sum),
                            FeatureRGB = paste0("(",round(rdr_col$red),":",
                                             round(rdr_col$green),":",
                                             round(rdr_col$blue),")"),
                            FeatureDarkRHS = rdr_col$dark_color,
                            FeatureMidRHS = rdr_col$mid_color,
                            FeatureLightRHS = rdr_col$light_color)
      Out_vec <- cbind(Out_vec, RDR_out)
    }
    Out_df <- rbind(Out_df, Out_vec)
    if(include_img){
      filename <- stringr::str_extract(Out_vec[1], ".*(?=\\.([:alpha:]*)$)")
      img_qc <- imager::colorise(img_out, imager::as.pixset(is.na(img_out)), "white")
      if(drp){
        img_qc <- imager::colorise(img_qc, DrpPlot(img_out, drp_df), "green")
      }
      if(!is.null(rdr)){
        img_qc <- imager::colorise(img_qc, rdr_px, col = mask_col, alpha = 0.5)
      }
      imager::save.image(img_qc, file.path(outdir, paste0(filename,".jpg")))
    }
    gc()
    shiny::incProgress(1/length(imgs))
  }
  filestamp <- lubridate::stamp("Jan17.1999.3-34-01")
  write.csv(Out_df, file.path(outdir, paste0("SF_Out_", filestamp(Sys.time()),".csv")), row.names = F)
}

################################################################################
#' @title Stand metrics experiment 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/stand_metrics.R")

#' -----------------------------------------------------------------------------
#' Libraries
library(foreach)
library(doParallel)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_pc Path to the point cloud
#' @param output_name Path and name of the outputs
#' @param threads Number of threads

path_pc <- "F:/point_clouds/for_correction/FAB2/normalized/2022-04-10_FAB2_normalized.las"
output_name <- "F:/point_clouds/for_correction/FAB2/normalized/2022-04-10_FAB2_metrics"
threads <- 4

k = 1 
xy_res = 0.5 
z_res = 0.1
z_min = 0.1
z_max = 10
limit_fOV = NULL


#' -----------------------------------------------------------------------------
#' Function

metrics <- function(path_pc, path_gpkg, output_name, threads) {
  
  #Read point cloud
  pc <- readLAS(path_pc)
  
  #Create point cloud
  pc <- data.table(X = pc$X,
                   Y = pc$Y,
                   Z = pc$Z,
                   weight = 1/pc$NumberOfReturns,
                   ScanAngleRank = pc$ScanAngleRank)
  
  #Select specific field of view
  if(is.null(limit_fOV) != TRUE) {
    pc <- subset(pc, ScanAngleRank <= limit_fOV & ScanAngleRank >= -limit_fOV)
  }
    
  #Just col of interest
  pc <- pc[, 1:4]
  gc()
    
  #Limits
  x_range <- range(pc$X)
  y_range <- range(pc$Y)
  z_max <- ifelse(is.null(z_max), max(pc$Z), z_max)
    
  #Sequences
  x_seq <- seq((x_range[1]+xy_res/2), (x_range[2]-xy_res/2), by = xy_res)
  y_seq <- seq((y_range[1]+xy_res/2), (y_range[2]-xy_res/2), by = xy_res)
  z_seq <- c(-10, seq((z_min+z_res/2), (z_max-z_res/2), by = z_res))
  
  #Labels
  x_lab <- x_seq + xy_res/2
  x_lab <- x_lab[1:(length(x_lab)-1)]
  y_lab <- y_seq + xy_res/2
  y_lab <- y_lab[1:(length(y_lab)-1)]
  z_lab <- z_seq + z_res/2
  z_lab <- z_lab[1:(length(z_lab)-1)]
  
  #Break points into categories
  pc[, X_box := as.numeric(as.character(cut(X, breaks = x_seq, labels = x_lab)))]
  pc[, Y_box := as.numeric(as.character(cut(Y, breaks = y_seq, labels = y_lab)))]
  pc[, Z_box := as.numeric(as.character(cut(Z, breaks = z_seq, labels = z_lab)))]
  
  #Get unique grid
  xyz_grid <- unique(pc[, c("X_box", "Y_box")], by = c("X_box", "Y_box"))
  zframe <- data.table(z = z_lab[-1])
  
  #Loop over grid
  for(i in 1:nrow(xyz_grid)) {
    
    temp <- subset(pc, X_box == xyz_grid$X_box[i] &
                       Y_box == xyz_grid$Y_box[i])
    
    temp <- subset(temp, Z >= 0)
    
    #Remove ground
    z_above <- temp$Z[temp$Z >= z_min]
    
    #Next if null
    if(length(z_above) <= 5 | max(z_above) <= ((z_res*3)+z_min)) {
      next
    }
    
    #Basic grid metrics
    sub_frame <- data.table(X = xyz_grid$X_box[i], 
                            Y = xyz_grid$Y_box[i],
                            npoints = length(z_above),
                            height_max = max(temp$Z),
                            height_mean = mean(temp$Z),
                            height_cv = sd(z_above)/mean(z_above),
                            skwewness = skewness(z_above),
                            kurtosis = kurtosis(z_above))
    
    #Vertical profiles
    LAD <- LAD(temp$Z, dz = z_res, k = k, z0 = z_min)
    gpag <- gap_fraction_profile(temp$Z, dz = z_res, z0 = z_min)
    
    #Metrics from vertical profiles
    sub_frame$n_profiles <- nrow(LAD)
    sub_frame$p_vci <- VCI(z_above, sub_frame$height_max, by = z_res)
    sub_frame$p_entropy <- entropy(z_above, by = z_res, zmax = sub_frame$height_max)
    sub_frame$H <- shannon(LAD$lad)
    sub_frame$Hmax <- shannon(rep(1, nrow(LAD))) #H max
    sub_frame$equitavility <- sub_frame$H/sub_frame$Hmax
    sub_frame$negentropy <- sub_frame$Hmax - sub_frame$H
    
    #Cx, Cy, RG
    lad <- LAD
    lad$lad <- 0
    lad[(nrow(lad)+1),] <- c(z_min/2, 0)
    lad <- rbind(lad, LAD)
    poly <-sf_polygon(lad)
    centroid <- st_centroid(poly)
    centroid <- st_coordinates(centroid)
    
    sub_frame$Cx <- centroid[1,2]
    sub_frame$Cy <- centroid[1,1]
    TopRG <- sum((LAD$lad - sub_frame$Cx)^2)+sum((LAD$z - sub_frame$Cy)^2)
    sub_frame$RG <- sqrt(TopRG/length(LAD$lad))
    
    #Profile manage for stacking
    lad_profile <- merge(zframe, LAD, by = "z", all.x = TRUE, all.y = TRUE)
    lad_profile <- as.data.table(t(lad_profile)[1:2,])
    colnames(lad_profile) <- paste0("LAD_", as.character(z_seq))
    lad_profile <- lad_profile[2,]
    
    gpag_profile <- merge(zframe, gpag, by = "z", all.x = TRUE, all.y = TRUE)
    gpag_profile <- as.data.table(t(gpag_profile)[1:2,])
    colnames(gpag_profile) <- paste0("Pgap_", as.character(z_seq))
    gpag_profile <- gpag_profile[2,]
    
    #Stack results
    profiles <- cbind(lad_profile, gpag_profile)
    sub_frame <- cbind(sub_frame, profiles)
    
    #stack
    sub_complete <- rbind(sub_complete, sub_frame)
    
    #Clean residuals
    rm(list = c("temp", "z_above", "sub_frame", "LAD", "gpag", "lad", "poly", 
                "centroid", "TopRG", "lad_profile", "gpag_profile", "profiles"))
    gc()
    
  }
  
  #Export
  fwrite(complete, output_name, sep = "\t")
  
  #Return 
  return(complete)
  
}

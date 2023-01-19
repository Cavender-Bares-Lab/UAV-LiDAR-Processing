################################################################################
#' @title Stand metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(moments)
library(sf)
library(sfheaders)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param k the coefficient of extinction.
#' @param xy_res vertical resolution.
#' @param z_res vertical resolution.
#' @param z_min threshold for understory or floor points.
#' @param z_max maximum height, useful for stacking profiles with different elevations. If NULL
#' it uses the point with the heights elevation.
#' @param limit_fOV degrees to limit the field of view. If NULL it uses all the points.

#point_cloud <- readLAS("data/PIBA-1050.las")

#' -----------------------------------------------------------------------------
#' Function
stand_metrics <- function(point_cloud, 
                          k = 1, 
                          xy_res = 0.5, 
                          z_res = 0.1,
                          z_min = 0.25,
                          z_max = 15,
                          limit_fOV = NULL) {
  
  #Create point cloud
  pc <- data.table(X = point_cloud$X,
                   Y = point_cloud$Y,
                   Z = point_cloud$Z,
                   ScanAngleRank = point_cloud$ScanAngleRank)
  
  #Select specific field of view
  if(is.null(limit_fOV) != TRUE) {
    pc <- subset(pc, ScanAngleRank <= limit_fOV & ScanAngleRank >= -limit_fOV)
  }
  
  #Just col of interest
  pc <- pc[, 1:3]
  
  #Limits
  x_range <- range(pc$X)
  y_range <- range(pc$Y)
  z_max <- ifelse(is.null(z_max), max(pc$Z), z_max)
  
  #Sequences
  x_seq <- seq((x_range[1]+xy_res/2), (x_range[2]-xy_res/2), by = xy_res)
  y_seq <- seq((y_range[1]+xy_res/2), (y_range[2]-xy_res/2), by = xy_res)
  z_seq <- seq((z_min+z_res/2), (z_max-z_res/2), by = z_res)
  xyz_grid <- CJ(X = x_seq, Y = y_seq)
  zframe <- data.table(z = z_seq)
  
  #Frame to complete
  sub_complete <- data.table()

  #Loop over grid
  for(i in 1:nrow(xyz_grid)) {
    
    print(i)
    
    temp <- subset(pc, X >= pc$X[i]-xy_res/2 &
                       X < pc$X[i]+xy_res/2 &
                       Y >= pc$Y[i]-xy_res/2 &
                       Y < pc$Y[i]+xy_res/2)
    
    #Remove ground
    z_above <- temp$Z[temp$Z >= z_min]
    
    #Next if null
    if(length(z_above) <= 10 | max(z_above) <= ((z_res*3)+z_min)) {
      next
    }
    
    #Basic grid metrics
    sub_frame <- data.table(X = pc$X[i], 
                            Y = pc$Y[i],
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
    colnames(lad_profile) <- paste0("LAD_", as.character(lad_profile[1,]))
    lad_profile <- lad_profile[2,]
    
    gpag_profile <- merge(zframe, gpag, by = "z", all.x = TRUE, all.y = TRUE)
    gpag_profile <- as.data.table(t(gpag_profile)[1:2,])
    colnames(gpag_profile) <- paste0("Pgap_", as.character(gpag_profile[1,]))
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
  
  #All the stand
  #Remove ground
  z_above <- pc$Z[pc$Z >= z_min]
  
  #Next if null
  if(length(z_above) <= 10 | max(z_above) <= ((z_res*3)+z_min)) {
    next
  }
  
  #Basic grid metrics
  sub_frame <- data.table(X = 0, 
                          Y = 0,
                          npoints = length(z_above),
                          height_max = max(pc$Z),
                          height_mean = mean(pc$Z),
                          height_cv = sd(z_above)/mean(z_above),
                          skwewness = skewness(z_above),
                          kurtosis = kurtosis(z_above))
  
  #Vertical profiles
  LAD <- LAD(pc$Z, dz = z_res, k = k, z0 = z_min)
  gpag <- gap_fraction_profile(pc$Z, dz = z_res, z0 = z_min)
  
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
  rm(list = c("pc", "z_above", "sub_frame", "LAD", "gpag", "lad", "poly", 
          "centroid", "TopRG", "lad_profile", "gpag_profile", "profiles", 
          "x_range", "y_range", "z_max", "x_seq", "y_seq", "z_seq", "xyz_grid", 
          "zframe"))
  gc()

  #Return
  return(sub_complete)
}

#Shannon function
shannon <- function(n_points) {
  p.i <- n_points/sum(n_points)
  H <- (-1) * sum(p.i * log10(p.i))
  return(H)
}


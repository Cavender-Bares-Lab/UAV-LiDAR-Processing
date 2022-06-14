################################################################################
#' @title Forest Structure 
################################################################################

#' @description Get the metrics of Forest Structure from a point cloud.

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

#' -----------------------------------------------------------------------------
#' @example

#point_cloud <- readLAS("data/PIBA_1015.las")
#forest_structure(point_cloud, k = 1, xy_res = 1.0, z_res = 0.25, z_min = 0.25, z_max = 8)
#forest_structure(point_cloud[1:3], k = 1, xy_res = 1.0, z_res = 0.25, z_min = 0.25, z_max = 8)


#' -----------------------------------------------------------------------------
#' Function
forest_structure <- function(point_cloud, 
                             k = 1, 
                             xy_res = 1.0, 
                             z_res = 0.25,
                             z_min = 0.25,
                             z_max = 8) {
  
  #Create point cloud
  pc <- data.table(X = point_cloud$X,
                   Y = point_cloud$Y,
                   Z = point_cloud$Z)
  
  #Get mid location
  X <- mean(range(pc$X))
  Y <- mean(range(pc$X))
  
  #Profile
  z_seq <- seq((z_min+z_res/2), (z_max-z_res/2), by = z_res)
  zframe <- data.table(z = z_seq)
  
  #Add basic information
  if(nrow(pc[pc$Z >= z_min, ]) == 0) {
    
    #Remove ground
    z_above <- 0
    
    #Basic grid metrics
    sub_frame <- data.table(X = X, 
                            Y = Y,
                            z_points = 0,
                            height_max = NA,
                            height_mean = NA,
                            height_cv = NA,
                            skwewness = NA,
                            kurtosis = NA)
    
  } else {
    
    #Remove ground
    z_above <- pc$Z[pc$Z >= z_min]
    
    #Basic grid metrics
    sub_frame <- data.table(X = X, 
                            Y = Y,
                            z_points = length(z_above),
                            height_max = max(z_above),
                            height_mean = mean(z_above),
                            height_cv = sd(z_above)/mean(z_above),
                            skwewness = skewness(z_above),
                            kurtosis = kurtosis(z_above))
    
  }
  
  #Vertical profiles
  LAD <- LAD(pc$Z, dz = z_res, k = k, z0 = z_min)
  gpag <- gap_fraction_profile(pc$Z, dz = z_res, z0 = z_min)
  
  #If profiles are bigger than 2 times z_res
  if(min(z_above) >= z_min & max(z_above) >= z_res*2 & nrow(LAD) >= 2) {
    
    #Metrics from vertical profiles
    sub_frame$n_profiles <- nrow(LAD)
    sub_frame$LAI <- sum(LAD$lad)
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
    sub_frame$RG <- sqrt(TopRG/(length(LAD$lad)+1))
    
    rm(list = c("poly", "centroid", "lad", "TopRG"))
    
    #Profile manage for stacking
    lad_profile <- merge(zframe, LAD, by = "z", all.x = TRUE, all.y = TRUE)
    lad_profile[is.na(lad), lad := 0]
    lad_profile <- as.data.table(t(lad_profile)[1:2,])
    colnames(lad_profile) <- paste0("LAD_", as.character(z_seq))
    lad_profile <- lad_profile[2,]
    
    gpag_profile <- merge(zframe, gpag, by = "z", all.x = TRUE, all.y = TRUE)
    gpag_profile[is.na(gf), gf := 1]
    gpag_profile <- as.data.table(t(gpag_profile)[1:2,])
    colnames(gpag_profile) <- paste0("Pgap_", as.character(z_seq))
    gpag_profile <- gpag_profile[2,]
    
  } else {
    
    #Metrics from vertical profiles
    sub_frame$n_profiles <- nrow(LAD)
    sub_frame$LAI <- NA
    sub_frame$p_vci <- NA
    sub_frame$p_entropy <- NA
    sub_frame$H <- NA
    sub_frame$Hmax <- NA
    sub_frame$equitavility <- NA
    sub_frame$negentropy <- NA
    
    #Cx, Cy, RG
    sub_frame$Cx <- NA
    sub_frame$Cy <- NA
    sub_frame$RG <- NA
    
    #Profile manage for stacking
    lad_profile <- merge(zframe, LAD, by = "z", all.x = TRUE, all.y = TRUE)
    lad_profile <- as.data.table(t(lad_profile)[1:2,])
    colnames(lad_profile) <- paste0("LAD_", as.character(z_seq))
    lad_profile <- lad_profile[2,]
    
    gpag_profile <- merge(zframe, gpag, by = "z", all.x = TRUE, all.y = TRUE)
    gpag_profile <- as.data.table(t(gpag_profile)[1:2,])
    colnames(gpag_profile) <- paste0("Pgap_", as.character(z_seq))
    gpag_profile <- gpag_profile[2,]
    
  }
  
  #Stack results
  profiles <- cbind(lad_profile, gpag_profile)
  sub_frame <- cbind(sub_frame, profiles)
  
  #Clean residuals
  rm(list = c("pc", "z_above", "X", "Y", "LAD", "gpag", "lad_profile", 
              "gpag_profile", "profiles", "z_seq", "zframe"))
  gc()
  
  return(sub_frame)
  
}

#Shannon function
shannon <- function(n_points) {
  p.i <- n_points/sum(n_points)
  H <- (-1) * sum(p.i * log10(p.i))
  return(H)
}


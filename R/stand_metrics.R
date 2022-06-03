################################################################################
#' @title Stand metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(moments)

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

#' -----------------------------------------------------------------------------
#' Function
stand_metrics <- function(point_cloud, 
                          k = 1, 
                          xy_res = 1.0, 
                          z_res = 0.25,
                          z_min = 0.25,
                          z_max = 10,
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

  #Stand grid
  for(i in 1:nrow(xyz_grid)) {
    temp <- subset(pc, X >= pc$X[i]-xy_res/2 &
                       X < pc$X[i]+xy_res/2 &
                       Y >= pc$Y[i]-xy_res/2 &
                       Y < pc$Y[i]+xy_res/2)
    
    #Stand grid metrics
    sub_frame <- data.table(X = pc$X[i], 
                            Y = pc$X[i],
                            height = max(temp$Z), 
                            shannon = shannon,
                            ri = rumple_index(x = temp$X[temp$Z >= z_min], 
                                              y = temp$Y[temp$Z >= z_min], 
                                              z = temp$Z[temp$Z >= z_min]),
                            vci = VCI(temp$Z[temp$Z >= z_min], max_z),
                            skwewness = skewness(temp$Z[temp$Z >= z_min]),
                            kurtosis = kurtosis(temp$Z[temp$Z >= z_min]))
    
    
    
    
    LAD <- LAD(temp$Z, dz = z_res, k = k, z0 = z_min)
    gpag <- gap_fraction_profile(temp$Z, dz = z_res, z0 = z_min)
    
    #Merge
    profile <- merge(cframe, LAD, by = "z", all.x = TRUE, all.y = TRUE)
    profile <- merge(profile, gpag, by = "z", all.x = TRUE, all.y = TRUE)
    profile <- as.data.table(t(profile)[2:3,])
    colnames(profile) <- as.character(z_seq)
    
  }

  
  
  return(frame)
}

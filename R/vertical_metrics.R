################################################################################
#' @title Vertical metrics 
################################################################################

#' @description Get the vertical metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(moments)
library(sfheaders)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param z_res vertical resolution.
#' @param z_min threshold for understory or floor points.
#' @param z_max maximum height, useful for stacking profiles with different elevations. If NULL
#' it uses the point with the heights elevation.

#' -----------------------------------------------------------------------------
#' Function
vertical_metrics <- function(point_cloud, 
                             z_res = 0.25,
                             z_min = 0.25,
                             z_max = 10) {
  
  #Create point cloud
  pc <- data.table(X = point_cloud$X,
                   Y = point_cloud$Y,
                   Z = point_cloud$Z)
  
  #Above points
  z_above <- pc[Z >= z_min, Z]
  
  #Basic grid metrics
  sub_frame <- data.table(height = max(pc$Z),
                          SEI_vertical = entropy(z_above, 
                                                 by = z_res, 
                                                 zmax = max(z_above)))
  
  # Define the number of x meters bins from 0 to zmax (rounded to the next integer)
  bk <- seq(z_res, ceiling(max(pc$Z)/z_res)*z_res, z_res)
  
  # Compute the p for each bin
  hist_count <- hist(z_above, breaks = bk, plot = F)$count
  hist_count <- hist_count + 1 #In case of occlusion
  hist_count <- hist_count/sum(hist_count)
  sub_frame$vertical_hill0 <- hill(hist_count, 0)
  sub_frame$vertical_hill1 <- hill(hist_count, 0.9999)
  sub_frame$vertical_hill2 <- hill(hist_count, 2)
  
  #Clean residuals
  rm(list = c("pc", "z_above", "hist_count", "bk")) #"LAD"
  gc()
  
  #Return
  return(sub_frame)
  
}

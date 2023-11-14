################################################################################
#' @title Horizontal metrics 
################################################################################

#' @description Get the vertical metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(sfheaders)
library(terra)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param xy_res Resolution of the grid

#' -----------------------------------------------------------------------------
#' Function
horizontal_metrics <- function(point_cloud, xy_res = 0.1) {
  
  # Calculate grid metrics
  metrics_raster <- rasterize_canopy(point_cloud, res = xy_res, p2r()) 
  metrics_raster <- terra::focal(metrics_raster, w, fun = fill.na)
  metrics_raster[metrics_raster <= 0] <- 0.001
  metrics_raster[is.na(metrics_raster)] <- NA
  metrics <- as.numeric(na.exclude(metrics_raster[]))
  metrics <- metrics[!is.na(metrics)]
  
  mean_maximun_height <- mean(metrics)
  sd_maximun_height <- sd(metrics)
  
  #Basic grid metrics
  sub_frame <- data.table(mean_maximun_height = mean_maximun_height,
                          sd_maximun_height = sd_maximun_height,
                          cv_maximun_height = sd_maximun_height/mean_maximun_height,
                          horizontal_hill0 = hill(metrics, 0),
                          horizontal_hill1 = hill(metrics, 0.9999),
                          horizontal_hill2 = hill(metrics, 2),
                          horizontal_difference = sqrt(sum((metrics - mean_maximun_height)^2)),
                          rumple = rumple_index(metrics_raster))
  
  #Clean residuals
  rm(list = c("metrics", "metrics_raster", "mean_maximun_height", "sd_maximun_height"))
  gc()
  
  #Return
  return(sub_frame)
  
}

# User-defined function
fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) }}
w <- matrix(1, 3, 3)

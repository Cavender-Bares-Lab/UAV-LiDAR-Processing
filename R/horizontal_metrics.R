################################################################################
#' @title Horizontal metrics 
################################################################################

#' @description Get the vertical metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(sfheaders)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param xy_res Resolution of the grid

#' -----------------------------------------------------------------------------
#' Function
horizontal_metrics <- function(point_cloud, xy_res = 0.25) {
  
  # Calculate grid metrics
  metrics_raster <- pixel_metrics(point_cloud, ~f(Z), res = xy_res) 
  metrics <- as.numeric(na.exclude(metrics_raster[]))
  metrics[metrics <= 0.25] <- NA
  metrics <- metrics[!is.na(metrics)]
  
  #Basic grid metrics
  sub_frame <- data.table(npixels = length(metrics),
                          mean_maximun_height = mean(metrics),
                          horizontal_hill0 = hill(metrics, 0),
                          horizontal_hill1 = hill(metrics, 0.9999),
                          horizontal_hill2 = hill(metrics, 2))
  
  #Clean residuals
  rm(list = c("metrics", "metrics_raster"))
  gc()
  
  #Return
  return(sub_frame)
  
}

# User-defined function
f <- function(x) { list(height = quantile(x, 0.99)) }

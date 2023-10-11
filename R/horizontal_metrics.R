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
#' @param limits degrees to limit the field of view. If NULL it uses all the points.

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
                          SEI_horizontal = shannon(metrics) / shannon(rep(1, length(metrics))),
                          FHD_horizontal = shannon(metrics),
                          rumple = rumple_index(metrics_raster$height))
  
  #Clean residuals
  rm(list = c("metrics", "metrics_raster"))
  gc()
  
  #Return
  return(sub_frame)
  
}

# User-defined function
f <- function(x) { list(height = quantile(x, 0.99)) }

#Shannon function
shannon <- function(n_points) {
  p.i <- n_points/sum(n_points)
  H <- (-1) * sum(p.i * log(p.i))
  return(H)
}
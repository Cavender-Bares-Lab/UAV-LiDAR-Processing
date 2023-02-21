################################################################################
#' @title Fractal geometry metrics
################################################################################

#' @description Get the fractal geometry from a point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(rTLS)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param zmin Ground cut-off.

#' -----------------------------------------------------------------------------
#' Function
stand_fractal <- function(point_cloud, z_min = 0.25) {
  
  #Point cloud stand
  pc <- data.table(X = point_cloud$X, 
                   Y = point_cloud$Y, 
                   Z = point_cloud$Z)
  
  #Remove ground
  pc <- pc[Z >= 0.25]
  
  #Remove zmax 
  limit <- quantile(pc$Z[pc$Z > z_min], 0.95)
  pc <- subset(pc, Z <= limit)
  
  min_dist = 0.1
  
  #Range size
  ranges <- c(max(pc[,1]) - min(pc[,1]), max(pc[,2]) - min(pc[,2]), max(pc[,3]) - min(pc[,3]))
  max.range <- ranges[which.max(ranges)] + 0.001
  max.range <- max.range/4
  edge_sizes <- seq(from = log10(c(max.range)), to = log10(min_dist), length.out = 10)
  edge_sizes <- 10^edge_sizes
  
  #Add id
  fractals <- voxels_counting(pc, 
                              edge_sizes = edge_sizes, 
                              min_size = min_dist, 
                              length_out = 10, 
                              bootstrap = FALSE, 
                              R = 500, 
                              progress = FALSE, 
                              parallel = FALSE, 
                              threads = NULL)
  
  #Get model
  model <- lm(fractals$H ~ log10(1/(fractals$Edge.X*fractals$Edge.Y*fractals$Edge.Z)))      
  
  results <- data.table(Intercept = model$coefficients[1],
                        Slope = model$coefficients[2],
                        Rsq = summary(model)$r.squared)
  
  #Export
  return(results)
  
}

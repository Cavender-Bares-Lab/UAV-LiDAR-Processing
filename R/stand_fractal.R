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
  pc <- data.table(X = pcs[[i]]$X, 
                   Y = pcs[[i]]$Y, 
                   Z = pcs[[i]]$Z)
  
  #Remove ground
  pc <- pc[Z >= 0.25]
  
  #Apply function
  min_dist <- min_distance(pc,
                           distance = "euclidean",
                           threads = 0L,
                           verbose = FALSE,
                           progress = FALSE)
  
  if(min_dist == 0) {
    min_dist = 0.01
  }
  
  #Add id
  fractals <- voxels_counting(pc, 
                              edge_sizes = NULL, 
                              min_size = min_dist, 
                              length_out = 10, 
                              bootstrap = FALSE, 
                              R = 500, 
                              progress = FALSE, 
                              parallel = FALSE, 
                              threads = NULL)
  
  #Get model
  model <- lm(log10(fractals$N_voxels) ~ log10(1/(fractals$Edge.X*fractals$Edge.Y*fractals$Edge.Z)))      
  
  results <- data.table(Intercept = model$coefficients[1],
                        Slope = model$coefficients[2],
                        Rsq = summary(model)$r.squared)
  
  #Export
  return(results)
  
}
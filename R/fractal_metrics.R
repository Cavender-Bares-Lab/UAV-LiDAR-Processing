################################################################################
#' @title Fractal geometry metrics
################################################################################

#' @description Get the fractal geometry from a point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(rTLS)
library(smatr)

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
  
  min_dist <- 0.1
  
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
  N_model <- sma(formula = log10(N_voxels) ~ log10(1/(fractals$Edge.X^3)),
                 data = fractals, 
                 method=c("SMA"))

  H_model <- sma(formula = log10(H) ~ log10(1/(fractals$Edge.X^3)),
                 data = fractals, 
                 method=c("SMA"))
  
  results <- data.table(Intercept_N = N_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_H = H_model$coef[[1]]$`coef(SMA)`[1],
                        Slope_N = N_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_H = H_model$coef[[1]]$`coef(SMA)`[2],
                        Rsq_N = N_model$r2[[1]][1],
                        Rsq_H = H_model$r2[[1]][1])
  
  #Clean residuals
  rm(list = c("pc", "min_dist", "ranges", "max.range", "edge_sizes", "fractals",
              "N_model", "H_model"))
  gc()
  
  #Export
  return(results)
  
}

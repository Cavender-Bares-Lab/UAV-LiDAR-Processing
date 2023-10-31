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
fractal_metrics <- function(point_cloud, z_min = 0.25) {
  
  
  #Point cloud stand
  point_cloud <- data.table(X = point_cloud$X, 
                            Y = point_cloud$Y, 
                            Z = point_cloud$Z)
  
  #Remove ground
  point_cloud <- point_cloud[Z >= z_min]
  
  #Range size
  max_dist <- max(diff(range(point_cloud$X)), diff(range(point_cloud$Y)), diff(range(point_cloud$Z)))
  max_dist <- max_dist/4
  min_dist <- 0.1
  edge_sizes <- seq(from = log10(c(max_dist)), to = log10(min_dist), length.out = 30)
  edge_sizes <- c(10^edge_sizes)
  
  #Add id
  fractals <- voxels_counting(point_cloud, 
                              edge_sizes = edge_sizes, 
                              min_size = min_dist)
  
  #Get model
  Hill0_model <- sma(formula = log(Hill0) ~ log(1/(fractals$Edge.X)),
                     data = fractals, 
                     method=c("SMA"))
  
  Hill1_model <- sma(formula = log(Hill1) ~ log(1/(fractals$Edge.X)),
                     data = fractals, 
                     method=c("SMA"))
  
  Hill2_model <- sma(formula = log(Hill2) ~ log(1/(fractals$Edge.X)),
                     data = fractals, 
                     method=c("SMA"))
  
  results <- data.table(Intercept_Hill0 = Hill0_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_Hill1 = Hill1_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_Hill2 = Hill2_model$coef[[1]]$`coef(SMA)`[1],
                        Slope_Hill0 = Hill0_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_Hill1 = Hill1_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_Hill2 = Hill2_model$coef[[1]]$`coef(SMA)`[2],
                        Rsq_Hill0 = Hill0_model$r2[[1]][1],
                        Rsq_Hill1 = Hill2_model$r2[[1]][1],
                        Rsq_Hill2 = Hill1_model$r2[[1]][1])
  
  #Clean residuals
  rm(list = c("point_cloud", "min_dist", "max_dist", "edge_sizes", "fractals",
              "Hill0_model", "Hill1_model", "Hill2_model"))
  gc()
  
  #Export
  return(results)
  
}

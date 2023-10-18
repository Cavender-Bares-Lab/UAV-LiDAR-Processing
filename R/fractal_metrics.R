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
  pc <- data.table(X = point_cloud$X, 
                   Y = point_cloud$Y, 
                   Z = point_cloud$Z)
  
  #Remove ground
  pc <- pc[Z >= z_min]
  
  #Range size
  max_dist <- 2.5
  min_dist <- 0.1
  edge_sizes <- seq(from = log10(c(max_dist)), to = log10(min_dist), length.out = 10)
  edge_sizes <- c(10^edge_sizes, 0.25)
  
  #Add id
  fractals <- voxels_counting(pc, 
                              edge_sizes = edge_sizes, 
                              min_size = min_dist, 
                              length_out = 10)
  ENL_25 <- fractals[Edge.X == 0.25,]
  fractals <- fractals[Edge.X != 0.25,]
  
  #Get model
  Hill0_model <- sma(formula = log(Hill0) ~ log(1/(fractals$Edge.X^3)),
                     data = fractals, 
                     method=c("SMA"))
  
  Hill1_model <- sma(formula = log(Hill1) ~ log(1/(fractals$Edge.X^3)),
                     data = fractals, 
                     method=c("SMA"))
  
  Hill2_model <- sma(formula = log(Hill2) ~ log(1/(fractals$Edge.X^3)),
                     data = fractals, 
                     method=c("SMA"))
  
  ENL0_model <- sma(formula = log(ENL_Hill0) ~ log(1/(fractals$Edge.X^3)),
                 data = fractals, 
                 method=c("SMA"))
  
  ENL1_model <- sma(formula = log(ENL_Hill1) ~ log(1/(fractals$Edge.X^3)),
                 data = fractals, 
                 method=c("SMA"))
  
  ENL2_model <- sma(formula = log(ENL_Hill2) ~ log(1/(fractals$Edge.X^3)),
                    data = fractals, 
                    method=c("SMA"))
  
  results <- data.table(Intercept_Hill0 = Hill0_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_Hill1 = Hill1_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_Hill2 = Hill2_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_ENL0 = ENL0_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_ENL1 = ENL1_model$coef[[1]]$`coef(SMA)`[1],
                        Intercept_ENL2 = ENL2_model$coef[[1]]$`coef(SMA)`[1],
                        Slope_Hill0 = Hill0_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_Hill1 = Hill1_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_Hill2 = Hill2_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_ENL0 = ENL0_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_ENL1 = ENL1_model$coef[[1]]$`coef(SMA)`[2],
                        Slope_ENL2 = ENL2_model$coef[[1]]$`coef(SMA)`[2],
                        Rsq_Hill0 = Hill0_model$r2[[1]][1],
                        Rsq_Hill1 = Hill2_model$r2[[1]][1],
                        Rsq_Hill2 = Hill1_model$r2[[1]][1],
                        Rsq_ENL0 = ENL0_model$r2[[1]][1],
                        Rsq_ENL1 = ENL1_model$r2[[1]][1],
                        Rsq_ENL2 = ENL2_model$r2[[1]][1],
                        ENL_25 = ENL_25$ENL_Hill0[1],
                        ENL_25 = ENL_25$ENL_Hill1[1],
                        ENL_25 = ENL_25$ENL_Hill2[1])
  
  #Clean residuals
  rm(list = c("pc", "min_dist", "max_dist", "edge_sizes", "fractals", "ENL_25",
              "ENL0_model", "ENL1_model", "ENL2_model", "Hill0_model", "Hill1_model", "Hill2_model"))
  gc()
  
  #Export
  return(results)
  
}

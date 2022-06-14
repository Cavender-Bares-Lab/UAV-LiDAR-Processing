################################################################################
#' @title Forest Fractal geometry
################################################################################

#' @description Get the metrics of fractal geometry from a point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(rTLS)
library(moments)
library(sf)
library(sfheaders)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param z_min threshold for understory or floor points.

#' -----------------------------------------------------------------------------
#' @example

#point_cloud <- readLAS("data/PIBA_1015.las")
#forest_fractal(point_cloud, z_min = 0.25)
#forest_fractal(point_cloud[1:3], z_min = 0.25)

#' -----------------------------------------------------------------------------
#' Function
forest_fractal <- function(point_cloud, z_min = 0.25) {
  
  #Resample
  pc <- decimate_points(point_cloud, random_per_voxel(0.01, 1))
  
  #Create point cloud
  pc <- data.table(X = pc$X,
                   Y = pc$Y,
                   Z = pc$Z)
  
  #Remove ground
  pc <- subset(pc, Z >= z_min)
  
  #Add basic information
  if(nrow(pc) == 0 | max(pc$Z) < 1.0) {
    
    frame <- data.table(points = 0,
                        intercept = NA,
                        slope = NA,
                        rsq = NA)
    
  } else {
    
    distance <- min_distance(pc, 
                             distance = "euclidean",
                             threads = 1L,
                             verbose = FALSE,
                             progress = FALSE)
    
    if(distance == 0) {
      distance <- 0.01
    }
    
    counting <- voxels_counting(pc, 
                                edge_sizes = NULL,
                                min_size = distance,
                                length_out = 10, 
                                bootstrap = FALSE,
                                R = NULL,
                                progress = FALSE,
                                parallel = FALSE,
                                threads = NULL)
    
    
    fractal <- lm(H ~ log10(1/(Edge.X*Edge.Y*Edge.Z)), 
                    data = counting)
    
    frame <- data.table(points = nrow(pc),
                        intercept = fractal$coefficients[1],
                        slope = fractal$coefficients[2],
                        rsq = summary(fractal)$r.squared)
    
    #Clean residuals
    rm(list = c("distance", "counting", "fractal"))
    
  }
    
  #Clean residuals
  rm(list = c("pc"))
  gc()
  
  return(frame)
  
}

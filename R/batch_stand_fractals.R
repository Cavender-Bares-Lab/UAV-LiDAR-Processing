################################################################################
#' @title Batch stand fractals 
################################################################################

#' @description Get the fractal geometry of stands.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/stand_metrics.R")

#' -----------------------------------------------------------------------------
#' Libraries
library(foreach)
library(doParallel)
library(rTLS)

#-------------------------------------------------------------------------------
#' Arguments
#' @param point_cloud
#' @param limits_gpkg
#' @param output_name 
#' @param threads

point_cloud <- readLAS("F:/point_clouds/2022-05-18_FAB2.las")
limits_gpkg <- st_read(dsn = "data/FAB2_plots.gpkg")
threads <- 4

#' -----------------------------------------------------------------------------
#' Function

batch_stand_fractals <- function(point_cloud, limits_gpkg, output_name, threads) {
  
  #Divide point cloud
  pcs <- clip_roi(point_cloud, limits_gpkg)
  
  #Set up cluster
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  #N of stands
  stands <- length(pcs)
  
  # Loop over scenes to estimate delta VI
  complete <- foreach(i = 1:stands,
                      .combine=rbind,
                      .packages = c("rTLS", "data.table"),
                      .inorder = F) %dopar% {
                        
                        if(!is.null(pcs[[i]])) {
                          
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
                            min_dist = 0.005
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
                          
                          #Add id
                          fractals$id <- limits_gpkg$id[i]
                          
                          #Get model
                          
                          model <- lm(log10(fractals$H+1) ~ log10(1/(fractals$Edge.X*fractals$Edge.Y*fractals$Edge.Z)))      
                          
                          #Export
                          return(metrics)
                          
                        }
                      }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
  #Export
  fwrite(complete, output_name, sep = "\t")
  
  #Return 
  return(complete)
  
}

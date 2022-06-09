################################################################################
#' @title Batch stand metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/stand_metrics.R")

#' -----------------------------------------------------------------------------
#' Libraries
library(foreach)
library(doParallel)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_pc Path to the point cloud
#' @param path_gpkg Path to the gpkl
#' @param output_name Path and name of the outputs
#' @param threads Number of threads

path_pc <- "Z:/9-UAV/LiDAR/2022-04-10_FAB1-2/L3/FAB2/2022-04-10_FAB2_normalized.las"
path_gpkg <- "data/FAB2_plots.gpkg"
point_cloud <- "Z:/9-UAV/LiDAR/2022-04-10_FAB1-2/L3/FAB2/2022-04-10_FAB2m"
threads <- 16

#' -----------------------------------------------------------------------------
#' Function

batch_stand_metrics <- function(path_pc, path_gpkg, output_name, threads) {
  
  #Read point cloud
  pc <- readLAS(path_pc)
  
  #Read gpkp
  limits_gpkg <- st_read(dsn = path_gpkg)
  
  #Divide point cloud
  pcs <- clip_roi(pc, limits_gpkg)
  
  #Release memory
  rm(list = c("pc"))
  gc()
  
  #N of stands
  stands <- length(pcs)
  
  #Combine function
  frame_combine <- function(x, b) {
    
    if(class(x) == "data.table" & class(b) == "data.table") {
      frame <- rbind(x, b)
    }
    return(frame)
  }
  
  #Set up cluster
  cl <- makeCluster(threads)
  #cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate delta VI
  complete <- foreach(i = 1:10,
                      .combine= frame_combine,
                      .packages = c("lidR", "data.table", "moments", "sf", "sfheaders"),
                      .export = c("stand_metrics", "shannon"),
                      .inorder = F) %dopar% {
                         
                         if(!is.null(pcs[[i]])) {
                         
                         #Apply function
                         metrics <- stand_metrics(point_cloud = pcs[[i]], 
                                                  k = 1, 
                                                  xy_res = 1, 
                                                  z_res = 0.25, 
                                                  z_min = 0.25, 
                                                  z_max = 10)
                         
                         #Add id
                         metrics$id <- limits_gpkg$id[i]
                         
                         #Reciduals
                         gc()
                         
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

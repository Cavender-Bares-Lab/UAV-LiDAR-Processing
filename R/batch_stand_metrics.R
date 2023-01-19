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
#' @param edge A distance to the plot edge to apply as a buffer.
#' @param output_name Path and name of the outputs
#' @param threads Number of threads

root_path <- "/home/antonio/Documents"
path_pc <- paste0(root_path, "/", "2022-07-06_FAB2_normalized.las")
path_gpkg <- "data/plot/FAB2_plots.gpkg"
output_name <- "F:/point_clouds/for_correction/FAB2/normalized/2022-04-10_FAB2_metrics"
threads <- 4

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
  
  #Loop over the number of stands
  for(i in 1:stands) {
    
    #Get subsets
    sub_pc <- pcs[[i]]
    
    #The boundary box
    limits <- limits_gpkg[i, ]
    limits <- st_bbox(limits$geom)
    
    #Rotate stand
    pc_rotate <- rotate_stand(sub_pc, limits)
    
    metrics <- stand_metrics(point_cloud = pc_rotate, 
                             k = 1, 
                             xy_res = 1.0, 
                             z_res = 0.1, 
                             z_min = 0.25, 
                             z_max = 12)
    
    
    
  }
  
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
  complete <- foreach(i = 1:4,
                      .combine= frame_combine,
                      .packages = c("lidR", "data.table", "moments", "sf", "sfheaders"),
                      .export = c("stand_metrics", "shannon"),
                      .inorder = F) %do% {
                         
                         if(!is.null(pcs[[i]])) {
                         
                         #Apply function
                         metrics <- stand_metrics(point_cloud = pcs[[i]], 
                                                  k = 1, 
                                                  xy_res = 1.0, 
                                                  z_res = 0.1, 
                                                  z_min = 0.25, 
                                                  z_max = 12)
                         
                         #Add id
                         metrics$id <- limits_gpkg$id[i]
                         
                         #Residuals
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

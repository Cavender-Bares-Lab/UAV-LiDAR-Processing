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
#' @param point_cloud
#' @param limits_gpkg
#' @param output_name 
#' @param threads

point_cloud <- readLAS("F:/point_clouds/2022-05-18_FAB2.las")
limits_gpkg <- st_read(dsn = "data/FAB2_plots.gpkg")
threads <- 4

#' -----------------------------------------------------------------------------
#' Function

batch_stand_metrics <- function(point_cloud, limits_gpkg, output_name, threads) {
  
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

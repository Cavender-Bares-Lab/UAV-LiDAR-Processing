################################################################################
#' @title Batch stand metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/forest_fractal.R")

#' -----------------------------------------------------------------------------
#' Libraries
library(foreach)
library(doParallel)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_pc Path to the point cloud
#' @param path_gpkg Path to the gpkg.
#' @param output_name Path and name of the outputs
#' @param buffer Applies a buffer to remove the edge effect. If buffer = NULL, buffer
#' is applied. To remove the edge effect it most be negative.
#' @param threads Number of threads. Be careful it may requires a lot of memory.

path_pc <- "/media/antonio/antonio_ssd/point_clouds/2022-05-18/2022-05-18_FAB2_normalized.las"
path_gpkg <- "data/FAB2_plots.gpkg"
output_name <- "/media/antonio/antonio_ssd/point_clouds/2022-05-18/2022-05-18_fractals"
buffer = -0.5
threads <- 22

#batch_stand_metrics(path_pc, path_gpkg,  output_name,  buffer = -0.5, threads = 16)

#' -----------------------------------------------------------------------------
#' Function

batch_stand_metrics <- function(path_pc, 
                                path_gpkg, 
                                output_name, 
                                buffer = -0.5, 
                                threads = 16) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  pc <- readLAS(path_pc)
  
  #Read gpkp
  limits_gpkg <- st_read(dsn = path_gpkg)
  
  if(is.null(buffer) != TRUE) {
    aoi <- st_buffer(limits_gpkg, -0.5)
  } else {
    aoi <- limits_gpkg
  }
  
  #Clip point cloud        ---------------------------------------------
  extend <- st_bbox(aoi)
  
  #Rectangle for speed
  pc <- clip_rectangle(pc, xleft = extend[1], 
                       ybottom = extend[2], 
                       xright = extend[3], 
                       ytop = extend[4])
  
  #Divide point cloud
  pcs <- clip_roi(pc, aoi)
  
  #Release memory
  rm(list = c("pc"))
  gc()
  
  #N of stands
  stands <- length(pcs)
  
  #Set up cluster
  #cl <- makeCluster(threads)
  cl <- makeCluster(threads, type = "FORK")
  registerDoParallel(cl)
  
  # Loop over scenes to estimate delta VI 
  frame <- foreach(i = 1:stands,
                   .combine = rbind,
                   .packages = c("lidR", "rTLS", "data.table", "moments", "sf", "sfheaders"),
                   .export = c("forest_fractal"),
                   .inorder = F) %dopar% {
                     
                     #Apply function
                     metrics <- forest_fractal(point_cloud = pcs[[i]],
                                               z_min = 0.25)
                     
                     #Add id
                     metrics$id <- aoi$id[i]
                     
                     #Export
                     return(metrics)
                     
                   }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
  #Order files
  col <- c(ncol(frame), (1:(ncol(frame)-1)))
  frame <- frame[, .SD, .SDcols=c(col)]
  
  #Export
  fwrite(frame, paste0(output_name, ".txt"), sep = "\t")
  
  #Get metrics
  stands <- merge(limits_gpkg, frame, by = "id")
  st_write(stands, dsn = paste0(output_name, ".gpkg"), driver = "GPKG")
  
  #Return 
  return(frame)
  
}

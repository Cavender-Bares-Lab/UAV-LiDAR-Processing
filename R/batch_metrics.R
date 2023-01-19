################################################################################
#' @title Batch stand and fractal metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/stand_metrics.R")
source("R/stand_fractal.R")
source("R/rotate_stand.R")

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
#' @param threads Number of threads for parallel processing

root_path <- "F:/LiDAR/L4/FAB1"
#root_path <- "/home/antonio/Documents"
path_pc <- paste0(root_path, "/", "2022-04-10_FAB1_normalized.las")
path_gpkg <- "data/less/FAB1_less.gpkg"
output_name <- paste0(root_path, "/", "2022-04-10_FAB1_stand")
threads <- 4

#' -----------------------------------------------------------------------------
#' Function

batch_stand <- function(path_pc, path_gpkg, output_name, threads) {
  
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
  complete <- foreach(i = 1:stands,
                      .combine= frame_combine,
                      .packages = c("lidR", "data.table", "moments", "sf", "sfheaders", "rTLS"),
                      .export = c("stand_metrics", "shannon", "rotate_stand", "stand_fractal"),
                      .inorder = F) %do% {
                         
                         if(!is.null(pcs[[i]])) {
                           
                           #Select point cloud
                           pc <- pcs[[i]]
                           
                           #Apply filter
                           pc <- decimate_points(pc, random_per_voxel(0.01, 1))
                           
                           # Stands metrics ------------------------------------
                           # Apply function
                           metrics <- stand_metrics(point_cloud = pc, 
                                                    k = 1, 
                                                    z_res = 0.1, 
                                                    z_min = 0.25, 
                                                    z_max = 12)
                           
                           # Fractal metrics------------------------------------
                           #The boundary box
                           limits <- limits_gpkg[i, ]
                           limits <- st_bbox(limits$geom)
                           
                           #Rotate stand
                           pc_rotate <- rotate_stand(pc, limits)
                           
                           #Get fractal 
                           fractal <- stand_fractal(pc_rotate)
                           
                           # Merge
                           results <- data.table(Plot = limits_gpkg$Plot[i])
                           results <- cbind(results, fractal, metrics) 
                           
                           return(results)
                           
                         }
                       }
  
  #Stop cluster
  stopCluster(cl)
  gc()
  
  #Fill 0
  setnafill(complete, fill = 0)   
  
  #Export
  fwrite(complete, output_name, sep = "\t")
  
}


#' -----------------------------------------------------------------------------
#' @examples 

# FAB1 ----------------------------------
root_path <- "F:/LiDAR/L4/FAB1"
path_gpkg <- "data/less/FAB1_less.gpkg"
threads <- 4

# 2022-04-10
path_pc <- paste0(root_path, "/", "2022-04-10_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-04-10_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-05-18
path_pc <- paste0(root_path, "/", "2022-05-18_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-05-18_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-06-12
path_pc <- paste0(root_path, "/", "2022-06-12_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-06-12_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-07-06
path_pc <- paste0(root_path, "/", "2022-07-06_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-07-06_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-08-03
path_pc <- paste0(root_path, "/", "2022-08-03_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-08-03_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-09-07
path_pc <- paste0(root_path, "/", "2022-09-07_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-09-07_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-09-18
path_pc <- paste0(root_path, "/", "2022-09-18_FAB1_normalized-master.las")
output_name <- paste0(root_path, "/", "2022-09-18_FAB1_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)


# FAB2 ----------------------------------
root_path <- "F:/LiDAR/L4/FAB2"
path_gpkg <- "data/less/FAB2_less.gpkg"
threads <- 4

# 2022-04-10
path_pc <- paste0(root_path, "/", "2022-04-10_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-04-10_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-05-18
path_pc <- paste0(root_path, "/", "2022-05-18_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-05-18_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-06-12
path_pc <- paste0(root_path, "/", "2022-06-12_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-06-12_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-07-06
path_pc <- paste0(root_path, "/", "2022-07-06_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-07-06_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-08-03
path_pc <- paste0(root_path, "/", "2022-08-03_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-08-03_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-09-07
path_pc <- paste0(root_path, "/", "2022-09-07_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-09-07_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)

# 2022-09-18
path_pc <- paste0(root_path, "/", "2022-09-18_FAB2_normalized.las")
output_name <- paste0(root_path, "/", "2022-09-18_FAB2_stand")
batch_stand(path_pc, path_gpkg, output_name, threads)
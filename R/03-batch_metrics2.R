################################################################################
#' @title Batch stand and fractal metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/vertical_metrics.R")
source("R/horizontal_metrics.R")
source("R/stand_fractal.R")

#' -----------------------------------------------------------------------------
#' Libraries
library(foreach)
library(doParallel)
library(lidR)
library(terra)
library(sf)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_pc Path to the point cloud
#' @param path_gpkg Path to the gpkg
#' @param z_correction Z correction based on direct comparisons.
#' @param output_name Path and name of the outputs
#' @param threads Number of threads for parallel processing

#root_path <- "F:/LiDAR/L4/FAB1"
root_path <- "/media/antonio/Extreme Pro/Projects/LiDAR/FAB2"
path_pc <- paste0(root_path, "/", "2022-05-18_FAB2_noise.laz")
path_gpkg <- "data/less/FAB2_less.gpkg"
output_name <- paste0(root_path, "/", "2022-05-18_FAB2_stand")
threads <- 6

#' -----------------------------------------------------------------------------
#' Function

batch_stand <- function(path_pc, path_gpkg, z_correction = 0, output_name, threads) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  pc <- readLAS(path_pc)
  
  #Remove noise
  pc <- filter_poi(pc, Classification != 7)
  
  #Read gpkp
  limits_gpkg <- st_read(dsn = path_gpkg)
  limits_gpkg <- limits_gpkg[order(limits_gpkg$Plot),]
  
  #Clip point cloud        
  extend <- st_bbox(limits_gpkg)
  
  #Rectangle for speed
  pcs <- clip_rectangle(pc, xleft = extend[1] - 0.25, 
                        ybottom = extend[2] - 0.25, 
                        xright = extend[3] + 0.25, 
                        ytop = extend[4] + 0.25)
  
  #Release memory
  rm(list = c("pc"))
  gc()
  
  #Divide point cloud
  #pcs <- clip_roi(pcs, limits_gpkg[,1])
  #saveRDS(pcs, paste0(output_name, ".rds"))
  #gc()
  
  #N of stands
  stands <- dim(limits_gpkg)[1]
  
  #Set threads to 1
  set_lidr_threads(1)
  
  # Loop over scenes to estimate delta VI
  get_metrics <- function(i, 
                          point_cloud, 
                          limits_gpkg,
                          output_name) {
    
    # Select the limits of interest
    limit <- limits_gpkg[i, ]
    Plot <- limit$Plot[1]
    
    # Crop the point cloud
    pc <- clip_roi(point_cloud, limit)
                        
    #Remove large angles and potential noise
    pc <- filter_poi(pc, ScanAngleRank >= -15 &
                         ScanAngleRank <= 15)
                          
    pc <- classify_noise(pc, sor(k = 50,
                                 m = 0.975,
                                 quantile = TRUE))
    pc <- filter_poi(pc, Classification != 18)
                          
    #Apply filter
    pc <- decimate_points(pc, random_per_voxel(0.01, 1))
                          
    # Logic to process
    size_above <- length(pc$Z[pc$Z > 0.25]) <= 10
    hight_above <- quantile(pc$Z[pc$Z > 0.25], 0.95) <= 0.5
    
    if(size_above == TRUE | hight_above == TRUE) {
      
      # Clean residuals
      rm(list = c("pc", "size_above", "hight_above", "limit", "Plot"))
      gc()
      
      #Return empty frame
      return(NA)
      
    } else {
      
      # Stands metrics ------------------------------------
      # Vertical function
      v_metrics <- vertical_metrics(point_cloud = pc, 
                                    k = 1, 
                                    z_res = 0.5, 
                                    z_min = 0.25, 
                                    z_max = 15)
      
      # Horizontal metrics
      h_metrics <- horizontal_metrics(point_cloud = pc)
      
      # 3D metrics 
      fractal <- stand_fractal(point_cloud = pc, z_min = 0.25)
      
      # Merge
      results <- data.table(Plot = Plot,
                            area = area(pc),
                            density = density(pc))
      results <- cbind(results, fractal, v_metrics, h_metrics) 
      
      # Clean residuals
      rm(list = c("v_metrics", "h_metrics", "fractal", 
                  "pc", "size_above", "hight_above"))
      gc()
      
      names_export <- paste0(output_name, "_", Plot, ".csv")
      fwrite(results, names_export)
      
      return(NA)
      
    }
  }

  # Parallel
  mclapply(1:stands, 
           FUN = get_metrics, 
           point_cloud = pcs, 
           limits_gpkg = limits_gpkg,
           output_name = output_name,
           mc.cores = threads,
           mc.preschedule = FALSE,
           mc.cleanup = FALSE)
  
}

#' -----------------------------------------------------------------------------
#' @examples 

# FAB1 ----------------------------------
root_path <- "/media/antonio/Extreme SSD/LiDAR/L4/FAB1"
path_gpkg <- "data/less/FAB1_less.gpkg"
threads <- 14

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
root_path <- "/media/antonio/Extreme Pro/Projects/LiDAR/FAB2"
path_gpkg <- "data/less/FAB2_less.gpkg"
threads <- 6

# 2022-04-10
path_pc <- paste0(root_path, "/", "2022-04-10_FAB1-2_noise.laz")
output_name <- paste0(root_path, "/", "2022-04-10_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)

# 2022-05-18
path_pc <- paste0(root_path, "/", "2022-05-18_FAB2_noise.laz")
output_name <- paste0(root_path, "/", "2022-05-18_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)

# 2022-06-12
path_pc <- paste0(root_path, "/", "2022-06-12_FAB2_noise.laz")
output_name <- paste0(root_path, "/", "2022-06-12_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)

# 2022-07-06
path_pc <- paste0(root_path, "/", "2022-07-06_FAB2_noise.laz")
output_name <- paste0(root_path, "/", "2022-07-06_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)

# 2022-08-03
path_pc <- paste0(root_path, "/", "2022-08-03_FAB2_noise.laz")
output_name <- paste0(root_path, "/", "2022-08-03_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)

# 2022-09-07
path_pc <- paste0(root_path, "/", "2022-09-07_FAB2_noise.laz")
output_name <- paste0(root_path, "/", "2022-09-07_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)

# 2022-09-18
path_pc <- paste0(root_path, "/", "2022-09-18_FAB2_noise.laz")
output_name <- paste0(root_path, "/", "2022-09-18_FAB2_stand")
batch_stand(path_pc, path_gpkg, z_correction = 0, output_name, threads)
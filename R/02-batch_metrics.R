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
library(pbmcapply)
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

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
path_pc <- paste0(root_path, "/L2/", "2022-04-10_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-04-10_FAB2.gpkg")
output_name <- paste0(root_path, "/", "2022-04-10_FAB2_results")
threads <- 6

#' -----------------------------------------------------------------------------
#' Function

batch_stand <- function(path_pc, path_gpkg, z_correction = 0, output_name, threads) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud remove noise and large scan angles
  pc <- readLAS(path_pc, 
                select = "xyz",
                filter = "-keep_scan_angle -15 15 -keep_class 1 2 8")
  
  #Read gpkp
  limits_gpkg <- st_read(dsn = path_gpkg)
  limits_gpkg <- limits_gpkg[order(limits_gpkg$Plot),]
  
  #Clip point cloud        
  extend <- st_bbox(limits_gpkg)
  
  #Rectangle for speed
  pc <- clip_rectangle(pc, xleft = extend[1] - 0.25, 
                        ybottom = extend[2] - 0.25, 
                        xright = extend[3] + 0.25, 
                        ytop = extend[4] + 0.25)
  
  #Release memory
  gc()
  
  #N of stands
  stands <- dim(limits_gpkg)[1]
  
  #Set threads to 1
  set_lidr_threads(1)
  
  # Loop over scenes to estimate delta VI
  get_metrics <- function(X, 
                          point_cloud, 
                          limits_gpkg,
                          output_name) {
    
    # Select the limits of interest
    limit <- limits_gpkg[X, ]
    Plot <- limit$Plot[1]
    limit <- st_buffer(limit, -0.5)
    
    # Crop the point cloud
    plot_pc <- clip_roi(point_cloud, limit)
    
    #Remove potential noise
    plot_pc <- classify_noise(plot_pc, sor(k = 50,
                                           m = 0.975,
                                           quantile = TRUE))
    plot_pc <- filter_poi(plot_pc, Classification != 18)
                          
    #Apply filter
    plot_pc <- decimate_points(plot_pc, random_per_voxel(0.005, 1))
    
    #Point cloud info
    points <- plot_pc@header$`Number of point records`
    area <- st_area(limit)
    
    # Logic to process
    size_above <- length(plot_pc$Z[plot_pc$Z > 0.25]) <= 10
    hight_above <- quantile(plot_pc$Z[plot_pc$Z > 0.25], 0.95) <= 0.5
    
    if(size_above == TRUE | hight_above == TRUE) {
      
      # Clean residuals
      rm(list = c("plot_pc", "size_above", "hight_above", 
                  "limit", "Plot", "points", "area"))
      gc()
      
      #Return empty frame
      return(NA)
      
    } else {
      
      # Stands metrics ------------------------------------
      # Vertical function
      v_metrics <- vertical_metrics(point_cloud = plot_pc, 
                                    k = 1, 
                                    z_res = 0.25, 
                                    z_min = 0.25, 
                                    z_max = 10)
      
      # Horizontal metrics
      h_metrics <- horizontal_metrics(point_cloud = plot_pc,
                                      xy_res = 0.25)
      
      # 3D metrics 
      fractal <- stand_fractal(point_cloud = plot_pc, z_min = 0.25)
      
      # Merge
      results <- data.table(Plot = Plot,
                            area = as.numeric(area),
                            npoints = points,
                            density = points/as.numeric(area))
      results <- cbind(results, fractal, v_metrics, h_metrics) 
      
      # Clean residuals
      rm(list = c("v_metrics", "h_metrics", "fractal", 
                  "plot_pc", "size_above", "hight_above", 
                  "limit", "Plot", "points", "area"))
      gc()
      
      names_export <- paste0(output_name, "_", Plot, ".csv")
      fwrite(results, names_export)
      
      return(NA)
      
    }
  }

  # Parallel
  pbmclapply(1:stands, 
             FUN = get_metrics, 
             point_cloud = pc, 
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
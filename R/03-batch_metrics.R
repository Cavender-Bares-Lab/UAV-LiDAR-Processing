################################################################################
#' @title Batch stand and fractal metrics 
################################################################################

#' @description Get the stand metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Load source code
source("R/vertical_metrics.R")
source("R/horizontal_metrics.R")
source("R/summary_voxels.R")
source("R/fractal_metrics.R")

#' -----------------------------------------------------------------------------
#' Libraries
library(pbmcapply)
library(lidR)
library(terra)
library(sf)
library(foreach)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_pc Path to the point cloud
#' @param path_gpkg Path to the gpkg
#' @param output_name Path and name of the outputs
#' @param threads Number of threads for parallel processing

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"
path_pc <- paste0(root_path, "/L2/", "2022-04-10_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-04-10_FAB2.gpkg")
output_name <- paste0(root_path, "/", "2022-04-10_FAB2_results")
threads <- 28

#' -----------------------------------------------------------------------------
#' Function

batch_metrics <- function(path_pc, path_gpkg, output_name, threads) {
  
  #Set number of threads to use
  set_lidr_threads(30)
  
  #Read point cloud remove noise and large scan angles
  pc <- readLAS(path_pc, 
                select = "xyzcnr",
                filter = "-keep_class 1 2 8")
  
  #Read gpkp
  limits_gpkg <- st_read(dsn = path_gpkg)
  limits_gpkg <- subset(limits_gpkg, Area != "GCP")
  limits_gpkg <- limits_gpkg[order(limits_gpkg$Plot),]
  
  #Clip point cloud        
  extend <- st_bbox(limits_gpkg)
  
  #Rectangle for speed
  pc <- clip_rectangle(pc, 
                       xleft = extend[1],
                       ybottom = extend[2],
                       xright = extend[3],
                       ytop = extend[4])
  
  #Release memory
  gc()
  
  #N of stands
  stands <- dim(limits_gpkg)[1]
  
  #Set threads to 1
  set_lidr_threads(1)
  
  # Loop over scenes to estimate delta VI
  get_metrics <- function(X, 
                          pc, 
                          limits_gpkg,
                          output_name) {
    
    # Select the limits of interest
    limit <- limits_gpkg[X, ]
    Plot <- limit$Plot[1]
    plot_new <- limit$plot_new[1]
    Area <- limit$Area[1]
    limit <- st_buffer(limit, -0.5)
    
    # Crop the point cloud
    plot_pc <- clip_roi(pc, limit)
    
    # Use vegetation points and remove noise
    vegetation_pc <- filter_poi(plot_pc, Classification == 1)
    vegetation_pc <- filter_poi(vegetation_pc, Z >= 0.25)
    vegetation_pc <- classify_noise(vegetation_pc, sor(k = 30,
                                                       m = 0.98,
                                                       quantile = TRUE))
    vegetation_pc <- decimate_points(vegetation_pc, random_per_voxel(0.01, 1))
    
    # Get height threshold
    max_points <- quantile(vegetation_pc$Z, 0.999)
    
    # Logic to process
    size_above <- max_points < 1.5
    npoints <- vegetation_pc@header$`Number of point records` < 1000
    
    if(size_above == TRUE | npoints == TRUE) {
      
      # Clean residuals
      #rm(list = c("limit", "Plot", "Area", "limit", "plot_pc", 
      #            "vegetation_pc", "max_points", "size_above",
      #            "npoints"))
      gc()
      
      #Return empty frame
      return(print(paste0("Plot ", plot_new, " is not higher than 1.5 m")))
      
    } else {
      
      total_points <- plot_pc@header$`Number of point records`
      
      #-------------------------------------------------------------------------
      # Gap fraction
      Npulses <- plot_pc@header$`Number of points by return`[1]
      Wz <- sum(1/filter_poi(plot_pc, Z >= 0.25)$NumberOfReturns)
      FC <- Wz/Npulses
      
      #-------------------------------------------------------------------------
      # Horizontal metrics
      
      #Ground pc
      ground_pc <- filter_poi(plot_pc, Z < 0.25)
      rm(list = c("plot_pc"))
      ground_pc <- decimate_points(ground_pc, random_per_voxel(0.01, 1))
      
      # Horizontal metrics
      h_metrics <- horizontal_metrics(point_cloud = rbind(ground_pc, vegetation_pc),
                                      xy_res = 0.1)
      rm(list = c("ground_pc"))
      
      #-------------------------------------------------------------------------
      # Vectical Metrics
      
      # Vertical function
      v_metrics <- vertical_metrics(point_cloud = vegetation_pc, 
                                    z_res = 0.25, 
                                    z_min = 0.25, 
                                    z_max = 10)
      
      #-------------------------------------------------------------------------
      # 3D metrics 
      fractal <- fractal_metrics(point_cloud = vegetation_pc, 
                                 z_min = 0.25)
      
      #-------------------------------------------------------------------------
      # Results
      
      # Merge
      results <- data.table(Plot = Plot,
                            plot_new = plot_new,
                            Type = Area,
                            area = as.numeric(st_area(limit)),
                            npoints = total_points,
                            density = total_points/as.numeric(st_area(limit)),
                            FC = FC)
      
      results <- cbind(results, v_metrics, h_metrics, fractal) 
      results <- cbind(results, h_metrics) 
      
      names_export <- paste0(output_name, "_plot_", plot_new, ".csv")
      fwrite(results, names_export)
      
      # Clean residuals
      #rm(list = c("limit", "Plot", "Area", "vegetation_pc", "max_points",
      #            "size_above", "total_points", "FC", "Npulses", "Wz", 
      #            "h_metrics", "v_metrics", "fractal", "results", "names_export"))
      gc()
      
      return(print(paste0("Plot ", plot_new, " was successfully processed")))
      
    }
  }
  
  # Parallel
  pbmclapply(140:stands, 
             FUN = get_metrics, 
             pc = pc, 
             limits_gpkg = limits_gpkg,
             output_name = output_name,
             mc.cores = threads,
             mc.preschedule = FALSE,
             mc.cleanup = FALSE)
  
}

#' -----------------------------------------------------------------------------
#' @examples 

# FAB2 ----------------------------------
#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"
threads <- 16

# 2022-04-10
path_pc <- paste0(root_path, "/L2/", "2022-04-10_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-04-10_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-04-10_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-05-18
path_pc <- paste0(root_path, "/L2/", "2022-05-18_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-05-18_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-05-18_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-06-12
path_pc <- paste0(root_path, "/L2/", "2022-06-12_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-06-12_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-06-12_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-07-06
path_pc <- paste0(root_path, "/L2/", "2022-07-06_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-07-06_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-07-06_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-08-03
path_pc <- paste0(root_path, "/L2/", "2022-08-03_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-08-03_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-08-03_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-09-07
path_pc <- paste0(root_path, "/L2/", "2022-09-07_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-09-07_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-09-07_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-09-18
path_pc <- paste0(root_path, "/L2/", "2022-09-18_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-09-18_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-09-18_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2022-10-24
path_pc <- paste0(root_path, "/L2/", "2022-10-24_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2022-10-24_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2022-10-24_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-04-13
path_pc <- paste0(root_path, "/L2/", "2023-04-13_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-04-13_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-04-13_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-04-26
path_pc <- paste0(root_path, "/L2/", "2023-04-26_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-04-26_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-04-26_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-05-26
path_pc <- paste0(root_path, "/L2/", "2023-05-26_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-05-26_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-05-26_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-07-29
path_pc <- paste0(root_path, "/L2/", "2023-07-29_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-07-29_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-07-29_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-08-22
path_pc <- paste0(root_path, "/L2/", "2023-08-22_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-08-22_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-08-22_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-09-12
path_pc <- paste0(root_path, "/L2/", "2023-09-12_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-09-12_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-09-12_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

# 2023-10-15
path_pc <- paste0(root_path, "/L2/", "2023-10-15_FAB2.laz")
path_gpkg <- paste0(root_path, "/GIS/", "2023-10-15_FAB2.gpkg")
output_name <- paste0(root_path, "/FSC/", "2023-10-15_FAB2_FSC_")
batch_metrics(path_pc, path_gpkg, output_name, threads)

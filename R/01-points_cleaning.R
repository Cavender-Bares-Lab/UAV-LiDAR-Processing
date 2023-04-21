################################################################################
#' @title Point cloud cleaning 
################################################################################

#' @description Cleaning of points of the cloud. It first remove the points 
#' outside the boundary layer of interest and then perform the classification 
#' of noise, duplicate and ground points. Noise points are not removed.

#' @return A point cloud of the area of interest.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(rlas)
library(RCSF)
library(terra)
library(sf)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param input_file Path and name of the .las file to process.
#' @param output_name Path and name (without extension) of files to export.
#' @param resolution Resolution of the digital models.
#' @param clip_path A path and name of a .kml of boundaries to crop.
#' @param threads  An integer of the number of threads to use.


#' -----------------------------------------------------------------------------
#' @example

#input_file <- "data/2022-04-10_FAB.las"
input_file <- "/home/antonio/Documents/LiDAR/L2/2022-04-10_FAB1-2.las"
output_name <- "/home/antonio/Documents/LiDAR/L3/2022-04-10_FAB2"
resolution <- 0.1
clip_path <- "data/large_boundaries/FAB2_large.gpkg"
threads <- 28

points_cleaning(input_file, output_name, resolution = 0.10, clip_path, threads = 26)

#' @references 
# W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne
# LiDAR Data Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501,
# 2016. (http://www.mdpi.com/2072-4292/8/6/501/htm)

#' -----------------------------------------------------------------------------
#' Function

points_cleaning <- function(input_file, 
                            output_name, 
                            resolution = 1.0, 
                            clip_path = NULL, 
                            threads = 26) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  point_cloud <- readLAS(input_file)
  
  #Using clip
  if(is.null(clip_path) != TRUE) {
    
    #Read clipping polygon
    clip <- st_read(dsn = clip_path)
    
    #Get projection
    pc_CRS <- crs(point_cloud)
    vector <- crs(clip)
    
    #if(all.equal(pc_CRS, vector) == FALSE) {
    #  stop("Projections of point cloud and cliping vector does not match")
    #}
    
    #Create a buffer 4 times the resolution on the clipping polygon
    boundary <- st_union(st_buffer(clip, 1.0))
    
    #---------------------------------------------------------------------------
    # Point cloud processing
    
    #Clip point cloud        
    extend <- st_bbox(boundary)
    
    #Rectangle for speed
    pc <- clip_rectangle(point_cloud, xleft = extend[1], 
                         ybottom = extend[2], 
                         xright = extend[3], 
                         ytop = extend[4])
    
    #ROI for details
    pc <- clip_roi(pc, boundary)
    
    #Release memory
    rm(list = c("point_cloud"))
    gc()
    
  } else {
    
    pc <- point_cloud
    
    #Release memory
    rm(list = c("point_cloud"))
    gc()
    
  }
  
  #Remove points with high angles ---------------------------------------
  pc <- filter_poi(pc, ScanAngleRank >= -15 & ScanAngleRank <= 15)
  
  #Remove duplicate points  ---------------------------------------------
  pc <- filter_duplicates(pc)
  
  #Classify and filter noise points  ------------------------------------
  pc <- classify_noise(pc, 
                       algorithm = sor(k = 50, m = 0.95, quantile = TRUE))
  
  pc <- filter_poi(pc, Classification != LASNOISE)
  
  #Remove duplicate points  ---------------------------------------------
  pc <- decimate_points(pc, random_per_voxel(res = 0.01, n = 1))
  
  #Classify ground points  ----------------------------------------------
  pc <- classify_ground(pc, 
                        algorithm = csf(sloop_smooth = FALSE,
                                        class_threshold = 0.25,
                                        cloth_resolution = resolution,
                                        rigidness = 2L,
                                        iterations = 500L,
                                        time_step = 0.65),
                        last_returns = TRUE)
  
  #Export•
  pc_name <- paste0(output_name, "_clean.laz")
  writeLAS(pc, pc_name, index = FALSE)
  
}

points_cleaning(input_file = "/home/antonio/Documents/LiDAR/L2/2022-04-10_FAB1-2.las", 
                output_name = "/home/antonio/Documents/LiDAR/L3/2022-04-10_FAB2", 
                resolution = 1.0, 
                clip_path = "data/large_boundaries/FAB2_large.gpkg", 
                threads = 28)

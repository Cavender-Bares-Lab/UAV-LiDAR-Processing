################################################################################
#' @title Function to derive a Digital Models from a UAV - point cloud 
################################################################################

#' @description Derive a Digital Terrain Model (DTM), Digital Surface Model (DSM),
#' Canopy Height Model (CHM), and Density Map (DM) of a point cloud derived from 
#' UAV-LiDAR. The DTM uses a the Cloth Simulation Filtering algorithm from
#' Zhang et al (2016). The DSM and CHM use the points-to-raster method.
#' 
#' @return Several GeoTIFF and a normalized point cloud by height.
#' 
#' @references 
#' W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan. 2016. “An 
#' Easy-to-Use Airborne LiDAR Data Filtering Method Based on Cloth Simulation,
#' ” Remote Sens., vol. 8, no. 6, p. 501. 
#' (http://www.mdpi.com/2072-4292/8/6/501/htm)

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
#' @param threads  An interger of the number of threads to use.

input_file <- "C:/Users/guzman/Documents/temp/2022-04-10_FAB.las"
output_name <- "data/2022-04-10_FAB"
resolution <- 0.1
clip_path <- "data/FAB2_blocks_buffer.gpkg"
threads <- 25

#' -----------------------------------------------------------------------------
#' Function

get_digital_models <- function(input_file, output_name, resolution = 0.10, threads = 26) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  point_cloud <- readLAS(input_file)
  
  #Classify ground points
  point_cloud <- classify_ground(point_cloud, 
                                 algorithm = csf(sloop_smooth = FALSE,
                                                 class_threshold = 0.2,
                                                 cloth_resolution = 0.2,
                                                 rigidness = 3L,
                                                 iterations = 500L,
                                                 time_step = 0.65))
  
  #Get projection
  CRS <- crs(point_cloud)
  
  #Clipping vector read and project
  clip <- st_read(dsn = clip_path)
  clip <- st_transform(clip, CRS)
  
  #Digital Terrain Model  ---------------------------------------------
  dtm <- rasterize_terrain(las = point_cloud, 
                           res = resolution,
                           algorithm = knnidw(k = 10L, p = 2, rmax = 0.2))
  
  #dtm <- crop(dtm, clip)
  dtm_name <- paste0(output_name, "_DTM.tif")
  writeRaster(dtm, dtm_name, overwrite=TRUE)
  
  #Digital Surface Model  ---------------------------------------------
  dsm <- rasterize_canopy(las = point_cloud, 
                          res = resolution, 
                          algorithm = p2r(resolution, na.fill = tin()))
  
  #dsm <- crop(dsm, clip)
  dsm_name <- paste0(output_name, "_DSM.tif")
  writeRaster(dsm, dsm_name, overwrite=TRUE)
  
  #Normalize height ---------------------------------------------------
  point_cloud_normalized <- normalize_height(point_cloud, 
                                             knnidw(k = 10L, p = 2, rmax = 1))
  
  normalized_name <- paste0(output_name, "_normalized.las")
  lasheader <- header_create(point_cloud_normalized)
  writeLAS(point_cloud_normalized, normalized_name, index = FALSE)
  
  
  #Canopy Height Model -----------------------------------------------
  chm <- rasterize_canopy(point_cloud_normalized, 
                          res = resolution, 
                          algorithm = p2r(resolution/2, na.fill = tin()))
  
  #chm <- crop(chm, clip)
  chm_name <- paste0(output_name, "_CHM.tif")
  writeRaster(chm, chm_name, overwrite=TRUE)
  
  #Density map --------------------------------------------------------
  dm <- rasterize_density(point_cloud, 
                          res = resolution)
  
  #dm <- crop(dm, clip)
  dm_name <- paste0(output_name, "_DM.tif")
  writeRaster(dm, dm_name, overwrite=TRUE)
  
}
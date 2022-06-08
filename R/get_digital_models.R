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
library(geometry)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param input_file Path and name of the .las file to process.
#' @param output_name Path and name (without extension) of files to export.
#' @param resolution Resolution of the digital models.
#' @param DTM Use the a predefine DTM. If NULL it creates a new one. 
#' @param clip_path A path and name of a .kml of boundaries to crop.
#' @param threads  An integer of the number of threads to use.

#input_file <- "data/2022-04-10_FAB.las"
input_file <- "Z:/9-UAV/LiDAR/2022-05-18_FAB2/L2/2022-05-18_FAB2.las"
#output_name <- "data/2022-04-10_FAB2"
output_name <- "Z:/9-UAV/LiDAR/2022-05-18_FAB2/L3/2022-05-18_FAB2"
resolution <- 0.1
DTM <- "Z:/9-UAV/LiDAR/2022-04-10_FAB1-2/L3/FAB2/2022-04-10_FAB2_DTM.tif"
clip_path <- "data/FAB2_blocks_buffer.gpkg"
threads <- 6

#' -----------------------------------------------------------------------------
#' Function

get_digital_models <- function(input_file, 
                               output_name, 
                               resolution = 0.10, 
                               DTM = NULL, 
                               clip_path, 
                               threads = 26) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  pc <- readLAS(input_file)
  
  #Read clipping polygon
  clip <- st_read(dsn = clip_path)
  clip_terra <- vect(clip_path)
  
  #Get projection
  pc_CRS <- crs(pc)
  vector <- crs(clip)
  
  if(all.equal(pc_CRS, vector) == FALSE) {
    stop("Projections of point cloud and cliping vector does not match")
  }
  
  if(is.null(DTM)) {
    
    #Digital Terrain Model  ------------------------------------------
    dtm <- rasterize_terrain(las = pc, 
                             res = resolution,
                             algorithm = tin())
    
    dtm <- terra::crop(dtm, clip_terra, mask = TRUE)
    dtm_name <- paste0(output_name, "_DTM.tif")
    writeRaster(dtm, dtm_name, overwrite=TRUE)
    
    #Release memory
    rm(list = c("dtm_name"))
    gc()
    
  } else {
    dtm <- rast(DTM)
  }
  
  #Filter points ------------------------------------------------------
  pc <- filter_poi(pc, Classification != LASNOISE)
  
  #Digital Surface Model  ---------------------------------------------
  dsm <- rasterize_canopy(las = pc,
                          res = resolution,
                          algorithm = p2r(resolution, na.fill = tin()))
  
  dsm <- terra::crop(dsm, clip_terra, mask = TRUE)
  dsm_name <- paste0(output_name, "_DSM.tif")
  writeRaster(dsm, dsm_name, overwrite=TRUE)
  
  #Release memory
  rm(list = c("dsm", "dsm_name"))
  gc()
  
  #Normalize height ---------------------------------------------------
  pc_normalized <- pc - dtm
  
  normalized_name <- paste0(output_name, "_normalized.las")
  writeLAS(pc_normalized, normalized_name, index = FALSE)
  
  #Release memory
  rm(list = c("normalized_name", "pc"))
  gc()
  
  #Canopy Height Model -----------------------------------------------
  chm <- rasterize_canopy(pc_normalized, 
                          res = resolution, 
                          algorithm = p2r(resolution, na.fill = tin()))
  
  chm <- terra::crop(chm, clip_terra, mask = TRUE)
  chm_name <- paste0(output_name, "_CHM.tif")
  writeRaster(chm, chm_name, overwrite=TRUE)
  
  #Release memory
  rm(list = c("chm", "chm_name"))
  gc()
  
  #Density map --------------------------------------------------------
  dm <- rasterize_density(pc_normalized, 
                          res = resolution)
  
  dm <- terra::crop(dm, clip, mask = TRUE)
  dm_name <- paste0(output_name, "_DM.tif")
  writeRaster(dm, dm_name, overwrite=TRUE)
  
  #Release memory
  rm(list = c("dm", "dm_name"))
  gc()
  
}

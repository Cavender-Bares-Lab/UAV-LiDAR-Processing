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
#' @param DTM Use the a pre-define DTM. If NULL it creates a new one. 
#' @param buffer_path A path and name of a .gpkg of boundaries to crop the plots of interest (final area).
#' @param threads An integer of the number of threads to use.

input_file <- "/home/antonio/FAB2/2022-04-10_FAB2_clean.laz"
output_name <- "/home/antonio/FAB2/2022-04-10_FAB2.las"
resolution <- 0.1
DTM <- "/media/antonio/Work/FAB2/LiDAR/DTM/FAB2_DTM_master.tif"
extend <- c(485153.182, 5028102.4825, 485160.531, 5028107.804)
buffer_path <- "data/buffer/FAB2_buffer.gpkg"
threads <- 26

#' -----------------------------------------------------------------------------
#' Function

get_digital_models <- function(input_file, 
                               output_name, 
                               resolution = 0.10, 
                               DTM = NULL, 
                               extend, 
                               buffer_path,
                               threads = 26) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  pc <- readLAS(input_file)

  #Filter noise points 
  pc <- filter_poi(pc, Classification != 18L)
  
  #Read clipping polygon
  buffer <- st_read(dsn = buffer_path)
  buffer_terra <- vect(buffer_path)
  
  #Get projection
  pc_CRS <- crs(pc)
  vector <- crs(buffer)
  
  #if(all.equal(pc_CRS, vector) == FALSE) {
  #  stop("Projections of point cloud and cliping vector does not match")
  #}
  
  #Create and compare DTMs -----------------------------------------------------
  if(is.null(DTM) != TRUE) {
    
    #Rectangle for ground section
    pc_ground <- clip_rectangle(pc, xleft = extend[1], 
                         ybottom = extend[2], 
                         xright = extend[3], 
                         ytop = extend[4])
    
    #DTM
    dtm_ground <- rasterize_terrain(las = pc_ground, 
                                    res = resolution,
                                    algorithm = knnidw(k = 50, p = 2, rmax = 0.5))

    #Get master DTM
    dtm <- rast(DTM)
    dtm_ground_master <- crop(dtm, dtm_ground)
    
    #Get difference
    dtm_difference <- dtm_ground - dtm_ground_master
    dtm_difference <- round(quantile(dtm_difference[], 0.5, na.rm = TRUE), 3)
    
    #Transform pc
    pc$Z <- pc$Z - dtm_difference
    
    #Release memory
    rm(list = c("pc_ground", "dtm_ground", "dtm_ground_master", "dtm_difference"))
    gc()

  } 
  
  #Using clip  -----------------------------------------------------------------
  boundary <- st_union(st_buffer(buffer, resolution*4))

  #Clip point cloud
  extend <- st_bbox(boundary)
    
  #Rectangle for speed
  pc <- clip_rectangle(pc, xleft = extend[1], 
                           ybottom = extend[2], 
                           xright = extend[3], 
                           ytop = extend[4])
    
  #ROI for details
  pc <- clip_roi(pc, boundary)
    
  #Release memory
  rm(list = c("boundary", "extend"))
  gc()
  
  #Digital Terrain Model (if not master) ---------------------------------------
  if(is.null(DTM) == TRUE) {
    
    dtm <- rasterize_terrain(las = pc, 
                             res = resolution,
                             algorithm = knnidw(k = 50, p = 2, rmax = 0.5))
    
    dtm <- terra::crop(dtm, buffer_terra, mask = TRUE)
    dtm_name <- paste0(output_name, "_DTM.tif")
    writeRaster(dtm, dtm_name, overwrite=TRUE)
    
    #Release memory
    rm(list = c("dtm_name"))
    gc()
    
  }
  
  #Digital Surface Model  ------------------------------------------------------
  dsm <- rasterize_canopy(las = pc,
                          res = resolution,
                          algorithm = p2r(resolution, na.fill = tin()))
  
  dsm <- terra::crop(dsm, buffer_terra, mask = TRUE)
  dsm_name <- paste0(output_name, "_DSM.tif")
  writeRaster(dsm, dsm_name, overwrite=TRUE)
  
  #Release memory
  rm(list = c("dsm", "dsm_name"))
  gc()
  
  #Normalize height ------------------------------------------------------------
  pc_normalized <- pc - dtm
  
  normalized_name <- paste0(output_name, "_normalized.laz")
  writeLAS(pc_normalized, normalized_name, index = FALSE)
  
  #Release memory
  rm(list = c("normalized_name"))
  gc()
  
  #Canopy Height Model ---------------------------------------------------------
  chm <- rasterize_canopy(pc_normalized, 
                          res = resolution, 
                          algorithm = p2r(resolution, na.fill = tin()))
  
  chm <- terra::crop(chm, buffer_terra, mask = TRUE)
  chm_name <- paste0(output_name, "_CHM.tif")
  writeRaster(chm, chm_name, overwrite=TRUE)
  
  #Release memory
  rm(list = c("chm", "chm_name"))
  gc()
  
  #Density map -----------------------------------------------------------------
  dm <- rasterize_density(pc_normalized, 
                          res = resolution)
  
  dm <- terra::crop(dm, buffer_terra, mask = TRUE)
  dm_name <- paste0(output_name, "_DM.tif")
  writeRaster(dm, dm_name, overwrite=TRUE)
  
  #Release memory
  rm(list = c("pc_normalized", "dm", "dm_name"))
  gc()
  
}

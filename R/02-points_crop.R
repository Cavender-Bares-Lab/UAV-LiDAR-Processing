################################################################################
#' @title Crop small section
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

#' -----------------------------------------------------------------------------
#' Arguments
#' @param input_file Path and name of the .las file to process.
#' @param output_name Path and name (without extension) of files to export.
#' @param resolution Resolution of the digital models.
#' @param DTM Use the a predefine DTM. If NULL it creates a new one. 
#' @param clip_path A path and name of a .kml of boundaries to crop.
#' @param threads  An integer of the number of threads to use.

#input_file <- "data/2022-04-10_FAB.las"
input_file <- "/home/antonio/FAB2/2022-04-10_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-04-10_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26

points_crop(input_file, output_name, extend, threads = 26)

#' @references 
# W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne
# LiDAR Data Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501,
# 2016. (http://www.mdpi.com/2072-4292/8/6/501/htm)

#' -----------------------------------------------------------------------------
#' Function

points_crop <- function(input_file, output_name, extend, threads = 26) {
  
  #Set number of threads to use
  set_lidr_threads(threads)
  
  #Read point cloud
  point_cloud <- readLAS(input_file)
  
  #Rectangle for speed
  pc <- clip_rectangle(point_cloud, 
                       xleft = extend[1], 
                       ybottom = extend[2], 
                       xright = extend[3], 
                       ytop = extend[4])
  
  #Export•
  writeLAS(pc, output_name, index = FALSE)
  
}

input_file <- "/home/antonio/FAB2/2022-04-10_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-04-10_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-05-18_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-05-18_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-06-12_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-06-12_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-07-06_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-07-06_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-08-03_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-08-03_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-09-07_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-09-07_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-09-18_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-09-18_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

input_file <- "/home/antonio/FAB2/2022-04-10_FAB2_clean.las"
output_name <- "/home/antonio/FAB2/2022-04-10_FAB2_crop.las"
extend <- c(484972, 5028090, 485034, 5028114)
threads = 26
points_crop(input_file, output_name, extend, threads = 26)

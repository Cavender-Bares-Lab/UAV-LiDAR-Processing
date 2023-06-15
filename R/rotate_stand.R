################################################################################
#' @title Rotate point clouds of stands 
################################################################################

#' @description Rotate in a 2D plane point clouds of stands by identifying the 
#' extreme point in a corner.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(graphics)
library(rTLS)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param pc point cloud to rotate
#' @param limits 

#' -----------------------------------------------------------------------------
#' Function
rotate_stand <- function(point_cloud, limits) {
  
  #Rotation parameters ------------------------------
  corners <- as.data.table(st_coordinates(st_cast(limits, "MULTIPOINT")))
  corners <- unique(corners)
  
  #Get centroits and corners
  centroid <- c(mean(corners$X), 
                mean(corners$Y))
  corners <- subset(corners, X >= centroid[1] & Y >= centroid[2])
  corners <- corners[which.max(Y),] #Incase of duplicates
  
  #Get sites
  x <- corners$X[1] - centroid[1]
  y <- corners$Y[1] - centroid[2]
  hypothenuse <- sqrt((corners$X[1] - centroid[1])^2 + ((corners$Y[1] - centroid[2])^2))
  
  #Get angle
  angle <- acos((y/hypothenuse)) * 180/pi
  
  #Rotation angle
  rotation_angel <- 45 - angle
  
  #Get frame
  to_rotate <- data.table(X = point_cloud$X, Y = point_cloud$Y, Z = point_cloud$Z)
  
  #Rotate
  rotation <- rotate3D(to_rotate, roll = 0, pitch = 0, yaw = - rotation_angel)
  pc_rotated <- point_cloud
  pc_rotated@header@PHB[["X offset"]] <- floor(min(rotation$X))
  pc_rotated@header@PHB[["Y offset"]] <- floor(min(rotation$Y))
  
  pc_rotated$X <- rotation$X
  pc_rotated$Y <- rotation$Y
  
  return(pc_rotated)
  
}

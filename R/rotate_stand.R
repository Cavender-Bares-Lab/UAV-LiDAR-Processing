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
  
  #Get frame
  pc <- data.table(X = point_cloud$X, Y = point_cloud$Y, Z = point_cloud$Z)
  
  #Get distances
  centroid <- c(mean(limits[c(1, 3)]), 
                mean(limits[c(2, 4)]))
  hypothenuse <- sqrt( (limits[3] - centroid[1])^2 + ((limits[4] - centroid[2])^2))
  adjacent <- limits[3] - centroid[1]
  
  #Get angle
  angle <- acos((adjacent/hypothenuse)) * 180/pi
  
  #Rotation angle
  rotation_angel <- (45 - angle)
  
  #Rotate
  rotation <- rotate3D(pc, roll = 0, pitch = 0, yaw = rotation_angel)
  pc <- point_cloud
  pc@header@PHB[["X offset"]] <- floor(min(rotation$X))
  pc@header@PHB[["Y offset"]] <- floor(min(rotation$Y))
  
  pc$X <- rotation$X
  pc$Y <- rotation$Y
  
  return(pc)
  
}

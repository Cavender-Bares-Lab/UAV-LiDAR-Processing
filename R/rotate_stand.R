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

point_cloud <- readLAS("data/PIBA_1015.las")
#pc <- data.table(X = pc$X, Y = pc$Y, Z = pc$Z)

#' -----------------------------------------------------------------------------
#' Function
rotate_stand <- function(point_cloud) {
  
  pc <- data.table(X = point_cloud$X, Y = point_cloud$Y, Z = point_cloud$Z)
  
  #Convex hull
  ch <- chull(x = pc$X, y = pc$Y)
  
  #Get edge
  edge <- pc[ch]
  edge$Z <- 0
  
  #Get distance
  centroid <- c(mean(pc$X), mean(pc$Y), 0)
  dist <- euclidean_distance(point = c(mean(pc$X), mean(pc$Y), 0), edge, threads = 1L)
  max_dist <- as.numeric(edge[which.max(dist)])
  hypotenuse <- dist[which.max(dist)]
  
  #Determine the quadrant
  if(max_dist[1] < centroid[1] & max_dist[2] >= centroid[2]) { # I quadrant
    lateral <- centroid[1] - max_dist[1] 
    angle <- acos(lateral/hypotenuse) * (180/pi)
    
  } else if(max_dist[1] >= centroid[1] & max_dist[2] >= centroid[2]) { # II quadrant
    lateral <- max_dist[1] - centroid[1] 
    angle <- acos(lateral/hypotenuse) * (180/pi)
    
  } else if(max_dist[1] >= centroid[1] & max_dist[2] < centroid[2]) { # III quadrant
    lateral <- max_dist[1] - centroid[1]
    angle <- acos(lateral/hypotenuse) * (180/pi)
    
  } else if(max_dist[1] < centroid[1] & max_dist[2] < centroid[2]) { # IV quadrant
    lateral <- centroid[1] - max_dist[1]
    angle <- acos(lateral/hypotenuse) * (180/pi)
  }
  
  #Rotation angle
  if(angle >= 45) {
    rotation_angel <- angle - 45
  } else {
    rotation_angel <- 45 - angle 
  }
  
  #Stand rotation
  stand <- rotate2D(pc[, 1:2], angle = rotation_angel)
  
  #Approximate coordinate
  pc <- point_cloud
  pc$X <- stand$X
  pc$Y <- stand$Y
  
  return(pc)
  
}
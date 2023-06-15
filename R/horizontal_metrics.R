################################################################################
#' @title Horizontal metrics 
################################################################################

#' @description Get the vertical metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(moments)
library(sf)
library(sfheaders)
library(Lmoments)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param limits degrees to limit the field of view. If NULL it uses all the points.

#' -----------------------------------------------------------------------------
#' Function
horizontal_metrics <- function(point_cloud) {
  
  # Calculate grid metrics
  metrics <- pixel_metrics(point_cloud, ~f(Z), 0.25) 
  
  #Basic grid metrics
  sub_frame <- data.table(npixels = nrow(metrics$max[]),
                          h_height_max_mean = mean(metrics$max[], na.rm = TRUE),
                          h_height_sd_mean = sd(metrics$max[], na.rm = TRUE),
                          h_height_cv_mean = mean(metrics$cv[], na.rm = TRUE),
                          h_skwewness_mean = mean(metrics$skwewness[], na.rm = TRUE),
                          h_kurtosis_mean = mean(metrics$kurtosis[], na.rm = TRUE),
                          h_gini_mean = mean(metrics$gini[], na.rm = TRUE),
                          h_gini_sd = sd(metrics$gini[], na.rm = TRUE),
                          rumple = rumple_index(metrics$max))
  
  #Clean residuals
  rm(list = c("metrics"))
  
  ###"lad_profile", "gpag_profile", "profiles",
  
  gc()
  
  #Return
  return(sub_frame)
  
}

#Gini
gini <- function(Z, ground) {
  
  ## Valbuena R., Packalen P., Martín-Fernández S., Maltamo M. (2012) Diversity and 
  ## equitability ordering profiles applied to the study of forest structure. 
  ## Forest Ecology and Management 276: 185–195. \doi{10.1016/j.foreco.2012.03.036}
  
  z <- Z[Z > ground]
  
  n <- length(z)
  x <- sort(z)
  G <- 2 * sum(x * 1L:n)/sum(x) - (n + 1L)
  gc <- G/(n - 1L)
  
  return(gc)
  
}

# User-defined function
f <- function(x) { list(max = max(x),
                        cv = sd(x)/mean(x),
                        skwewness = skewness(x),
                        kurtosis = kurtosis(x),
                        gini = gini(x, ground = 0.25))
  }

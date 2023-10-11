################################################################################
#' @title Vertical metrics 
################################################################################

#' @description Get the vertical metrics of a segmented point cloud.

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lidR)
library(moments)
library(sfheaders)

#' -----------------------------------------------------------------------------
#' Arguments
#' @param point_cloud the normalized and rotated point cloud.
#' @param k the coefficient of extinction.
#' @param xy_res vertical resolution.
#' @param z_res vertical resolution.
#' @param z_min threshold for understory or floor points.
#' @param z_max maximum height, useful for stacking profiles with different elevations. If NULL
#' it uses the point with the heights elevation.
#' @param limit_fOV degrees to limit the field of view. If NULL it uses all the points.

#' -----------------------------------------------------------------------------
#' Function
vertical_metrics <- function(point_cloud, 
                             k = 1, 
                             z_res = 0.25,
                             z_min = 0.25,
                             z_max = 10) {
  
  #Create point cloud
  pc <- data.table(X = point_cloud$X,
                   Y = point_cloud$Y,
                   Z = point_cloud$Z)
  
  #Above points
  z_above <- pc[Z >= z_min, Z]
  
  #Basic grid metrics
  sub_frame <- data.table(height = quantile(pc$Z, 0.99),
                          height_cv = sd(z_above)/mean(z_above),
                          skwewness = skewness(pc$Z),
                          kurtosis = kurtosis(pc$Z))
  
  #Vertical profiles
  #LAD <- LAD(pc$Z, dz = z_res, k = 1.0, z0 = z_min)
  #gpag <- gap_fraction_profile(pc$Z, dz = z_res, z0 = z_min)
  #LAD$z <- round(LAD$z, 1)
  #gpag$z <- round(gpag$z, 1)

  #Metrics from vertical profiles
  #sub_frame$nprofiles <- nrow(LAD)
  #sub_frame$LAIe <- sum(LAD$lad)
  
  sub_frame$SEI_vertical <- entropy(z_above, by = z_res, zmax = max(z_above))
  sub_frame$FDH_vertical <- FDH_vertical(z_above, by = z_res)
  sub_frame$VCI <- VCI(z_above, zmax = 10, by = z_res)
  sub_frame$gini <- gini(Z = pc$Z, ground = z_min)
  
  
  #Clean residuals
  rm(list = c("pc", "z_above")) #"LAD"
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

FDH_vertical <- function(z, by = 0.25) {
  
  # Get max
  zmax = max(z)
  
  # Define the number of x meters bins from 0 to zmax (rounded to the next integer)
  bk <- seq(0.25, ceiling(zmax/by)*by, by)
  
  # Compute the p for each bin
  hist <- hist(z, breaks = bk, plot = F)$count
  hist <- hist/sum(hist)
  
  FDH <- -sum(hist*log10(hist)) / log10(zmax)
  
  return(FDH)
}

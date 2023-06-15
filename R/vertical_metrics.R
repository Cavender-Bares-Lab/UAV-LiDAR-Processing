################################################################################
#' @title Vertical metrics 
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
                             z_res = 0.1,
                             z_min = 0.25,
                             z_max = 15,
                             limit_fOV = NULL) {
  
  #Create point cloud
  pc <- data.table(X = point_cloud$X,
                   Y = point_cloud$Y,
                   Z = point_cloud$Z,
                   ScanAngleRank = point_cloud$ScanAngleRank)
  
  #Select specific field of view
  if(is.null(limit_fOV) != TRUE) {
    pc <- subset(pc, ScanAngleRank <= limit_fOV & ScanAngleRank >= -limit_fOV)
  }
  
  #Just col of interest
  pc <- pc[, 1:3]
  
  #Limits
  x_range <- range(pc$X)
  y_range <- range(pc$Y)
  z_max <- ifelse(is.null(z_max), max(pc$Z), z_max)
  
  #Remove zmax 
  limit <- quantile(pc$Z[pc$Z > z_min], 0.99)
  pc <- subset(pc, Z <= limit)
  
  #Sequences
  z_seq <- round(seq((z_min+z_res/2), (z_max-z_res/2), by = z_res), 1)
  zframe <- data.table(z = z_seq)
  
  #Remove ground
  z_above <- pc$Z[pc$Z >= z_min]
  
  #Frame to complete
  sub_complete <- data.table()
  
  #Basic grid metrics
  sub_frame <- data.table(npoints = length(z_above),
                          height_max = max(pc$Z),
                          height_mean = mean(pc$Z),
                          height_cv = sd(z_above)/mean(z_above),
                          skwewness = skewness(z_above),
                          kurtosis = kurtosis(z_above))
  
  #Vertical profiles
  LAD <- LAD(pc$Z, dz = z_res, k = 1.0, z0 = z_min)
  gpag <- gap_fraction_profile(pc$Z, dz = z_res, z0 = z_min)
  LAD$z <- round(LAD$z, 1)
  gpag$z <- round(gpag$z, 1)
  
  #Metrics from vertical profiles
  sub_frame$n_profiles <- nrow(LAD)
  sub_frame$LAIe <- sum(LAD$lad)
  sub_frame$vci <- VCI(z_above, sub_frame$height_max, by = z_res)
  sub_frame$entropy <- entropy(z_above, by = z_res, zmax = sub_frame$height_max)
  sub_frame$H <- shannon(LAD$lad + 1)
  sub_frame$Hmax <- shannon(rep(1, nrow(LAD))) #H max
  sub_frame$equitavility <- sub_frame$H/sub_frame$Hmax
  sub_frame$negentropy <- sub_frame$Hmax - sub_frame$H
  sub_frame$gini <- gini(Z = pc$Z, ground = z_min)
  sub_frame$FDH <- - sum(LAD$lad * log(LAD$lad)) / log(length(LAD$z))
  
  #Get L-moments
  lmom <- as.numeric(Lmoments(pc$Z[pc$Z > z_min]))
  sub_frame$Lskew <- lmom[3] / lmom[2]
  sub_frame$Lkurt <- lmom[4] / lmom[2]
  sub_frame$Lcoefvar <- lmom[2] / lmom[1]
  
  #Cx, Cy, RG
  lad <- data.table(z = 0, lad = 0)
  lad_max <- data.table(z = (max(LAD$z) + z_res), lad = 0)
  lad <- rbind(lad, LAD)
  lad <- rbind(lad, lad_max)
  
  sub_frame$Cx <- mean(lad$lad)
  sub_frame$Cy <- mean(lad$z)
  sub_frame$TopRG <- sum((lad$lad - sub_frame$Cx)^2)+sum((lad$z - sub_frame$Cy)^2)
  sub_frame$RG <- sqrt(sub_frame$TopRG/length(LAD$lad))
  
  #Profile manage for stacking
  #lad_profile <- merge(zframe, LAD, by = "z", all.x = TRUE, all.y = TRUE)
  #lad_profile <- as.data.table(t(lad_profile)[1:2,])
  #colnames(lad_profile) <- paste0("LAD_", as.character(z_seq))
  #lad_profile <- lad_profile[2,]
  #setnafill(lad_profile, fill = 0)
  
  #gpag_profile <- merge(zframe, gpag, by = "z", all.x = TRUE, all.y = TRUE)
  #gpag_profile <- as.data.table(t(gpag_profile)[1:2,])
  #colnames(gpag_profile) <- paste0("Pgap_", as.character(z_seq))
  #gpag_profile <- gpag_profile[2,]
  #setnafill(gpag_profile, fill = 1)
  
  #Stack results
  #profiles <- cbind(lad_profile, gpag_profile)
  #sub_frame <- cbind(sub_frame, profiles)
  
  #stack
  sub_complete <- rbind(sub_complete, sub_frame)
  
  #Clean residuals
  rm(list = c("pc", "z_above", "sub_frame", "LAD", "gpag", "lad", "lad_max", 
              "lmom", "x_range", "y_range", "z_seq", "zframe"))
  
  ###"lad_profile", "gpag_profile", "profiles",
  
  gc()
  
  #Return
  return(sub_complete)
  
}

#Shannon function
shannon <- function(n_points) {
  p.i <- n_points/sum(n_points)
  H <- (-1) * sum(p.i * log10(p.i))
  return(H)
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

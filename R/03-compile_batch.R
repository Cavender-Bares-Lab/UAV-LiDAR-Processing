################################################################################
#' @title Compile results from batch stand and fractal metrics 
################################################################################

#' @description Compile the results from batch processing

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(sf)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_in Path to the files
#' @param path_out Path and name to the final compilation

#-------------------------------------------------------------------------------
#' @example 

path <-  "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
path <- "F:/Projects/LiDAR/data"
path_in <- paste0(path, "/FSC")
path_gpkg <- paste0(path, "/GIS/2022-04-10_FAB2.gpkg")
path_out <- paste0(path, "/FSC_results_h.csv")

compile_results(path_in, gpkg, path_out)

#-------------------------------------------------------------------------------
#' Function

compile_results <- function(path_in, path_gpkg, path_out) {
  
  files <- list.files(path_in, ".csv")
  
  results <- data.table()
  
  #Loop over files
  for(i in 1:length(files)) {
    
    file <- fread(paste0(path_in, "/", files[i]))
    
    date <- as.IDate(substr(files[i], 1, 10))
    Site <- substr(files[i], 12, 15)
    
    file$date <- date
    file$Site <- Site
    
    results <- rbind(results, file)
    
  }
  
  cols <- ncol(results)
  
  #Re-arrange
  results <- results[, .SD, .SDcols = c(((cols-1):cols), (1:(cols-2)))]
  
  # Read gpkg to merge
  limits_gpkg <- st_read(dsn = path_gpkg)
  limits_gpkg <- subset(limits_gpkg, Area != "GCP")
  names(limits_gpkg)[1:2] <- c("Plot", "SR")
  
  gpkg_frame <- as.data.table(as.data.frame(limits_gpkg))
  gpkg_frame <- gpkg_frame[, .SD, .SDcols = c(1:17, 19:20)]
  
  # Merge results
  final <- merge(gpkg_frame, 
                 results, 
                 by = c("Plot", "plot_new"), 
                 all.x = TRUE,
                 all.y = TRUE)
  
  #Export
  fwrite(final, path_out)
  
}

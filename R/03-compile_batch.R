################################################################################
#' @title Compile results from batch stand and fractal metrics 
################################################################################

#' @description Compile the results from batch processing

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)

#-------------------------------------------------------------------------------
#' Arguments
#' @param path_in Path to the files
#' @param path_out Path and name to the final compilation

#-------------------------------------------------------------------------------
#' @example 

path <-  "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
path_in <- paste0(path, "/FSC")
path_out <- paste0(path, "/FSC_results.csv")

compile_results(path_in, path_out)

#-------------------------------------------------------------------------------
#' Function

compile_results <- function(path_in, path_out) {
  
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
  
  #Export
  fwrite(results, path_out)
  
}

################################################################################
#' @title Mergin plot information with derived metrics
################################################################################

#' @description It merge the results from stand metrics with the plot information
#' 
#' @return A .csv file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(sf) 

#' -----------------------------------------------------------------------------
#' Arguments
#' @param path_metrics Path and name of the .las file to process.
#' @param path_gpkg Path and name (without extension) of files to export.
#' @param resolution Resolution of the digital models.

path_metrics <- "/home/antonio/Documents/LiDAR/L5/FAB2/FAB2_LiDAR-metrics.csv"
path_gpkg <- "data/less/FAB2_less.gpkg"
out_path <- "/home/antonio/Documents/LiDAR/L5/FAB2/FAB2_meta.csv"

#' -----------------------------------------------------------------------------
#' Function

merge_information <- function(path_metrics, path_gpkg) {
  
  #Metrics
  metrics <- fread(path_metrics)
  
  #Read gpkp
  limits_gpkg <- st_read(dsn = path_gpkg)
  area <- round(st_area(limits_gpkg), 2)
  limits <- as.data.table(limits_gpkg)
  limits$area <- as.numeric(area)
  limits <- subset(limits, PRESENT == 1)
  limits <- limits[, c("Plot", "Species.Richness", "Treatment", "Species.List", "area")]
  
  #Merge 
  frame <- merge(limits, metrics, by = "Plot")
  
  #Export
  fwrite(frame, out_path)
  
}



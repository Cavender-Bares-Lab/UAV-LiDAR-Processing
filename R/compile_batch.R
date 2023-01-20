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

path <- "F:/LiDAR/L5"
path_in <- paste0(path, "/FAB1")
path_out <- paste0(path, "/FAB1/LiDAR_results.csv")

#-------------------------------------------------------------------------------
#' Function

compile_results <- function(path_in, path_out) {
  
  files <- list.files(path_in, ".txt")
  
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



################################################################################
#' @title Data analysis of the plots.
################################################################################

#' @description Silly data-analysis
#' 
#' @return A .csv file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(ggplot2)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' File reading and merging

# Diversity
diversity <- fread(paste0(root_path, "/diversity.csv"))

# FSC
FSC <- fread(paste0(root_path, "/FSC_results.csv"))
FSC <- subset(FSC, Type == "Small")
colnames(FSC)[3] <- "plot"

# Merge files
data <- merge(diversity, FSC, by = c("plot"), all.x = FALSE, all.y = TRUE)
fwrite(data, paste0(root_path, "/master_file.csv")) #Manual checking
data <- fread(paste0(root_path, "/master_file.csv"))

#' -----------------------------------------------------------------------------
#' Data preparation

# Remove large plots
hist(data$area)
data <- subset(data, area <= 150)

# Long time planted trees
#hist(data$year_mean)
#data <- subset(data, year_mean <= 2018)

# Remove plots with few trees
#hist(data$ntrees)
#data <- data[ntrees >= 85,]

# Get day of the year and just 2022
data[, DOY := yday(data$date)]
data <- subset(data, year(date) == 2022)

# Plots with 13 complete observations
remove <- data[, .N, by = c("plot")]
remove <- remove[N == 8]
data <- merge(data, remove[, 1], by = c("plot"), all.x = FALSE, all.y = TRUE)
data[, .N, by = c("plot")]

# Remove other plots
data <- data[plot != "132",]
data <- data[plot != "61",]

#Export clean data
fwrite(data, paste0(root_path, "/master_clean.csv")) #Manual checking
data <- fread(paste0(root_path, "/master_clean.csv"))

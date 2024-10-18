################################################################################
#' @title Data analysis of the plots.
################################################################################

#' @description Silly data-analysis
#' 
#' @return A .csv file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/work/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' File reading and merging

# Forest structural complexity
LiDAR <- fread(paste0(root_path, "/LiDAR_results (2024-09-19).csv"))
colnames(LiDAR)[1] <- "plot"
LiDAR <- LiDAR[, c(2, 4, 20, 24:49)]
LiDAR <- LiDAR[order(plot_new)]

# Forest structure
structure <- fread(paste0(root_path, "/structural_attributes (2024-10-17).csv"))
structure <- structure[order(plot_new)]

# Diversity
diversity <- fread(paste0(root_path, "/diversity (2024-10-17).csv"))
diversity <- diversity[order(plot_new)]

# AWP and overyielding
NBE <- fread(paste0(root_path, "/plot_NBE (2024-10-17).csv"))
NBE <- NBE[order(plot_new)]

# Merge files
data <- merge(structure, NBE, by = c("plot_new"), all.x = FALSE, all.y = TRUE)
data <- merge(data, diversity, by = c("plot_new"), all.x = TRUE, all.y = FALSE)
data <- merge(data, LiDAR, by = c("plot_new"), all.x = TRUE, all.y = FALSE)
fwrite(data, paste0(root_path, "/master_file (2024-10-17).csv")) #Manual checking
data <- fread(paste0(root_path, "/master_file (2024-10-17).csv"))

#' -----------------------------------------------------------------------------
#' Data preparation

# Long time planted trees
hist(data$year_mean)
data <- subset(data, year_mean < 2019)

# Remove plots with few trees
hist(data$ntrees)
data <- data[ntrees >= 40,]

# Get day of the year and just 2022
data <- data[!is.na(date), ]
data[, DOY := yday(data$date)]
data <- subset(data, year(date) == 2022)

# Plots with 13 complete observations
remove <- data[, .N, by = c("plot_new")]
remove <- remove[N == 8]
data <- merge(data, remove[, 1], by = c("plot_new"), all.x = FALSE, all.y = TRUE)
data[, .N, by = c("plot_new")]

# Volume higher than 0.01/ha
data <- data[volume >= 1.56]

#Export clean data
fwrite(data, paste0(root_path, "/master_clean (2024-10-17).csv")) #Manual checking
data <- fread(paste0(root_path, "/master_clean.csv"))


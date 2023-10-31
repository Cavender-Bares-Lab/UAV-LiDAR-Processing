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

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' File reading and merging

# Forest structural complexity
FSC <- fread(paste0(root_path, "/FSC_results.csv"))
colnames(FSC)[1] <- "plot"
FSC <- FSC[, c(2, 20, 25:47)]

# Forest structure
structure <- fread(paste0(root_path, "/structural_attributes.csv"))

# Diversity
diversity <- fread(paste0(root_path, "/diversity.csv"))

# Merge files
data <- merge(structure, diversity, by = c("plot_new"), all.x = TRUE, all.y = TRUE)
data <- merge(data, FSC, by = c("plot_new"), all.x = TRUE, all.y = TRUE)
fwrite(data, paste0(root_path, "/master_file.csv")) #Manual checking
data <- fread(paste0(root_path, "/master_file.csv"))

#' -----------------------------------------------------------------------------
#' Data preparation

# Long time planted trees
hist(data$year_mean)
data <- subset(data, year_mean <= 2019)

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

# Remove plot
data <- data[volume >= 0.01,]


#Export clean data
fwrite(data, paste0(root_path, "/master_clean.csv")) #Manual checking
data <- fread(paste0(root_path, "/master_clean.csv"))


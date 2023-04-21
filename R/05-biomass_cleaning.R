################################################################################
#' @title Plot biomass (FAB2)
################################################################################

#' @description Data cleaning for the plot biomass files
#' 
#' @return A .csv file

#' #' -----------------------------------------------------------------------------
#' Libraries
library(data.table)

#' -----------------------------------------------------------------------------
#' Processing

root_path <- "/home/antonio/Documents/LiDAR/L5"
data <- fread(paste0(root_path, "/fab2_allometry.csv"))

#Set date and subset by year
data[, measurement_year := paste0(strsplit(measurement_date, "/")[[1]][3]),
     by = seq_along(1:nrow(data))]

data[, measurement_date := paste0(strsplit(measurement_date, "/")[[1]][3], "-",
                                  strsplit(measurement_date, "/")[[1]][1], "-",
                                  strsplit(measurement_date, "/")[[1]][2]),
     by = seq_along(1:nrow(data))]

data <- subset(data, measurement_year == 2022)

#Removing columns
data <- data[, c("year_planted", 
         "species_richness", 
         "species", 
         "treatment", 
         "Area_m2",
         "block",
         "plot",
         "row",
         "column",
         "position",
         "survey",
         "measurement_date",
         "deadmissing",
         "Biomass.conoid_conoidoid_infill")]

#Plot summary
plot_summary <- data[deadmissing == "No", 
                     .(treatment = unique(treatment),
                       area = unique(Area_m2),
                       SR = unique(species_richness),
                       ntrees = .N, 
                       biomass = sum(Biomass.conoid_conoidoid_infill),
                       year_mean = mean(year_planted),
                       year_cv = sd(year_planted)/mean(year_planted)), 
                     by = "plot"]

#Transform biomass from cm3 to m3
plot_summary$biomass <- plot_summary$biomass / 1000
fwrite(plot_summary, paste0(root_path, "/biomass.csv"))

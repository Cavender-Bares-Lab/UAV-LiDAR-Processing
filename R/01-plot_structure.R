################################################################################
#' @title Cleaning of field inventories and estimation of plot metrics
################################################################################

#' @description Data cleaning for extracting plot forest structure metrics
#' 
#' @return A .csv file

#' --------------------------------------------------------------------------
#' Libraries

library(data.table)

#' -----------------------------------------------------------------------------
#' Working path

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Functions

hill <- function(n_points, q) {
  p <- n_points/sum(n_points)
  return((sum(p^q)^(1 / (1 - q))))
}

#' -----------------------------------------------------------------------------
#' Processing

# Load data
data <- fread(paste0(root_path, "/Jeannine_info/fab2_allometry.csv"))

# Define date
data$measurement_date <- as.Date(data$measurement_date, format= "%m/%d/%Y")
data$measurement_date <- as.IDate(data$measurement_date)
data$measurement_year <- year(data$measurement_date)

# Remove data minor errors
data[species == "Juniperus virginia", species := "Juniperus virginiana"]
data[species == "Tilia america", species := "Tilia americana"]

data[plot == "1020-154", plot := "1020_154"]
data[plot == "1020-2016", plot := "1020_2016"]
data[plot == "1020-2017", plot := "1020_2017"]
data[plot == "1020-2018", plot := "1020_2018"]
data[plot == "1011-149", plot := "1011_149"]
data[plot == "1011-150", plot := "1011_150"]
data[plot == "1019-152", plot := "1019_152"]
data[plot == "1019-153", plot := "1019_153"]

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
                 "individual_id",
                 "survey",
                 "measurement_date",
                 "measurement_year",
                 "deadmissing",
                 "V.conoid_conoidoid_infill",
                 "Biomass.conoid_conoidoid_infill")]

#-------------------------------------------------------------------------------
# Work on small plots and 2022 data 

frame_small <- subset(data, Area_m2 == 100)

# Rename row and columns
frame_small[row == 11, row := 1]
frame_small[row == 12, row := 2]
frame_small[row == 13, row := 3]
frame_small[row == 14, row := 4]
frame_small[row == 15, row := 5]
frame_small[row == 16, row := 6]
frame_small[row == 17, row := 7]
frame_small[row == 18, row := 8]
frame_small[row == 19, row := 9]
frame_small[row == 20, row := 10]

frame_small[column == 11, column := 1]
frame_small[column == 12, column := 2]
frame_small[column == 13, column := 3]
frame_small[column == 14, column := 4]
frame_small[column == 15, column := 5]
frame_small[column == 16, column := 6]
frame_small[column == 17, column := 7]
frame_small[column == 18, column := 8]
frame_small[column == 19, column := 9]
frame_small[column == 20, column := 10]

# Remove edges
frame_small <- frame_small[column != 1,]
frame_small <- frame_small[column != 10,]
frame_small <- frame_small[row != 1,]
frame_small <- frame_small[row != 10,]

# Rename plot for new merge with large plots
frame_small$plot_new <- frame_small$plot

#-------------------------------------------------------------------------------
# Work on large plots and 2022 data

frame_large <- subset(data, Area_m2 == 400)

# Rename plot for new merge with small plots
frame_large$plot_new <- "0"
frame_large[row >= 1 & row <= 10 & column >= 1 & column <= 10, plot_new := paste0(plot, "a")]
frame_large[row >= 1 & row <= 10 & column >= 11 & column <= 20, plot_new := paste0(plot, "b")]
frame_large[row >= 11 & row <= 20 & column >= 1 & column <= 10, plot_new := paste0(plot, "c")]
frame_large[row >= 11 & row <= 20 & column >= 11 & column <= 20, plot_new := paste0(plot, "d")]

# Rename row and columns
frame_large[row == 11, row := 1]
frame_large[row == 12, row := 2]
frame_large[row == 13, row := 3]
frame_large[row == 14, row := 4]
frame_large[row == 15, row := 5]
frame_large[row == 16, row := 6]
frame_large[row == 17, row := 7]
frame_large[row == 18, row := 8]
frame_large[row == 19, row := 9]
frame_large[row == 20, row := 10]

frame_large[column == 11, column := 1]
frame_large[column == 12, column := 2]
frame_large[column == 13, column := 3]
frame_large[column == 14, column := 4]
frame_large[column == 15, column := 5]
frame_large[column == 16, column := 6]
frame_large[column == 17, column := 7]
frame_large[column == 18, column := 8]
frame_large[column == 19, column := 9]
frame_large[column == 20, column := 10]

# Remove edges
frame_large <- frame_large[column != 1,]
frame_large <- frame_large[column != 10,]
frame_large <- frame_large[row != 1,]
frame_large <- frame_large[row != 10,]

#-------------------------------------------------------------------------------
# Merge small and large plots

plots <- rbind(frame_small, frame_large)

#-------------------------------------------------------------------------------
# Estimation of plot volume growth from 2021 to 2022.

# Reshape 2021
plots_2021 <- subset(data, year(measurement_date) == 2021)
plots_2021 <- plots_2021[, c("individual_id", "deadmissing", 
                             "measurement_date", "V.conoid_conoidoid_infill")]
plots_2021 <- plots_2021[deadmissing != "Yes", ]
plots_2021 <- plots_2021[, c(1, 3, 4)]
colnames(plots_2021)[2:3] <- c("date_2021", "volume_2021")

# Reshape 2022
plots_2022 <- subset(plots, year(measurement_date) == 2022)
plots_2022 <- plots_2022[, c("plot", "plot_new", "individual_id", "deadmissing", 
                             "measurement_date", "V.conoid_conoidoid_infill")]
plots_2022 <- plots_2022[deadmissing != "Yes", ]
plots_2022 <- plots_2022[, c(1:3, 5:6)]
colnames(plots_2022)[4:5] <- c("date_2022", "volume_2022")

# Merge years
plot_growth <- merge(plots_2021, plots_2022, by = c("individual_id"),
                     all.x = FALSE, all.y = FALSE)

# Estimate Annual Woody Productivity
plot_growth$AWP <- (log(plot_growth$volume_2022) - log(plot_growth$volume_2021)) /
  ((plot_growth$date_2022 - plot_growth$date_2021)/365)

# Get summary per plot
plot_growth_summary <- plot_growth[, .(total_growth_volume = sum(AWP),
                                       min_growth_volume = min(AWP),
                                       max_growth_volume = max(AWP),
                                       mean_growth_volume = mean(AWP),
                                       sd_growth_volume = sd(AWP)), 
                                   by = c("plot", "plot_new")]

#-------------------------------------------------------------------------------
# Summary of metrics by plot.

# Select 2022 inventory
plots <- subset(plots, year(measurement_date) == 2022)

# Get summary
plot_summary <- plots[deadmissing == "No", 
                      .(SR_real = length(unique(species)),
                        year_mean = mean(year_planted),
                        year_cv = sd(year_planted)/mean(year_planted), 
                        ntrees = .N, 
                        volume = sum(V.conoid_conoidoid_infill, na.rm = TRUE),
                        vol_Hill0 = hill(V.conoid_conoidoid_infill, 0),
                        vol_Hill1 = hill(V.conoid_conoidoid_infill, 0.999),
                        vol_Hill2 = hill(V.conoid_conoidoid_infill, 2)),
                      by = c("plot", "plot_new")]

complete <- merge(plot_summary, plot_growth_summary, 
                  by = c("plot", "plot_new"), 
                  all.x = TRUE, 
                  all.y = TRUE)

#-------------------------------------------------------------------------------
# Summary of metrics by species in the plot.

# Reshaping
species_summary <- plots[deadmissing == "No", .(ntrees = .N,
                                                volume = sum(V.conoid_conoidoid_infill, na.rm = TRUE)),
                         by = c("plot", "plot_new", "species")]

# Export species summary 
fwrite(species_summary, paste0(root_path, "species_summary.csv"))

#-------------------------------------------------------------------------------
# Get proportion of deciduous 
traits <- fread(paste0(root_path, "/traits.csv"))
traits <- traits[, c(5, 11)]
species <- species_summary[, c(1, 2, 3, 5)]
colnames(species)[3] <- "Species"

proportions <- merge(traits, species, by = "Species", all.x = TRUE, all.y = TRUE)
p <- proportions[, sum(volume), by = c("plot", "plot_new", "Gymnosperm")]
colnames(p)[4] <- "volume"
proportions <- data.frame(plot = "1", plot_new = "1", PA = 0)
unique_plots <- unique(p$plot_new)

for(i in 1:length(unique_plots)) {
  
  sub <- subset(p, plot_new == unique_plots[i])
  
  if(nrow(sub) == 1) {
    if(sub$Gymnosperm[1] == "N") {
      proportions[i, 1] <- sub$plot[1]
      proportions[i, 2] <- sub$plot_new[1]
      proportions[i, 3] <- 1.00
    } else {
      proportions[i, 1] <- sub$plot[1]
      proportions[i, 2] <- sub$plot_new[1]
      proportions[i, 3] <- 0.00
    }
  } else {
    gym <- sub[Gymnosperm == "Y", volume]
    ang <- sub[Gymnosperm == "N", volume]
    proportions[i, 1] <- sub$plot[1]
    proportions[i, 2] <- sub$plot_new[1]
    proportions[i, 3] <- ang/(gym + ang)
  }
}

complete <- merge(complete, proportions, 
                  by = c("plot", "plot_new"), 
                  all.x = TRUE, 
                  all.y = TRUE)

# Export structural attributes
fwrite(complete, paste0(root_path, "structural_attributes.csv"))




################################################################################
#' @title Net biodiversity effect
################################################################################

#' @description Estimation of the net biodiversity effect
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

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

# Transform volume
data$V.conoid_conoidoid_infill <- data$V.conoid_conoidoid_infill/1000000

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

trees <- rbind(frame_small, frame_large)

#-------------------------------------------------------------------------------
# Estimation of tree volume growth from 2021 to 2022.

# Reshape 2021
trees_2021 <- subset(trees, year(measurement_date) == 2021)
trees_2021 <- trees_2021[, c("individual_id", "deadmissing", 
                             "measurement_date", "V.conoid_conoidoid_infill")]
trees_2021 <- trees_2021[deadmissing != "Yes", ]
trees_2021 <- trees_2021[, c(1, 3, 4)]
colnames(trees_2021)[2:3] <- c("date_2021", "volume_2021")

# Reshape 2022
trees_2022 <- subset(trees, year(measurement_date) == 2022)
trees_2022 <- trees_2022[, c("plot", "plot_new", "individual_id", "deadmissing", 
                             "year_planted", "species", "species_richness",
                             "measurement_date", "V.conoid_conoidoid_infill")]
trees_2022 <- trees_2022[deadmissing != "Yes", ]
trees_2022 <- trees_2022[, c(1:3, 5:9)]
colnames(trees_2022)[7:8] <- c("date_2022", "volume_2022")

# Merge years
tree_growth <- merge(trees_2021, trees_2022, by = c("individual_id"),
                     all.x = TRUE, all.y = TRUE)

# Estimate Annual Woody Productivity
tree_growth$AWP <- (tree_growth$volume_2022 - tree_growth$volume_2021) /
  ((tree_growth$date_2022 - tree_growth$date_2021)/365.25)

# Clean of trees
tree_growth_mono <- tree_growth[year_planted < 2018, ]
tree_growth_mono <- tree_growth_mono[species_richness == 1, ]
tree_growth_mono <- na.exclude(tree_growth_mono)
tree_growth_mono[, .N, by = "species"]

# Mean per species
species_growth <- tree_growth_mono[, .(mean_AWP = mean(AWP, na.rm = TRUE)),
                                   by = "species"]

# Plot growth
tree_growth <- tree_growth[!is.na(volume_2022),]
plot_growth <- tree_growth[, .(volume = sum(volume_2022, na.rm = TRUE),
                               observed_AWP = sum(AWP, na.rm = TRUE),
                               ntrees = .N),
                           by = c("plot_new", "species", "species_richness")]

# Total volume
total_volume <- plot_growth[, .(total_volume = sum(volume)),
                            by = c("plot_new", "species_richness")]

#' -----------------------------------------------------------------------------
#' Estimate the NBE

# Merge volume per and plot growth
frame <- merge(plot_growth, total_volume, by = c("plot_new", "species_richness"))

# Proportion of volume per species on the plot
frame$PV <- frame$volume/frame$total_volume
frame[, sum(PV), by = "plot_new"] # Check for calculation

# Merge species growth with frame
frame <- merge(frame, species_growth, by = c("species"))

# Weighted
frame$expected_AWP <- frame$mean_AWP*frame$PV

# NBE per plot
plot_NBE <- frame[, .(volume = sum(volume),
                      observed_AWP = sum(observed_AWP),
                      expected_AWP = sum(expected_AWP),
                      ntrees = sum(ntrees)),
                  by = c("plot_new", "species_richness")]

plot_NBE$NBE <- (plot_NBE$observed_AWP - plot_NBE$expected_AWP)

fwrite(plot_NBE, paste0(root_path, "/plot_NBE.csv"))

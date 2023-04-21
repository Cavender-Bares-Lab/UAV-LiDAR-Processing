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
library(sf)

#' -----------------------------------------------------------------------------
#' Working path

path <- "/home/antonio/Documents/LiDAR/L5"

#' -----------------------------------------------------------------------------
#' File reading

#Read biomass
biomass <- fread(paste0(path, "/biomass.csv"))
biomass <- biomass[, .SD, .SDcols = c(1, 3:ncol(biomass))]

#Read structural complexity
frame <- fread(paste0(path, "/FAB2_meta.csv"))
colnames(frame)[2] <- "SR"
frame$date <- as.factor(frame$date)
frame <- subset(frame, Site == "FAB2")
data <- merge(frame, biomass, by = c("Plot", "SR"), all.x = TRUE, all.y = TRUE)

#' -----------------------------------------------------------------------------
#' Data preparation

#Remove large plots
hist(data$area.x)
data <- subset(data, area.y <= 150)

#Remove recent plots
data <- subset(data, use == "Yes")

#Remove plots with few trees
hist(data$ntrees)
data <- data[ntrees >= 90,]
data$SR <- as.factor(data$SR)

#' -----------------------------------------------------------------------------
#' Create plots

ggplot(data) +
  geom_point(aes(x = SR, y = Slope_N, fill = ntrees), shape = 21) +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = ntrees, y = Slope_N, fill = date), shape = 21) +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = Intercept_N, y = log10(biomass), fill = date), shape = 21) +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = LAIa, y = RG, fill = date), shape = 21) +
  geom_smooth(aes(x = LAIa, y = RG, colour = date), method = "lm", se = FALSE) +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = log10(LAIa), y = Slope_N, fill = SR), shape = 21) +
  geom_smooth(aes(x = log10(LAIa), y = Slope_N, colour = SR), method = "lm", se = FALSE)  +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = log10(LAIa), y = Slope, fill = SR), shape = 21) +
  geom_smooth(aes(x = log10(LAIa), y = Slope, colour = SR), method = "lm", se = FALSE)  +
  facet_grid(SR ~ date)

#VCI
ggplot(data) +
  geom_point(aes(x = SR, y = vci, fill = SR), shape = 21) +
  geom_smooth(aes(x = SR, y = vci, colour = SR), method = "lm", se = FALSE)  +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = ntrees, y = vci, fill = SR), shape = 21) +
  geom_smooth(aes(x = ntrees, y = vci, colour = SR), method = "lm", se = FALSE)  +
  facet_grid(SR ~ date)

#Structural complexity
ggplot(data) +
  geom_point(aes(x = SR, y = Slope, fill = SR), shape = 21) +
  geom_smooth(aes(x = SR, y = Slope, colour = SR), method = "lm", se = FALSE)  +
  facet_grid( ~ date)

ggplot(data) +
  geom_point(aes(x = SR, y = log10(npoints), fill = SR), shape = 21) +
  geom_smooth(aes(x = SR, y = log10(npoints), colour = SR), method = "lm", se = FALSE)  +
  facet_grid(SR ~ date)

#Structural complexity
ggplot(data) +
  geom_point(aes(x = SR, y = n_profiles, fill = SR), shape = 21) +
  geom_smooth(aes(x = SR, y = n_profiles, colour = SR), method = "lm", se = FALSE)  +
  facet_grid( ~ date)


#"Plot"         "Area"         "biomass"      "ntrees"       "Site"         "SR"           "Treatment.x"  "Species"      "date"         "Intercept"   
#"Slope"        "Rsq"          "npoints"      "height_max"   "height_mean"  "height_cv"    "skwewness"    "kurtosis"     "n_profiles"   "LAIa"        
#"vci"          "entropy"      "H"            "Hmax"         "equitavility" "negentropy"   "Cx"           "Cy"           "RG"

plots <- unique(data$Plot)

for(i in 1:plots) {
  
  stand <- subset(data, Plot == plots[i])
  
  
  
  
}
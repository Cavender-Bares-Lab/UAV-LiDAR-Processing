library(data.table)
library(ggplot2)
library(sf)

#Read .gpkg file
path_gpkg <- "data/less/FAB2_less.gpkg"
limits_gpkg <- st_read(dsn = path_gpkg)
area <- as.numeric(st_area(limits_gpkg))
Plot <- limits_gpkg$Plot
data <- data.table(Plot = Plot, Area = area)

#Read biomass
biomass <- fread("F:/LiDAR/L5/biomass.csv")
data <- merge(data, biomass, by = "Plot", all.x = TRUE, all.y = TRUE)

#Read structural complexity
frame <- fread("F:/LiDAR/L5/LiDAR_FAB2.csv")
frame$SR <- as.factor(frame$SR)
frame$date <- as.factor(frame$date)
frame <- subset(frame, Site == "FAB2")
frame$date <- as.IDate(frame$date)

data <- merge(data, frame, by = "Plot", all.x = TRUE, all.y = TRUE)

ggplot(frame) +
  geom_point(aes(x = SR, y = Slope, fill = date), shape = 21) +
  geom_smooth(aes(x = SR, y = Slope, colour = date), method = "lm", se = FALSE)

ggplot(frame) +
  geom_point(aes(x = LAIa, y = RG, fill = date), shape = 21) +
  geom_smooth(aes(x = LAIa, y = RG, colour = date), method = "lm", se = FALSE)

ggplot(frame) +
  geom_point(aes(x = log10(LAIa), y = Slope, fill = SR), shape = 21) +
  geom_smooth(aes(x = log10(LAIa), y = Slope, colour = SR), method = "lm", se = FALSE)


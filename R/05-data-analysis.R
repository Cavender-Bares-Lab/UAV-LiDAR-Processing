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
#' File

path <- "/media/antonio/Extreme SSD/LiDAR/L5"
frame <- fread(paste0(path, "/LiDAR-metrics.csv"))
frame <- subset(frame, Site == "FAB2")

frame <- subset(frame, area <= 150)
frame <- subset(frame, Treatment == "Monoculture")
frame <- subset(frame, date == as.IDate("2022-07-06"))
plot(frame$date, frame$Slope)

LDA <- frame[, c(27:143)]

ggplot(frame) +
  geom_point(aes(x = date, y = equitavility, fill = Species), shape = 21)

ggplot(frame) +
  geom_point(aes(x = SR, y = Slope, fill = as.character(date)), shape = 21) +
  geom_smooth(aes(x = SR, y = Slope, colour = as.character(date)), method=lm)


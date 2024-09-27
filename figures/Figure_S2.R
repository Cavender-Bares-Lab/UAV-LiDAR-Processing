################################################################################
#' @title Accumulated growing degree days
################################################################################

#' @description Figure S2 of the accumulative growing degree days for Cedar Cree on
#' 2022
#' 
#' @return A jpeg file with the figure

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(ggplot2)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Processing

# Weather data
data <- fread(paste0(root_path, "/climate.csv"))

# Define date
data$Date <- as.IDate(data$Date)
data$DOY <- yday(data$Date)
data$GDD <- ((data$MaxTemp+data$MinTemp)/2) - 15 
data[GDD <= 0, GDD := 0]
data$AGDD <- cumsum(data$GDD)

# ------------------------------------------------------------------------------
# Plot details

tamano <- 12
tamano2 <- 10
text_size <- 2.8
th <- theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            plot.margin = margin(4, 4, 0, 1, "pt"),
            legend.position= c("top"),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_blank(),
            strip.background = element_rect(color="black",
                                            fill="black",
                                            linewidth=1.5,
                                            linetype="solid"),
            strip.text = element_text(color = "white"))

# ------------------------------------------------------------------------------
# Plot

plot <- ggplot(data,
               aes(x = DOY,
                   y = AGDD)) +
  geom_line(colour = "grey25",
            linewidth = 0.5) +
  geom_vline(xintercept = c(100, 138, 163, 188, 215, 250, 260, 297),
             colour = "darkgreen",
             linetype = "dotted",
             linewidth = 0.5) +
  scale_x_continuous(limits = c(1, 365), expand = c(0, 0), breaks = c(100, 200, 300)) +
  scale_y_continuous(limits = c(-20, 5310), expand = c(0, 0)) +
  #doy_color + doy_fill +
  xlab("Day of the Year") +
  ylab("Accumulated growing degree days") +
  theme_bw(base_size = tamano) +
  th


# Export figure
jpeg(paste0(root_path, "/Figure_S2.jpeg"), width = 100, height = 80, units = "mm", res = 600)

plot

dev.off()


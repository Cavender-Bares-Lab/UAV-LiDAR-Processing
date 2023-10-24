################################################################################
#' @title Temporal variation of the fractal geometry
################################################################################

#' @description Figure that shows the temporal variation of the fractal geometry
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(rcartocolor)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggpmisc)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Deciduous"]
frame[PA == 0, plot_type := "Evergreen"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Data reshaping

reshaping <- frame
reshaping_int <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                               "Intercept_Hill0", "Intercept_Hill1", "Intercept_Hill2")]
reshaping_frac <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                                "Slope_Hill0", "Slope_Hill1", "Slope_Hill2")]

reshaping_int <- melt(reshaping_int, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_int$parameter <- "intercept"

reshaping_frac <- melt(reshaping_frac, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_frac$parameter <- "slope"

reshaping <- rbind(reshaping_int, reshaping_frac)
reshaping[variable == "Intercept_Hill0" | variable == "Slope_Hill0" | variable == "height_hill0", qhill := "q0"]
reshaping[variable == "Intercept_Hill1" | variable == "Slope_Hill1" | variable == "height_hill1", qhill := "q1"]
reshaping[variable == "Intercept_Hill2" | variable == "Slope_Hill2" | variable == "height_hill2", qhill := "q2"]

reshaping$qhill <- as.factor(reshaping$qhill)
levels(reshaping$qhill) <- c(expression(paste(italic(q)," = 0")),
                             expression(paste(italic(q)," = 1")),
                             expression(paste(italic(q)," = 2")))

reshaping_frac <- reshaping[parameter == "slope"]
reshaping_frac$parameter <- as.factor(reshaping_frac$parameter)
levels(reshaping_frac$parameter) <-  c("slope" = expression(italic('d')[italic(D)]))

reshaping_int <- reshaping[parameter == "intercept"]
reshaping_int$parameter <- as.factor(reshaping_int$parameter)
levels(reshaping_int$parameter) <-  c("intercept" = expression(ENV*italic(a)[italic(D)]))

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
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Figure S1

doy_fract <- ggplot(reshaping_frac, 
                      aes(x = DOY,
                          y = value, 
                          colour = PA,
                          fill = PA,
                          gruop = as.factor(plot_new))) +
  geom_point(alpha = 0.1) +
  geom_line(alpha = 0.2) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_colour_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(ylim = c(1.5, 2.5), expand = TRUE) +
  scale_y_continuous(n.breaks = 3, breaks = c(1.50, 2.00, 2.50), 
                     labels = c("1.5", "2.0", "2.5")) +
  xlab(" ") +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(plot_type ~ qhill, labeller = label_parsed)

#Export figure
jpeg("Figure_S1.jpeg", width = 210, height = 150, units = "mm", res = 600)

doy_fract

dev.off()


# Figure S2

doy_int <- ggplot(reshaping_int, 
                    aes(x = DOY,
                        y = exp(value), 
                        colour = PA,
                        fill = PA,
                        gruop = as.factor(plot_new))) +
  geom_point(alpha = 0.1) +
  geom_line(alpha = 0.2) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_colour_carto_c("Proportion of Angiosperms", 
                       type = "diverging", 
                       palette = "Earth",
                       direction = -1,
                       limits = c(0, 1),
                       breaks = c(0.0, 0.5, 1.0)) +
  #coord_cartesian(ylim = c(1.5, 2.5), expand = TRUE) +
  #scale_y_continuous(n.breaks = 3, breaks = c(1.50, 2.00, 2.50), 
  #                   labels = c("1.5", "2.0", "2.5")) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab("Day of the Year") +
  ylab(bquote(ENV[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(plot_type ~ qhill, labeller = label_parsed)

#Export figure
jpeg("Figure_S2.jpeg", width = 210, height = 150, units = "mm", res = 600)

doy_int

dev.off()


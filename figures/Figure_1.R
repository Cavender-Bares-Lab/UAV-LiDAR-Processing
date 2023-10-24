################################################################################
#' @title Effect of biomass, tree size inequality, and PA on FSC.
################################################################################

#' @description Effect of biomass, tree size inequality, and PA on FSC
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

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Deciduous"]
frame[PA == 0, plot_type := "Evergreen"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Data reshaping

reshaping <- frame
reshaping$height_hill0 <- exp(reshaping$Intercept_Hill0 + (log(1/reshaping$mean_maximun_height)*reshaping$Slope_Hill0))
reshaping$height_hill1 <- exp(reshaping$Intercept_Hill1 + (log(1/reshaping$mean_maximun_height)*reshaping$Slope_Hill1))
reshaping$height_hill2 <- exp(reshaping$Intercept_Hill2 + (log(1/reshaping$mean_maximun_height)*reshaping$Slope_Hill2))

reshaping_int <- reshaping[, c("plot_type", "DOY", "volume",
                               "Intercept_Hill0", "Intercept_Hill1", "Intercept_Hill2")]
reshaping_frac <- reshaping[, c("plot_type", "DOY", "volume",
                               "Slope_Hill0", "Slope_Hill1", "Slope_Hill2")]
reshaping_height <- reshaping[, c("plot_type", "DOY", "volume",
                                  "height_hill0", "height_hill1", "height_hill2")]

reshaping_int <- melt(reshaping_int, id.vars = c("plot_type", "DOY", "volume"))
reshaping_int$parameter <- "intercept"
reshaping_frac <- melt(reshaping_frac, id.vars = c("plot_type", "DOY", "volume"))
reshaping_frac$parameter <- "slope"
reshaping_height <- melt(reshaping_height, id.vars = c("plot_type", "DOY", "volume"))
reshaping_height$parameter <- "ENVh"

reshaping <- rbind(reshaping_int, reshaping_frac, reshaping_height)
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

reshaping_ENVh <- reshaping[parameter == "ENVh"]
reshaping_ENVh$parameter <- as.factor(reshaping_ENVh$parameter)
levels(reshaping_ENVh$parameter) <-  c("ENVh" = expression(ENV*italic(h)[italic(D)]))

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

# Plot for fractals

vol_fractal <- ggplot(reshaping_frac, 
                      aes(x = volume/1000000,
                          y = value, 
                          color = DOY,
                          fill = DOY,
                          gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              colour = "black",
                                                              alpha = 1))) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0.0137, 0.462), ylim = c(1.5, 2.5), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3, breaks = c(1.50, 2.00, 2.50), 
                     labels = c("1.5", "2.0", "2.5")) +
  annotation_logticks(sides = "b") +
  xlab(expression(paste("Wood volume (m"^3, ")"))) +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(qhill ~ parameter, labeller = label_parsed)

# Plot for intercept

vol_intercept <- ggplot(reshaping_int,
                        aes(x = volume/1000000,
                            y = exp(value),
                            color = DOY,
                            fill = DOY,
                            gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              colour = "black",
                                                              alpha = 1))) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #coord_cartesian(xlim = c(0.0137, 0.462), ylim = c(3, 6.5), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(breaks = c(0.30, 0.45, 0.60),
  #                   labels = c("0.30", "0.45", "0.60")) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(expression(paste("Wood volume (m"^3, ")"))) +
  ylab(bquote(ENV*italic(a)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(qhill ~ parameter, labeller = label_parsed)

vol_hight <- ggplot(reshaping_ENVh,
                        aes(x = volume/1000000,
                            y = value,
                            color = DOY,
                            fill = DOY,
                            gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              colour = "black",
                                                              alpha = 1))) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #coord_cartesian(xlim = c(0.0137, 0.462), ylim = c(3, 6.5), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(breaks = c(0.30, 0.45, 0.60),
  #                   labels = c("0.30", "0.45", "0.60")) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(expression(paste("Wood volume (m"^3, ")"))) +
  ylab(bquote(ENV*italic(h)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(qhill ~ parameter, labeller = label_parsed)

#-------------------------------------------------------------------------------
#Merge panels

Figure_1 <- ggarrange(vol_fractal, vol_intercept, vol_hight,
                      ncol = 3, nrow = 1,  align = "hv", 
                      widths = c(2, 2), 
                      heights = c(2, 2, 2),
                      #labels = c("a", "b", "c", "d", "e", "f"), 
                      font.label = list(size = 14, 
                                        color = "black", 
                                        face = "plain", 
                                        family = NULL),
                      label.x = 0.13,
                      label.y = 0.95,
                      common.legend = TRUE)
#Export figure
jpeg("Figure_1.jpeg", width = 240, height = 190, units = "mm", res = 600)

Figure_1

dev.off()


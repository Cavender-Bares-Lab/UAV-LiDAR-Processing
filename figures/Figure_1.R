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
#frame[PA == 1, plot_type == "Angiosperms"]

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
            legend.box.background = element_blank())
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))


# Biomass

vol_SEIv <- ggplot(frame, aes(volume/1000000, 
                              exp(FHD_vertical) * log(height), 
                              color = DOY,
                              fill = DOY,
                              gruop = as.factor(DOY))) +
  geom_point(shape = 21, colour = "grey", alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #coord_cartesian(ylim = c(0.2, 1.0), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(n.breaks = 3, breaks = c(0.2, 0.6, 1.0), 
  #                   labels = c("0.20", "0.60", "1.00")) +
  annotation_logticks(sides = "b") +
  xlab(" ") +
  ylab(expression(SEI[vertical]))  +
  theme_bw(base_size = tamano) +
  th + gui

vol_SEIh <- ggplot(frame, aes(volume/1000000, 
                              exp(FHD_horizontal), 
                              color = DOY,
                              fill = DOY,
                              gruop = as.factor(DOY))) +
  geom_point(shape = 21, colour = "grey", alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #coord_cartesian(ylim = c(0.96, 1.00), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  #scale_y_continuous(n.breaks = 3) +
  annotation_logticks(sides = "b") +
  xlab(" ") +
  ylab(expression(SEI[horizontal]))  +
  theme_bw(base_size = tamano) +
  th + gui

vol_fractal <- ggplot(frame, aes(volume/1000000, 
                                 Slope_Hill1^exp(FHD_vertical), 
                                 color = DOY,
                                 fill = DOY,
                                 gruop = as.factor(DOY))) +
  geom_point(aes(size = ntrees), shape = 21, colour = "grey", alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #coord_cartesian(ylim = c(0.40, 0.85), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3) +
  annotation_logticks(sides = "b") +
  xlab(expression(paste("Wood volume (m"^3, ")"))) +
  ylab(expression({}*italic(D)[I]))  +
  theme_bw(base_size = tamano) +
  th + gui


# Tree size inequality

TSI_SEIv <- ggplot(frame, aes(tree_size_inequality_vol, 
                              SEI_vertical, 
                              color = DOY,
                              fill = DOY,
                              gruop = as.factor(DOY))) +
  geom_point(shape = 21, colour = "grey", alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  scale_x_continuous(n.breaks = 4) +
  coord_cartesian(xlim = c(0.2, 0.82), ylim = c(0.2, 1.0), expand = TRUE) +
  scale_y_continuous(n.breaks = 3, breaks = c(0.2, 0.6, 1.0), 
                     labels = c("0.20", "0.60", "1.00")) +
  xlab(" ") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th + gui

TSI_SEIh <- ggplot(frame, aes(tree_size_inequality_vol, 
                              SEI_horizontal, 
                              color = DOY,
                              fill = DOY,
                              gruop = as.factor(DOY))) +
  geom_point(shape = 21, colour = "grey", alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0.2, 0.82), ylim = c(0.96, 1.00), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 3) +
  xlab(" ") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th + gui

TSI_fractal <- ggplot(frame, aes(tree_size_inequality_vol, 
                                 Slope_Hill1, 
                                 color = DOY,
                                 fill = DOY,
                                 gruop = as.factor(DOY))) +
  geom_point(shape = 21, colour = "grey", alpha = 0.2) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #coord_cartesian(xlim = c(0.2, 0.82), ylim = c(0.65, 0.85), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 3) +
  xlab("Tree size inequality") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th + gui

#Merge panels
Figure_1 <- ggarrange(vol_SEIv, TSI_SEIv, 
                      vol_SEIh, TSI_SEIh, 
                      vol_fractal, TSI_fractal,
                      ncol = 2, nrow = 3,  align = "hv", 
                      widths = c(2, 2), 
                      heights = c(2, 2, 2),
                      labels = c("a", "b", "c", "d", "e", "f"), 
                      font.label = list(size = 14, 
                                        color = "black", 
                                        face = "plain", 
                                        family = NULL),
                      label.x = 0.13,
                      label.y = 0.95,
                      common.legend = TRUE)
#Export figure
jpeg("Figure_1.jpeg", width = 210, height = 210, units = "mm", res = 600)

Figure_1

dev.off()


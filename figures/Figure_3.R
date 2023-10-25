################################################################################
#' @title Effect of tree community composition
################################################################################

#' @description Relationships between diversity and FSC
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(rcartocolor)
library(ggplot2)
library(ggpmisc)
library(scales)
library(ggpubr)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Deciduous"]
frame[PA == 0, plot_type := "Evergreen"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

diversity <- fread(paste0(root_path, "/diversity_reshaped.csv"))


data <- merge(diversity, 
              frame, 
              by = c("plot_new"), 
              all.x = FALSE, 
              all.y = TRUE,
              allow.cartesian=TRUE)

# Factor orders
data$type <- as.factor(data$type)
data$type <- factor(data$type, 
                    levels = c("Taxonomic", 
                               "Phylogenetic", 
                               "Functional"))

# Plot details
tamano <- 12
tamano2 <- 10
text_size <- 2.8
th <- theme(plot.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            plot.margin = margin(4, 4, 0, 1.5, "pt"),
            legend.position= c("top"), 
            legend.direction = "horizontal", 
            legend.background = element_rect(fill = "transparent"), 
            legend.box.background = element_blank())
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

#faith MPD PSV PSR 

# Diversity
ggplot(data, 
       aes(PSR, 
           Slope_Hill0,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), 
             fill = "grey", 
             alpha = 0.2) +
  stat_smooth(method='lm', 
              formula = y ~ x, 
              se = FALSE,
              linewidth = 0.5) +
  stat_poly_eq(size = text_size,
               label.x = "left",
               label.y = "bottom") +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  #scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(trans = log10_trans()) +
  xlab(" ") +
  ylab(expression(SEI[vertical]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ type, scales = "free_x")

horizontal <- ggplot(data, 
       aes(PSV, 
           FHD_horizontal,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(shape = 21, 
             colour = "grey", 
             alpha = 0.2) +
  stat_smooth(method='lm', 
              formula = y~poly(x,2), 
              se = FALSE,
              linewidth = 0.5) +
  stat_poly_eq(size = text_size,
               label.x = "right",
               label.y = "bottom") +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0, 1), 
                  ylim = c(0.8, 1.0), 
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0), 
                     labels = c(0, 0.5, 1.0)) +
  scale_y_continuous(breaks = c(0.8, 0.9, 1.0), 
                     labels = c(0.8, 0.9, 1.0)) +
  xlab(" ") +
  ylab(expression(SEI[horizontal]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Horizontal" ~ type)

trid <- ggplot(data, 
       aes(PSV, 
           Slope_H,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(shape = 21, 
             colour = "grey", 
             alpha = 0.2) +
  stat_smooth(method = 'lm', 
              #formula = y~poly(x,2), 
              se = FALSE,
              linewidth = 0.5) +
  stat_poly_eq(size = text_size,
               #formula = y~poly(x,2),
               label.x = "right",
               label.y = "bottom") +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0, 1), 
                  ylim = c(0.5, 0.85), 
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), 
                     labels = c(0.0, 0.5, 1.0)) +
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8), 
                     labels = c(0.2, 0.5, 0.8)) +
  xlab("Species variability") +
  ylab(expression({}*italic(D)[I]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("3D" ~ type)


#Merge panels
Figure_3 <- ggarrange(vertical, 
                      horizontal, 
                      trid,
                      ncol = 1, nrow = 3,  align = "hv", 
                      font.label = list(size = 14, 
                                        color = "black", 
                                        face = "plain", 
                                        family = NULL),
                      common.legend = TRUE)
#Export figure
jpeg("Figure_3.jpeg", width = 210, height = 210, units = "mm", res = 600)

Figure_3

dev.off()

data[SR_real == 1, PSV := 0]

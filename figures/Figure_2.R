################################################################################
#' @title Phenological effects
################################################################################

#' @description Phenological effect on FSC and their association with 
#' plot metrics
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
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data
frame <- fread(paste0(root_path, "/master_clean.csv"))
diversity <- fread(paste0(root_path, "/diversity.csv"))

# Get phenological variability
FSC <- frame[, c("plot", "DOY", "shannon_vertical", 
                 "shannon_horizontal", "Slope_N")]

pheno <- FSC[, .(CV_SEI_vertical = sd(shannon_vertical)/mean(shannon_vertical),
                 CV_SEI_horizontal = sd(shannon_horizontal)/mean(shannon_horizontal),
                 CV_Slope_N = sd(Slope_N)/mean(Slope_N)), by = "plot"]
pheno$plot <- as.character(pheno$plot)

# Merge metrics of interest
frame <- merge(pheno, diversity, by = "plot", all.x = TRUE, all.y = FALSE)

# Plot details
tamano <- 12
tamano2 <- 8
text_size <- 2.8
th <- theme(plot.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            #axis.text.x = element_text(color = "black", size = tamano2),
            #axis.text.y = element_text(color = "black", size = tamano2),
            plot.margin = margin(4, 4, 0, 1.5, "pt"),
            legend.position= c("top"), 
            legend.direction = "horizontal", 
            legend.background = element_rect(fill = "transparent"), 
            legend.box.background = element_blank())
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Biomass

bio_SEIv <- ggplot(frame, aes(volumen/1000000, 
                              CV_SEI_vertical,
                              fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.012, 0.7), 
                  ylim = c(0.009, 0.48), 
                  expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(" ") +
  ylab(expression({}*italic(CV)~~SEI[vertical]))  +
  theme_bw(base_size = tamano) +
  th + gui

bio_SEIh <- ggplot(frame, aes(volumen/1000000, 
                              CV_SEI_horizontal,
                              fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.012, 0.7), 
                  ylim = c(0.0003, 0.055),
                  expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(" ") +
  ylab(expression({}*italic(CV)~~SEI[horizontal]))  +
  theme_bw(base_size = tamano) +
  th + gui

bio_fractal <- ggplot(frame, aes(volumen/1000000, 
                                 CV_Slope_N,
                                 fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.012, 0.7), 
                  ylim = c(0.0035, 0.3401),
                  expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(expression(paste("Wood volume (m"^3, ")"))) +
  ylab(expression({}*italic(CV)~~{}*italic(D)[b]))  +
  theme_bw(base_size = tamano) +
  th + gui

# Tree size inequality

TSI_SEIv <- ggplot(frame, aes(tree_size_inequality_vol, 
                              CV_SEI_vertical,
                              fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  #stat_ma_line(method = "SMA",
  #             se = TRUE,
  #             size = 0.5,
  #             colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.2, 0.8),
                  ylim = c(0.009, 0.48), 
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.2, 0.5, 0.8),
                     labels = c(0.2, 0.5, 0.8)) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th + gui

TSI_SEIh <- ggplot(frame, aes(tree_size_inequality_vol, 
                              CV_SEI_horizontal,
                              fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "right",
             label.y = "bottom",
             size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.2, 0.8), 
                  ylim = c(0.0003, 0.055),
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.2, 0.5, 0.8),
                     labels = c(0.2, 0.5, 0.8)) +
  scale_y_continuous(trans = log10_trans()) +
  xlab(" ") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th + gui

TSI_fractal <- ggplot(frame, aes(tree_size_inequality_vol, 
                                 CV_Slope_N,
                                 fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  #stat_ma_line(method = "SMA",
  #             se = TRUE,
  #             size = 0.5,
  #             colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "right",
             label.y = "bottom",
             size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  coord_cartesian(xlim = c(0.2, 0.8),
                  ylim = c(0.0035, 0.3401),
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.2, 0.5, 0.8),
                     labels = c(0.2, 0.5, 0.8)) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab("Tree size inequality") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th + gui


#Merge panels
Figure_2 <- ggarrange(bio_SEIv, TSI_SEIv, 
                      bio_SEIh, TSI_SEIh, 
                      bio_fractal, TSI_fractal,
                      ncol = 2, nrow = 3,  align = "hv", 
                      widths = c(2, 2), 
                      heights = c(2, 2, 2),
                      labels = c("a", "b", "c", "d", "e", "f"), 
                      font.label = list(size = 14, 
                                        color = "black", 
                                        face = "plain", 
                                        family = NULL),
                      label.x = 0.18,
                      label.y = 0.97,
                      common.legend = TRUE)
#Export figure
jpeg("Figure_2.jpeg", width = 210, height = 210, units = "mm", res = 600)

Figure_2

dev.off()


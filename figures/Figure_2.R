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
            plot.margin = margin(4, 4, 0, 0, "pt"),
            legend.position= c("top"), 
            legend.direction = "horizontal", 
            legend.background = element_rect(fill = "transparent"), 
            legend.box.background = element_blank())

# Biomass

bio_SEIv <- ggplot(frame, aes(biomass/1000, 
                              CV_SEI_vertical)) +
  geom_point(shape = 21, colour = "black", fill = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  coord_cartesian(expand = FALSE) +
  annotation_logticks(sides = "bl") +
  xlab(" ") +
  ylab(expression({}*italic(CV)~~SEI[vertical]))  +
  theme_bw(base_size = tamano) +
  th

bio_SEIh <- ggplot(frame, aes(biomass/1000, 
                              CV_SEI_horizontal)) +
  geom_point(shape = 21, colour = "black", fill = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) + 
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  coord_cartesian(ylim = c(0.00036, 0.06), expand = FALSE) +
  annotation_logticks(sides = "bl") +
  xlab(" ") +
  ylab(expression({}*italic(CV)~~SEI[horizontal]))  +
  theme_bw(base_size = tamano) +
  th

bio_fractal <- ggplot(frame, aes(biomass/1000, 
                                 CV_Slope_N)) +
  geom_point(shape = 21, colour = "black", fill = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  coord_cartesian(ylim = c(0.00036, 0.3405), expand = FALSE) +
  annotation_logticks(sides = "bl") +
  xlab("Above-ground biomass (kg)") +
  ylab(expression({}*italic(CV)~~{}*italic(D)[b]))  +
  theme_bw(base_size = tamano) +
  th



# Tree size inequality

TSI_SEIv <- ggplot(frame, aes(tree_size_inequality, 
                              CV_SEI_vertical)) +
  geom_point(shape = 21, colour = "black", fill = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "left",
             label.y = "bottom",
             size = text_size) +
  #scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  coord_cartesian(expand = FALSE) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th

TSI_SEIh <- ggplot(frame, aes(tree_size_inequality, 
                              CV_SEI_horizontal)) +
  geom_point(shape = 21, colour = "black", fill = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "right",
             label.y = "bottom",
             size = text_size) + 
  #scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  coord_cartesian(ylim = c(0.00036, 0.06), expand = FALSE) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th

TSI_fractal <- ggplot(frame, aes(tree_size_inequality, 
                                 CV_Slope_N)) +
  geom_point(shape = 21, colour = "black", fill = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               size = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "right",
             label.y = "bottom",
             size = text_size) +
  #scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  coord_cartesian(ylim = c(0.00036, 0.3405), expand = FALSE) +
  annotation_logticks(sides = "l") +
  xlab("Tree size inequality") +
  ylab(" ")  +
  theme_bw(base_size = tamano) +
  th


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
                      label.x = 0.25,
                      label.y = 0.99,
                      common.legend = TRUE)
#Export figure
tiff("Figure_2.tif", width = 140, height = 140, units = "mm", res = 600)

Figure_2

dev.off()


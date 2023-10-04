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
diversity <- fread(paste0(root_path, "/diversity_reshaped.csv"))

# Get phenological variability
FSC <- frame[, c("plot", "PA", "DOY", "shannon_vertical", 
                 "shannon_horizontal", "Slope_N")]

pheno <- FSC[, .(CV_SEI_vertical = sd(shannon_vertical)/mean(shannon_vertical),
                 CV_SEI_horizontal = sd(shannon_horizontal)/mean(shannon_horizontal),
                 CV_Slope_N = sd(Slope_N)/mean(Slope_N),
                 PA = mean(PA)), by = "plot"]
pheno$plot <- as.character(pheno$plot)

# Merge metrics of interest
frame <- merge(pheno, diversity, by = "plot", all.x = TRUE, all.y = FALSE)

# Factor orders
frame$type <- as.factor(frame$type)
frame$type <- factor(frame$type, levels = c("Taxonomic", 
                                          "Phylogenetic", 
                                          "Functional"))

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
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

#Plots

PSV_SEIv <- ggplot(frame, aes(PSV,
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
  coord_cartesian(xlim = c(0, 1.0), 
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), 
                     labels = c(0.0, 0.5, 1.0)) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(expression({}*italic(CV)~~SEI[vertical]))  +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Vertical" ~ type)

PSV_SEIh <- ggplot(frame, aes(PSV, 
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
  coord_cartesian(xlim = c(0, 1.0), 
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), 
                     labels = c(0.0, 0.5, 1.0)) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(expression({}*italic(CV)~~SEI[horizontal]))  +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Horizontal" ~ type)

PSV_trid <-ggplot(frame, aes(PSV, 
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
  coord_cartesian(xlim = c(0, 1.0),
                  ylim = c(0.003, 0.34),
                  expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), 
                     labels = c(0.0, 0.5, 1.0)) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab("Species variability") +
  ylab(expression({}*italic(CV)~~{}*italic(D)[b]))  +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("3D" ~ type)


#Merge panels
Figure_4 <- ggarrange(PSV_SEIv,
                      PSV_SEIh,
                      PSV_trid,
                      ncol = 1, nrow = 3,  align = "hv", 
                      common.legend = TRUE)
#Export figure
jpeg("Figure_4.jpeg", width = 210, height = 210, units = "mm", res = 600)

Figure_4

dev.off()


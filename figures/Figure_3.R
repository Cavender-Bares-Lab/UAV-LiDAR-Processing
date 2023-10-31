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
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Deciduous"]
frame[PA == 0, plot_type := "Evergreen"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "TD_PSV", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type")]
phylo <- frame[, c("DOY", "PD_PSV", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type")]
funct <- frame[, c("DOY", "FD_PSV", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type")]

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

colnames(taxa)[2] <- "PSV"
colnames(phylo)[2] <- "PSV"
colnames(funct)[2] <- "PSV"

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

data_melt <- melt(data, 
     id.vars = c("DOY", "PSV", "plot_type", "type"),
     measure.vars = c("Slope_Hill1", "Pgap", "mean_maximun_height"))

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

#faith MPD PSV PSR 

# Diversity
fractal <- ggplot(data_melt[variable == "Slope_Hill1",], 
       aes(x = PSV, 
           y = value,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              colour = "black",
                                                              alpha = 1),
                                          title.position = "top",
                                          title.hjust = 0.5)) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(1.40, 2.6), expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  xlab("Species variability") +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Structural complexity" ~ type, scales = "free")


gap <- ggplot(data_melt[variable == "Pgap",], 
       aes(x = PSV, 
           y = value,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = text_size) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              colour = "black",
                                                              alpha = 1),
                                          title.position = "top",
                                          title.hjust = 0.5)) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  xlab("Species variability") +
  ylab(bquote(italic(P)[gap]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Cover" ~ type, scales = "free")



ch <- ggplot(data_melt[variable == "mean_maximun_height",], 
       aes(x = PSV, 
           y = value,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              colour = "black",
                                                              alpha = 1),
                                          title.position = "top",
                                          title.hjust = 0.5)) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 4.5), expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  xlab("Species variability") +
  ylab(bquote(bar(italic(CH)))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Canopy height" ~ type, scales = "free")

# ------------------------------------------------------------------------------
#Merge panels
Figure_3 <- ggarrange(ch, 
                      gap, 
                      fractal,
                      ncol = 1, nrow = 3,  align = "hv", 
                      font.label = list(size = 14, 
                                        color = "black", 
                                        face = "plain", 
                                        family = NULL),
                      common.legend = TRUE)
#Export figure
jpeg(paste0(root_path, "/Figure_3.jpeg"), width = 210, height = 210, units = "mm", res = 600)

Figure_3

dev.off()


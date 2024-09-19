################################################################################
#' @title Effect of diversity on LiDAR derived metrics
################################################################################

#' @description Figure 4 of relationships species variability on LiDAR 
#' derived metrics
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

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "hill0_taxa", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]
phylo <- frame[, c("DOY", "hill0_phylo", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]
funct <- frame[, c("DOY", "hill0_FD_q", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

colnames(taxa)[2] <- "Diversity"
colnames(phylo)[2] <- "Diversity"
colnames(funct)[2] <- "Diversity"

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

data_melt <- melt(data, 
                  id.vars = c("DOY", "Diversity", "type"),
                  measure.vars = c("Slope_Hill1", "Pgap", "cv_maximun_height"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("cv_maximun_height", "Pgap", "Slope_Hill1"),
                          labels = c("Height heterogeneity", "Gap probability", "Structural complexity"))

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

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

plot_comp <- scale_shape_manual("Plot composition", values = c(21, 24, 22),
                                guide = guide_legend(override.aes = list(size = 2,
                                                                         colour = "black",
                                                                         alpha = 1),
                                                     title.position = "top",
                                                     title.hjust = 0.5)) 

doy_color <- scale_color_viridis_c("Day of the Year",
                                   option = "mako",
                                   begin = 0,
                                   end = 0.85,
                                   guide = "none")

doy_fill <-   scale_fill_viridis_c("Day of the Year",
                                   option = "mako",
                                   begin = 0,
                                   end = 0.85,
                                   limits = c(95, 305),
                                   breaks = c(100, 200, 300))

alpha_point <- 0.15

# ------------------------------------------------------------------------------
# Diversity plots

plot <- ggplot(data_melt, 
               aes(x = Diversity, 
                   y = value,
                   color = DOY,
                   fill = DOY,
                   gruop = as.factor(DOY))) +
  geom_point(colour = "grey25", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size) +
  doy_color + doy_fill + 
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 4) +
  xlab(bquote(Species~richness))  +
  xlab(bquote(Species~richness~~~~~~italic(PD)~~~~~~italic(FD)))  +
  ylab(bquote(italic(d)[italic(D)]~~~~italic(P)[gap]~~~italic(CH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ type, scales = "free")

# ------------------------------------------------------------------------------
#Export figure

jpeg(paste0(root_path, "/Figure_4a.jpeg"), width = 210, height = 180, units = "mm", res = 600)

plot

dev.off()


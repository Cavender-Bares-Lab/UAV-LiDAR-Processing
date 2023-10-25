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

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Deciduous"]
frame[PA == 0, plot_type := "Evergreen"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "hill1_taxa", "Slope_Hill1", "Intercept_Hill1", "plot_type")]
phylo <- frame[, c("DOY", "hill1_phylo", "Slope_Hill1", "Intercept_Hill1", "plot_type")]
funct <- frame[, c("DOY", "hill1_qDTM", "Slope_Hill1", "Intercept_Hill1", "plot_type")]

colnames(taxa)[2] <- "hill1"
colnames(phylo)[2] <- "hill1"
colnames(funct)[2] <- "hill1"

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

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
ggplot(data, 
       aes(hill1, 
           Slope_Hill1,
           color = DOY,
           fill = DOY,
           gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
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
  coord_cartesian(ylim = c(1.5, 2.5), expand = TRUE) +
  #scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(trans = log10_trans()) +
  xlab(" ") +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ type, scales = "free_x")

ggplot(data, 
       aes(hill1, 
           exp(Intercept_Hill1),
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
  #coord_cartesian(ylim = c(1.5, 2.5), expand = TRUE) +
  #scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  xlab(" ") +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ type, scales = "free_x")


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

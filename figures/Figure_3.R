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
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "hill0_taxa", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA")]
phylo <- frame[, c("DOY", "hill0_phylo", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA")]
funct <- frame[, c("DOY", "hill0_FD_q", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA")]

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
                  id.vars = c("DOY", "Diversity", "plot_type", "type"),
                  measure.vars = c("Slope_Hill1", "Pgap", "cv_maximun_height"))

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

doy_color <- scale_color_carto_c("Day of the Year", 
                                 type = "diverging", 
                                 palette = "Fall",
                                 guide = "none")

doy_fill <-   scale_fill_carto_c("Day of the Year", 
                                 type = "diverging", 
                                 palette = "Fall",
                                 limits = c(95, 305),
                                 breaks = c(100, 200, 300)) 

alpha_point <- 0.15

# ------------------------------------------------------------------------------
# Diversity plots
fractal <- ggplot(data_melt[variable == "Slope_Hill1",], 
                  aes(x = Diversity, 
                      y = value,
                      color = DOY,
                      fill = DOY,
                      gruop = as.factor(DOY))) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 #formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               #formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  #plot_comp + doy_color + doy_fill + 
  doy_color + doy_fill + 
  coord_cartesian(ylim = c(1.40, 2.6), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 3, breaks = c(1.50, 2.00, 2.50), 
                     labels = c("1.5", "2.0", "2.5")) +
  xlab(bquote(Species~richness))  +
  #xlab(bquote(Species~richness~(*^q=0*italic(D)])))  +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Structural complexity" ~ type, scales = "free")

gap <- ggplot(data_melt[variable == "Pgap",], 
              aes(x = Diversity, 
                  y = value,
                  color = DOY,
                  fill = DOY,
                  gruop = as.factor(DOY))) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 #formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               #formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  #plot_comp + doy_color + doy_fill + 
  doy_color + doy_fill + 
  coord_cartesian(ylim = c(0, 1), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  xlab(bquote(italic(FD)))  +
  ylab(bquote(italic(P)[cover]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Gap probability" ~ type, scales = "free")

ch <- ggplot(data_melt[variable == "cv_maximun_height",], 
             aes(x = Diversity, 
                 y = value,
                 color = DOY,
                 fill = DOY,
                 gruop = as.factor(DOY))) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.1) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 #formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               #formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "left",
               label.y = "top",
               size = text_size) +
  #plot_comp + doy_color + doy_fill + 
  doy_color + doy_fill + 
  coord_cartesian(ylim = c(0, 2.8), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  #scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  #scale_y_continuous(trans = log10_trans()) +
  #annotation_logticks(sides = "l") +
  xlab(bquote(italic(PD)))  +
  ylab(bquote(italic(CH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid("Height heterogeneity" ~ type, scales = "free")

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


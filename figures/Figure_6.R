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
library(ggpmisc)

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

taxa <- frame[, c("DOY", "TD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA")]
phylo <- frame[, c("DOY", "PD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA")]
funct <- frame[, c("DOY", "FD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA")]

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

colnames(taxa)[2] <- "PSV"
colnames(phylo)[2] <- "PSV"
colnames(funct)[2] <- "PSV"

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

cv_metrics <- data[, .(CV_slope = sd(Slope_Hill1)/mean(Slope_Hill1),
                       CV_ch = sd(cv_maximun_height)/mean(cv_maximun_height),
                       CV_pgap = sd(Pgap)/mean(Pgap)), 
                   by = c("plot_new", "PSV", "plot_type", "type", "PA")]
cv_metrics <- cv_metrics[!is.na(PSV), ]

cv_metrics <- melt(cv_metrics, 
                  id.vars = c("plot_new", "plot_type", "type", "PSV", "PA"),
                  measure.vars = c("CV_slope", "CV_ch", "CV_pgap"))

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

colour_PA <- scale_fill_viridis("Proportion of Angiosperms",
                                option = "D",
                                direction = 1,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plots
cv_vol_dD <- ggplot(cv_metrics[variable == "CV_slope",], aes(PSV,
                                    value,
                                    fill = PA)) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = alpha_point) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 #formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5,
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               #formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "top",
               size = text_size) +
  colour_PA +
  #plot_comp + colour_PA +
  coord_cartesian(xlim = c(0.0, 1.0), expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab("Species variability") +
  ylab(bquote(italic(CV)~italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Structural complexity" ~ type, scales = "free")


cv_vol_Pgap <- ggplot(cv_metrics[variable == "CV_pgap",], aes(PSV,
                                                value,
                                                fill = PA)) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = alpha_point) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
               se = FALSE,
               formula = y ~ x,
               #formula = y ~ poly(x, 2, raw = TRUE),
               linewidth = 0.5,
               colour = "black",
               linetype = "dotted") +
  stat_poly_eq(use_label(c("eq", "R2")),
             method = "lm",
             formula = y ~ x,
             #formula = y ~ poly(x, 2, raw = TRUE),
             label.x = "right",
             label.y = "top",
             size = text_size) +
  colour_PA +
  #plot_comp + colour_PA +
  coord_cartesian(xlim = c(0.0, 1.0), expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(bquote(italic(CV)~italic(P)[gap])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Gap probability" ~ type, scales = "free")

cv_vol_CH <-ggplot(cv_metrics[variable == "CV_ch",], aes(PSV,
                                               value,
                                               fill = PA)) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = alpha_point) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 #formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5,
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               #formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "top",
               size = text_size) +
  colour_PA +
  #plot_comp + colour_PA +
  coord_cartesian(xlim = c(0.0, 1.0), expand = TRUE) +
  scale_x_continuous(breaks = c(0.0, 0.5, 1.0), labels = c("0.0", "0.5", "1.0")) +
  #scale_y_continuous(n.breaks = 4) + 
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab(" ") +
  ylab(bquote(italic(CV)~italic(CH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Height heterogeneity" ~ type, scales = "free")

# ------------------------------------------------------------------------------
#Merge panels
Figure_6 <- ggarrange(cv_vol_CH,
                      cv_vol_Pgap,
                      cv_vol_dD,
                      ncol = 1, nrow = 3,  align = "hv", 
                      common.legend = TRUE)
#Export figure
jpeg(paste0(root_path, "/Figure_6.jpeg"), width = 210, height = 210, units = "mm", res = 600)

Figure_6

dev.off()


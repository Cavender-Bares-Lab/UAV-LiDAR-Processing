################################################################################
#' @title Effect of multiple dimensions of variability on the structural stability
################################################################################

#' @description Figure 7 to test the effect of multiple dimensions of variability 
#' on the seasonal structural stability of LiDAR metrics
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(ggplot2)
library(scales)
library(ggpmisc)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "TD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]
phylo <- frame[, c("DOY", "PD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]
funct <- frame[, c("DOY", "FD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

colnames(taxa)[2] <- "PSV"
colnames(phylo)[2] <- "PSV"
colnames(funct)[2] <- "PSV"

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_ch = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_pgap = mean(Pgap)/sd(Pgap)), 
                   by = c("plot_new", "PSV", "type", "PA")]
ss_metrics <- ss_metrics[!is.na(PSV), ]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "type", "PSV", "PA"),
                  measure.vars = c("SS_slope", "SS_ch", "SS_pgap"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("SS_ch", "SS_pgap", "SS_slope"),
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

colour_PA <- scale_fill_viridis("Proportion of Angiosperms",
                                option = "D",
                                direction = 1,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plots

plot <- ggplot(data_melt, aes(PSV,
                              value,
                              fill = PA)) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 linetype = "dotted",
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size) +
  colour_PA +  
  coord_cartesian(xlim = c(0.0, 1.0), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab("Taxonomic variability      Phylogenetic variability       Functional variability") +
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(P)[gap]]~~~italic(SS)[italic(CH)[CV]])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid(LiDAR ~ type, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_7a.jpeg"), width = 210, height = 180, units = "mm", res = 600)

plot

dev.off()
  

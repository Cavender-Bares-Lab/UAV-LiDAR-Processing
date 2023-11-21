################################################################################
#' @title Effect of diversity on the seasonal structural stability
################################################################################

#' @description Figure 4 to test the effect of diversity on the seasonal structural
#' stability of LiDAR metrics
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(rcartocolor)
library(ggplot2)
library(scales)
library(ggpmisc)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Working path

frame <- fread(paste0(root_path, "/master_clean.csv"))

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "hill0_taxa", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]
phylo <- frame[, c("DOY", "hill0_phylo", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]
funct <- frame[, c("DOY", "hill0_FD_q", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA")]

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

colnames(taxa)[2] <- "diversity"
colnames(phylo)[2] <- "diversity"
colnames(funct)[2] <- "diversity"

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

ss_metrics <- data[, .(SS_slope = 1/(sd(Slope_Hill1)/mean(Slope_Hill1)),
                       SS_ch = 1/(sd(cv_maximun_height)/mean(cv_maximun_height)),
                       SS_pgap = 1/(sd(Pgap)/mean(Pgap))), 
                   by = c("plot_new", "diversity", "type", "PA")]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "diversity", "PA", "type"),
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

plot <- ggplot(data_melt, aes(diversity,
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
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  colour_PA +  
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l") +
  xlab(bquote(Species~richness~~~italic(PD)~~~italic(FD))) +
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(P)[gap]]~~~italic(SS)[italic(CH)[CV]])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid(LiDAR ~ type, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_4.jpeg"), width = 210, height = 180, units = "mm", res = 600)

plot

dev.off()


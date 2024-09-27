################################################################################
#' @title Effect of multiple dimensions of variability on the structural stability
################################################################################

#' @description Figure 7 to test the effect of multiple dimensions of variability 
#' on the seasonal structural stability of LiDAR metrics
#' 
#' @return A png file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(ggplot2)
library(scales)
library(ggpmisc)
library(ggdark)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean (2024-09-19).csv"))

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "TD_PSV", "Slope_Hill1", "FC", "cv_maximun_height", "plot_new", "PA")]
phylo <- frame[, c("DOY", "PD_PSV", "Slope_Hill1", "FC", "cv_maximun_height", "plot_new", "PA")]
funct <- frame[, c("DOY", "FD_PSV", "Slope_Hill1", "FC", "cv_maximun_height", "plot_new", "PA")]

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
                       SS_hh = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_fc = mean(FC)/sd(FC)), 
                   by = c("plot_new", "PSV", "type", "PA")]
ss_metrics <- ss_metrics[!is.na(PSV), ]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "type", "PSV", "PA"),
                  measure.vars = c("SS_slope", "SS_hh", "SS_fc"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("SS_hh", "SS_fc", "SS_slope"),
                          labels = c("Height heterogeneity", "Fractional cover", "Structural complexity"))

# ------------------------------------------------------------------------------
# Plot details
th_black <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position= c("top"), 
        legend.background = element_rect(fill= "transparent"),
        panel.background = element_rect(fill= "transparent"),
        rect = element_rect(fill = "transparent"),
        plot.margin = margin(4, 4, 0, 1, "pt"),
        
        strip.background = element_rect(color="black", 
                                        fill="black", 
                                        linewidth=1.5, 
                                        linetype="solid"),
        strip.text = element_text(color = "white"))

th_trans <- dark_theme_bw(base_size = 11) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position= c("top"),
        legend.background = element_rect(fill= "transparent"),
        panel.background = element_rect(fill = "transparent"),
        rect = element_rect(fill = "transparent"),
        legend.title = element_text(colour="white"),
        legend.text = element_text(color = "white"),
        axis.text = element_text(color="white"),
        plot.background = element_rect(colour = "transparent",
                                       fill = "transparent",
                                       linewidth=0),
        plot.margin = margin(4, 4, 0, 1, "pt"),
        strip.background = element_rect(color="white", 
                                        fill="white", 
                                        linewidth=1.5, 
                                        linetype="solid"),
        strip.text = element_text(color = "black"))

th <- th_trans
line_col <- "white"

th <- th_black
line_col <- "black"

tamano <- 12
tamano2 <- 10
text_size <- 2.8

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

colour_PA <- scale_fill_viridis("Proportion of Angiosperms",
                                option = "D",
                                direction = 1,
                                begin = 0,
                                end = 0.975,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plots

plot <- ggplot(data_melt, aes(PSV,
                              value,
                              fill = PA)) +
  geom_point(colour = "grey28", alpha = alpha_point, shape = 21, size = 1.8) +
  stat_poly_line(method = "lm",
                 #se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 #linetype = "dotted",
                 colour = line_col) +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = text_size,
               colour = line_col) +
  colour_PA +  
  coord_cartesian(xlim = c(0.0, 1.0), expand = TRUE) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l",
                      colour = line_col) +
  xlab("Taxonomic variability      Phylogenetic variability       Functional variability") +
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(FC)]~~~italic(SS)[italic(HH)[CV]])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid(LiDAR ~ type, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figures/Figure_7_b.jpeg"), 
    width = 210, 
    height = 180, 
    units = "mm", 
    res = 600,
    bg = "transparent")

plot

dev.off()
  

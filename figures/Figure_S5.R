################################################################################
#' @title Comparison of seasonal structural stability 
################################################################################

#' @description Figure S5 that compares the seasonal structural stability among plots
#' composed of angiosperms, mixtures (angiosperms and gymnosperms), and gymnosperms.
#' 
#' @return A png file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggpmisc)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean (2024-09-19).csv"))

#' -----------------------------------------------------------------------------
#' Data reshaping

# Define plots
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

# Select columns of interest
data <- frame[, c("plot_new", "plot_type", "Block", "PA", 
                  "Slope_Hill1", "FC", "cv_maximun_height")]

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_hh = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_fc = mean(FC)/sd(FC)), 
                   by = c("plot_new", "plot_type", "Block", "PA")]

ss_metrics$plot_type <- as.factor(ss_metrics$plot_type)
ss_metrics$plot_type <- factor(ss_metrics$plot_type, levels = c("Angiosperms", 
                                                                "Mixture", 
                                                                "Gymnosperms"))

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "plot_type", "Block", "PA"),
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

#-------------------------------------------------------------------------------
# Figure S4

plot <- ggplot(data_melt,
             aes(x = plot_type,
                 y = value,
                 fill = plot_type)) +
  geom_point(shape = 21, 
             size = 1, 
             position = position_jitterdodge(), 
             color = "white", 
             alpha = 0.8) +
  geom_violin(alpha = 0.4, 
              position = position_dodge(width = .75), 
              linewidth = 0.5, 
              color = "black") +
  geom_boxplot(width=0.05, 
               color = "white", 
               alpha = .8, 
               outlier.shape = NA) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "l",
                      colour = line_col) +
  xlab("Community composition") + 
  scale_fill_manual("Composition", values = c("#d95f02", 
                                              "#7570b3",
                                              "#1b9e77")) + 
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(FC)]~~~italic(SS)[italic(HH)[CV]])) +
  theme_bw(base_size = tamano) + th + 
  facet_grid(LiDAR ~ ., scales = "free")

#Export figure
png(paste0(root_path, "/Figures/Figure_S5.png"), 
    width = 130, 
    height = 180, 
    units = "mm", 
    res = 600,
    bg = "transparent")

plot

dev.off()

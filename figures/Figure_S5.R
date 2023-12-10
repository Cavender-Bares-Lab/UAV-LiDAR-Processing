################################################################################
#' @title Comparison of sesonal structural stability 
################################################################################

#' @description Figure S5 that compares the sesonal structural stability among plots
#' composed of angiosperms, mixtures (angiosperms and gymnosperms), and gymnosperms.
#' 
#' @return A jpeg file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(rcartocolor)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggpmisc)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))

#' -----------------------------------------------------------------------------
#' Data reshaping

# Define plots
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

# Select columns of interest
data <- frame[, c("plot_new", "plot_type", "Block", "PA", 
                  "Slope_Hill1", "Pgap", "cv_maximun_height")]

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_ch = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_pgap = mean(Pgap)/sd(Pgap)), 
                   by = c("plot_new", "plot_type", "Block", "PA")]

ss_metrics$plot_type <- as.factor(ss_metrics$plot_type)
ss_metrics$plot_type <- factor(ss_metrics$plot_type, levels = c("Angiosperms", 
                                                                "Mixture", 
                                                                "Gymnosperms"))

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "plot_type", "Block", "PA"),
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

#-------------------------------------------------------------------------------
# Figure S4

S4 <- ggplot(data_melt,
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
  annotation_logticks(sides = "l") +
  xlab("Community composition") + 
  scale_fill_manual("Composition", values = c("#d95f02", 
                                              "#7570b3",
                                              "#1b9e77")) + 
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(P)[gap]]~~~italic(SS)[italic(CH)[CV]])) +
  theme_bw(base_size = tamano) + th + 
  facet_grid(LiDAR ~ ., scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_S5.jpeg"), width = 130, height = 180, units = "mm", res = 600)

S4

dev.off()

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
options(scipen = 99999)

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

vol <- frame[, c("DOY", "volume", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type", "PA", "plot_new")]
AWP <- frame[, c("DOY", "total_AWP", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type", "PA", "plot_new")]
sigmaAWPD <- frame[, c("DOY", "sd_AWP", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type", "PA", "plot_new")]

vol$type <- "Stand volume"
AWP$type <- "Stand productivity"
sigmaAWPD$type <- "Growth variability" 

colnames(vol)[2] <- "metric"
colnames(AWP)[2] <- "metric"
colnames(sigmaAWPD)[2] <- "metric"

data <- rbind(vol, AWP, sigmaAWPD)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Stand volume", "Stand productivity", "Growth variability"))

cv_metrics <- data[, .(CV_slope = sd(Slope_Hill1)/mean(Slope_Hill1),
                       CV_ch = sd(mean_maximun_height)/mean(mean_maximun_height),
                       CV_pgap = sd(Pgap)/mean(Pgap)), 
                   by = c("plot_new", "metric", "plot_type", "type", "PA")]

cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "plot_type", "type", "metric", "PA"),
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

# ------------------------------------------------------------------------------
# Plot
db <- ggplot(cv_metrics[variable == "CV_slope"], aes(metric,
                                                     value,
                                                     fill = PA)) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               formula = y ~ x,
               linewidth = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             formula = y ~ x,
             label.x = "right",
             label.y = "top",
             size = text_size) +
  colour_PA + plot_comp + 
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(Wood~volume~(m^3))) + 
  ylab(bquote(italic(CV)~italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Structural Complexity" ~ type, scales = "free")

pgap <- ggplot(cv_metrics[variable == "CV_pgap"], aes(metric,
                                                      value,
                                                      fill = PA)) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               formula = y ~ x,
               linewidth = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             formula = y ~ x,
             label.x = "right",
             label.y = "bottom",
             size = text_size) +
  colour_PA + plot_comp +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(AWD[plot]~(m^3~y^-1))) + 
  ylab(bquote(italic(CV)~italic(P)[gap])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Cover" ~ type, scales = "free")

ch <- ggplot(cv_metrics[variable == "CV_ch"], aes(metric,
                                                  value,
                                                  fill = PA)) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  stat_ma_line(method = "SMA",
               se = TRUE,
               formula = y ~ x,
               linewidth = 0.5,
               colour = "black") +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             formula = y ~ x,
             label.x = "right",
             label.y = "top",
             size = text_size) +
  colour_PA + plot_comp +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(sigma*AWD[tree]~(m^3~y^-1))) + 
  ylab(bquote(italic(CV)~bar(italic(CH)))) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Canopy height" ~ type, scales = "free")

# ------------------------------------------------------------------------------
#Merge panels
Figure_2 <- ggarrange(ch, pgap, db,
                      ncol = 1, nrow = 3,  align = "hv", 
                      common.legend = TRUE)
#Export figure
jpeg(paste0(root_path, "/Figure_2.jpeg"), width = 210, height = 210, units = "mm", res = 600)

Figure_2

dev.off()



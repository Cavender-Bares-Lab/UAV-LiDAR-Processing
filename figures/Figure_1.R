################################################################################
#' @title Effect of biomass, tree size inequality, and PA on FSC.
################################################################################

#' @description Effect of biomass, tree size inequality, and PA on FSC
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
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Data reshaping


vol <- frame[, c("DOY", "volume", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type")]
AWP <- frame[, c("DOY", "total_AWP", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type")]
sigmaAWPD <- frame[, c("DOY", "sd_AWP", "Slope_Hill1", "Pgap", "mean_maximun_height", "plot_type")]

vol$type <- "Stand volume"
AWP$type <- "Stand productivity"
sigmaAWPD$type <- "Growth variability" 

colnames(vol)[2] <- "metric"
colnames(AWP)[2] <- "metric"
colnames(sigmaAWPD)[2] <- "metric"

data <- rbind(vol, AWP, sigmaAWPD)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Stand volume", "Stand productivity", "Growth variability"))

data_melt <- melt(data, 
                  id.vars = c("DOY", "metric", "plot_type", "type"),
                  measure.vars = c("Slope_Hill1", "Pgap", "mean_maximun_height"))


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
# Panels
db <- ggplot(data_melt[variable == "Slope_Hill1"], 
             aes(x = metric,
                 y = value, 
                 color = DOY,
                 fill = DOY,
                 gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = alpha_point) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 #formula = y ~ x,
                 formula = y ~ poly(x, 2, raw = TRUE),
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               #formula = y ~ x,
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  plot_comp + doy_color + doy_fill + 
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3, breaks = c(1.50, 2.00, 2.50), 
                     labels = c("1.5", "2.0", "2.5")) +
  annotation_logticks(sides = "b") +
  xlab(bquote(Wood~volume~(m^3))) + 
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Structural Complexity" ~ type, scales = "free")

pgap <- ggplot(data_melt[variable == "Pgap"], 
               aes(x = metric,
                   y = value, 
                   color = DOY,
                   fill = DOY,
                   gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = alpha_point) +
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
  plot_comp + doy_color + doy_fill + 
  coord_cartesian(ylim = c(0, 1), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3, breaks = c(0.0, 0.5, 1.0), 
                     labels = c("0.0", "0.5", "1.0")) +
  annotation_logticks(sides = "b") +
  xlab(bquote(AWD[plot]~(m^3~y^-1))) + 
  ylab(bquote(italic(P)[gap])) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Cover" ~ type, scales = "free")

ch <- ggplot(data_melt[variable == "mean_maximun_height"], 
             aes(x = metric,
                 y = value, 
                 color = DOY,
                 fill = DOY,
                 gruop = as.factor(DOY))) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = alpha_point) +
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
  plot_comp + doy_color + doy_fill + 
  coord_cartesian(ylim = c(0, 4.5), expand = TRUE) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3, breaks = c(0.0, 2.0, 4.0), 
                     labels = c("0.0", "2.0", "4.0")) +
  annotation_logticks(sides = "b") +
  xlab(bquote(sigma*AWD[tree]~(m^3~y^-1))) + 
  ylab(bquote(bar(italic(CH))~(m))) +
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid("Canopy height" ~ type, scales = "free")

#-------------------------------------------------------------------------------
#Merge panels

Figure_1 <- ggarrange(ch, pgap, db,
                      ncol = 1, nrow = 3,  align = "hv", 
                      common.legend = TRUE)
#Export figure
jpeg(paste0(root_path, "/Figure_1.jpeg"), width = 210, height = 210, units = "mm", res = 600)

Figure_1

dev.off()
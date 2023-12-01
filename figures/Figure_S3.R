################################################################################
#' @title Temporal variation of the fractal geometry
################################################################################

#' @description Figure that shows the temporal variation of the fractal geometry
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

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Data reshaping

reshaping <- frame
reshaping_ch <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                               "cv_maximun_height")]
reshaping_pgap <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                                "Pgap")]
reshaping_frac <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                                "Slope_Hill1")]

reshaping_ch <- melt(reshaping_ch, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_ch$parameter <- "Height heterogeneity"

reshaping_pgap <- melt(reshaping_pgap, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_pgap$parameter <- "Gap probability"

reshaping_frac <- melt(reshaping_frac, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_frac$parameter <- "Structural complexity"

reshaping <- rbind(reshaping_ch, reshaping_pgap, reshaping_frac)

reshaping$parameter <- as.factor(reshaping$parameter)
reshaping$parameter <- factor(reshaping$parameter, levels = c("Height heterogeneity", 
                                                              "Gap probability",
                                                              "Structural complexity"))

reshaping$plot_type <- as.factor(reshaping$plot_type)
reshaping$plot_type <- factor(reshaping$plot_type, levels = c("Angiosperms", 
                                                              "Mixture",
                                                              "Gymnosperms"))

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

fill_PA <- scale_fill_viridis("Proportion of Angiosperms",
                                option = "D",
                                direction = 1,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

colour_PA <- scale_colour_viridis("Proportion of Angiosperms",
                                option = "D",
                                direction = 1,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

# Figure S1

S1 <- ggplot(reshaping,
       aes(x = DOY,
           y = value,
           colour = PA,
           fill = PA,
           gruop = as.factor(plot_new))) +
  geom_point(alpha = 0.1) +
  geom_line(alpha = 0.2) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_colour_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_y_continuous(n.breaks = 4) +
  xlab("Day of the year ") +
  ylab(bquote(italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(parameter ~ plot_type, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_S1.jpeg"), width = 210, height = 150, units = "mm", res = 600)

S1

dev.off()


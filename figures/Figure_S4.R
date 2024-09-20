################################################################################
#' @title Temporal variation of the fractal geometry
################################################################################

#' @description Figure S4 that shows the temporal variation of the fractal geometry
#' 
#' @return A png file

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

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean (2024-09-19).csv"))
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Data reshaping

reshaping <- frame
reshaping_ch <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                              "cv_maximun_height")]
reshaping_fc <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                                "FC")]
reshaping_frac <- reshaping[, c("plot_new", "PA", "plot_type", "DOY", "volume",
                                "Slope_Hill1")]

reshaping_ch <- melt(reshaping_ch, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_ch$parameter <- "Height heterogeneity"

reshaping_fc <- melt(reshaping_fc, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_fc$parameter <- "Fractional cover"

reshaping_frac <- melt(reshaping_frac, id.vars = c("plot_new", "PA", "plot_type", "DOY", "volume"))
reshaping_frac$parameter <- "Structural complexity"

reshaping <- rbind(reshaping_ch, reshaping_fc, reshaping_frac)

reshaping$parameter <- as.factor(reshaping$parameter)
reshaping$parameter <- factor(reshaping$parameter, levels = c("Height heterogeneity", 
                                                              "Fractional cover",
                                                              "Structural complexity"))

reshaping$plot_type <- as.factor(reshaping$plot_type)
reshaping$plot_type <- factor(reshaping$plot_type, levels = c("Angiosperms", 
                                                              "Mixture",
                                                              "Gymnosperms"))

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

pa_color <- scale_colour_gradientn("Proportion of Angiosperms",
                                    colours = rev(brewer.pal(9, "PuOr")[-5]),
                                    limits = c(0.0, 1.0),
                                    breaks = c(0.0, 0.5, 1.0),
                                    guide = "none")

pa_fill <- scale_fill_gradientn("Proportion of Angiosperms",
                                   colours = rev(brewer.pal(9, "PuOr")[c(-5)]),
                                   limits = c(0, 1),
                                   breaks = c(0.0, 0.5, 1.0))

# Figure S1

plot <- ggplot(reshaping,
             aes(x = DOY,
                 y = value,
                 colour = PA,
                 fill = PA,
                 gruop = as.factor(plot_new))) +
  geom_point(alpha = 0.1) +
  geom_line(alpha = 0.2) +
  pa_color +
  pa_fill + 
  scale_y_continuous(n.breaks = 4) +
  xlab("Day of the year ") +
  ylab(bquote(italic(d)[italic(D)]~~~~italic(FC)~~~italic(HH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(parameter ~ plot_type, scales = "free")

#Export figure
png(paste0(root_path, "/Figures/Figure_S4.png"), 
    width = 210, 
    height = 150, 
    units = "mm", 
    res = 600,
    bg = "transparent")

plot

dev.off()


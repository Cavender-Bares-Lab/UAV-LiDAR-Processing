################################################################################
#' @title Extended figure S3
################################################################################

#' @description Extended figure S3 providing the statistics associated with the 
#' linear regressions
#' 
#' @return A png file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
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
#' Data reshaping

data <- frame[, c("plot_new", "PA", "Block", "DOY", "volume",
                  "Slope_Hill1", "FC", "cv_maximun_height")]

data_melt <- melt(data,
                  id.vars = c("DOY", "volume"),
                  measure.vars = c("Slope_Hill1", "FC", "cv_maximun_height"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("cv_maximun_height", "FC", "Slope_Hill1"),
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

doy_color <- scale_colour_gradientn("Day of the Year",
                                    colours = rev(brewer.pal(9, "PRGn")[-5]),
                                    limits = c(95, 305),
                                    breaks = c(100, 200, 300),
                                    guide = "none")

doy_fill <-   scale_fill_gradientn("Day of the Year",
                                   colours = rev(brewer.pal(9, "PRGn")[-5]),
                                   limits = c(95, 305),
                                   breaks = c(100, 200, 300))

alpha_point <- 0.75

# ------------------------------------------------------------------------------
# Plot

plot <- ggplot(data_melt,
               aes(x = volume,
                   y = value,
                   colour = DOY,
                   fill = DOY,
                   gruop = as.factor(DOY))) +
  #geom_point(alpha = alpha_point, 
  #           shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               use_label(c("R2", "F", "P")),
               formula = y ~ x,
               label.x = "right", #left
               label.y = "bottom", #bottom
               size = text_size) +
  doy_color + 
  doy_fill +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3) +
  annotation_logticks(sides = "b",
                      colour = line_col) +
  xlab(bquote(Wood~volume~(m^3~ha^-1))) +
  ylab(bquote(italic(d)[italic(D)]~~~~italic(FC)~~~italic(HH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ ., scales = "free")

# Export figure
jpeg(paste0(root_path, "/Figures/Figure_S3_a.jpeg"), 
    width = 90, 
    height = 180, 
    pointsize = 12,
    units = "mm", 
    res = 600,
    bg = "transparent")

plot

dev.off()

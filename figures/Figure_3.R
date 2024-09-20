################################################################################
#' @title Effect of volume on the seasonal structural stability
################################################################################

#' @description Figure 3 to test the effect of volume on the seasonal structural
#' stability of LiDAR metrics
#' 
#' @return A png file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(ggplot2)
library(ggpmisc)
library(scales)
library(ggpubr)
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

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_ch = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_hh = mean(FC)/sd(FC)), 
                   by = c("plot_new", "volume", "PA")]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "volume", "PA"),
                  measure.vars = c("SS_slope", "SS_ch", "SS_hh"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("SS_ch", "SS_hh", "SS_slope"),
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

#th <- th_black
#line_col <- "black"

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
# Plot

plot <- ggplot(data_melt,
               aes(volume,
                   value,
                   fill = PA)) +
  geom_point(colour = "grey25", alpha = alpha_point, shape = 21, size = 1.8) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x, #exp(x)
                 linewidth = 0.5,
                 linetype = "dotted", #dotted
                 colour = line_col) +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size,
               colour = line_col) +
  colour_PA +  
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl",
                      colour = line_col) +
  xlab(bquote(Wood~volume~(m^3~ha^-1))) + 
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(FC)]~~~italic(SS)[italic(HH)[CV]])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ ., scales = "free")

#Export figure
png(paste0(root_path, "/Figures/Figure_3_trans_b.png"),
    width = 90, 
    height = 180,
    units = "mm",
    res = 600,
    bg = "transparent")

plot

dev.off()

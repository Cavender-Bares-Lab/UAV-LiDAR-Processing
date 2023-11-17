################################################################################
#' @title Relationship between overyilding and LiDAR seasonal stability
################################################################################

#' @description Figure 7 to test the effect of LiDAR seasonal stability on overyilding
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(rcartocolor)
library(ggplot2)
library(scales)
library(ggpmisc)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))

#' -----------------------------------------------------------------------------
#' Data reshaping

data <- frame[, c("plot_new", "SR_real", "PA", "Block", "DOY", "overyielding",
                  "Slope_Hill1", "Pgap", "cv_maximun_height")]

cv_metrics <- data[, .(CV_slope = sd(Slope_Hill1)/mean(Slope_Hill1),
                       CV_ch = sd(cv_maximun_height)/mean(cv_maximun_height),
                       CV_pgap = sd(Pgap)/mean(Pgap)), 
                   by = c("plot_new", "Block", "SR_real", "overyielding", "PA")]

data_melt <- melt(cv_metrics, 
                  id.vars = c("plot_new", "Block", "SR_real", "overyielding", "PA"),
                  measure.vars = c("CV_slope", "CV_ch", "CV_pgap"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("CV_ch", "CV_pgap", "CV_slope"),
                          labels = c("Height heterogeneity", "Gap probability", "Structural complexity"))

data_melt <- data_melt[SR_real > 1,]

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

colour_PA <- scale_fill_viridis("Proportion of Angiosperms",
                                option = "D",
                                direction = 1,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plot

plot <- ggplot(data_melt,
               aes(value,
                   overyielding,
                   fill = PA)) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 linetype = "solid",
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size) +
  colour_PA +  
  scale_x_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "b") +
  xlab(bquote(italic(CV)~italic(CH)[CV]~~~~italic(CV)~italic(P)[gap]~~~italic(CV)~italic(d)[italic(D)])) +
  ylab(bquote(Overyilding~(m^3~y^-1))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ LiDAR, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_7.jpeg"), width = 180, height = 90, units = "mm", res = 600)

plot

dev.off()

################################################################################
#' @title Effect of volume on the seasonal structural stability
################################################################################

#' @description Figure 2 to test the effect of volume on the seasonal structural
#' stability of LiDAR metrics
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
library(ggplot2)
library(scales)
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

data <- frame[, c("plot_new", "PA", "Block", "DOY", "volume",
                  "Slope_Hill1", "Pgap", "cv_maximun_height")]

ss_metrics <- data[, .(SS_slope = 1/(sd(Slope_Hill1)/mean(Slope_Hill1)),
                       SS_ch = 1/(sd(cv_maximun_height)/mean(cv_maximun_height)),
                       SS_pgap = 1/(sd(Pgap)/mean(Pgap))), 
                   by = c("plot_new", "volume", "PA")]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "volume", "PA"),
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
               aes(volume,
                   value,
                   fill = PA)) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 linetype = "dotted",
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size) +
  colour_PA +  
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(Wood~volume~(m^3))) + 
  ylab(bquote(italic(SS)[italic(d)[italic(D)]]~~~~italic(SS)[italic(P)[gap]]~~~italic(SS)[italic(CH)[CV]])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ ., scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_2a.jpeg"), width = 90, height = 180, units = "mm", res = 600)

plot

dev.off()


################################################################################
#' @title Net Biodiveristy Effect and seasonal stability
################################################################################

#' @description Phenological effect on FSC and their association with 
#' plot metrics
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
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
NBE <- fread(paste0(root_path, "/plot_NBE.csv"))

#' -----------------------------------------------------------------------------
#' Data reshaping
#' 

LiDAR <- frame[, c("DOY", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_type", "plot_new", "PA", "SR_real")]

cv_metrics <- LiDAR[, .(CV_slope = sd(Slope_Hill1)/mean(Slope_Hill1),
                       CV_ch = sd(cv_maximun_height)/mean(cv_maximun_height),
                       CV_pgap = sd(Pgap)/mean(Pgap)), 
                   by = c("plot_new", "plot_type", "PA", "SR_real")]

LiDAR_NBE <- merge(cv_metrics, NBE, by = c("plot_new"), all.x = TRUE, all.y = FALSE)

LiDAR_NBE <- LiDAR_NBE[species_richness > 1, ]

plot(LiDAR_NBE$NBE ~ log(LiDAR_NBE$CV_pgap))
abline(lm(LiDAR_NBE$NBE ~ log(LiDAR_NBE$CV_pgap)))
summary(lm(LiDAR_NBE$NBE ~ log(LiDAR_NBE$CV_pgap)))
        
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "plot_type", "type", "metric", "PA"),
                   measure.vars = c("CV_slope", "CV_ch", "CV_pgap"))

cv_metrics[variable == "CV_ch", variable := "Height heterogeneity"]
cv_metrics[variable == "CV_pgap", variable := "Gap probability"]
cv_metrics[variable == "CV_slope", variable := "Structural complexity"]

cv_metrics$variable <- as.factor(cv_metrics$variable)
cv_metrics$variable <- factor(cv_metrics$variable, levels = c("Height heterogeneity",
                                                              "Gap probability",
                                                              "Structural complexity"))

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

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plot
ch <- ggplot(cv_metrics[variable == "Height heterogeneity"], aes(value,
                                                                 metric,
                                                                 fill = PA)) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 colour = "black",
                 linetype = "dotted") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  colour_PA +  
  #colour_PA + plot_comp + 
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(italic(CV)~italic(CH)[CV]))  +
  ylab(bquote(AWP[plot]~(m^3~y^-1))) + 
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid(type ~ "Height heterogeneity", scales = "free")

pgap <- ggplot(cv_metrics[variable == "Gap probability"], aes(value,
                                                              metric,
                                                              fill = PA)) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  colour_PA +  
  #colour_PA + plot_comp + 
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(italic(CV)~italic(P)[gap])) +
  ylab(bquote(sigma*AWP[tree]~(m^3~y^-1))) + 
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid(type ~ "Gap probability", scales = "free")

db <- ggplot(cv_metrics[variable == "Structural complexity"], aes(value,
                                                                  metric,
                                                                  fill = PA)) +
  #geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  colour_PA +  
  #colour_PA + plot_comp + 
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(italic(CV)~italic(d)[italic(D)])) +
  ylab(bquote(sigma*AWP[tree]~(m^3~y^-1))) + 
  theme_bw(base_size = tamano) +
  th + gui + 
  facet_grid(type ~ "Structural complexity", scales = "free")

# ------------------------------------------------------------------------------
#Merge panels
Figure_7 <- ggarrange(ch, pgap, db,
                      ncol = 3, nrow = 1,  align = "hv", 
                      common.legend = TRUE)
#Export figure
jpeg(paste0(root_path, "/Figure_7.jpeg"), width = 240, height = 130, units = "mm", res = 600)

Figure_7

dev.off()

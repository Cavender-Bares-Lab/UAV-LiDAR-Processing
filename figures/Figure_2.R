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
#' Data reshaping

reshaping <- frame
reshaping$height_hill0 <- exp(reshaping$Intercept_Hill0 + (log(1/reshaping$mean_maximun_height)*reshaping$Slope_Hill0))
reshaping$height_hill1 <- exp(reshaping$Intercept_Hill1 + (log(1/reshaping$mean_maximun_height)*reshaping$Slope_Hill1))
reshaping$height_hill2 <- exp(reshaping$Intercept_Hill2 + (log(1/reshaping$mean_maximun_height)*reshaping$Slope_Hill2))

reshaping_int <- reshaping[, c("plot_type", "DOY", "volume",
                               "Intercept_Hill0", "Intercept_Hill1", "Intercept_Hill2", "PA")]
reshaping_frac <- reshaping[, c("plot_type", "DOY", "volume",
                                "Slope_Hill0", "Slope_Hill1", "Slope_Hill2", "PA")]
reshaping_height <- reshaping[, c("plot_type", "DOY", "volume",
                                  "height_hill0", "height_hill1", "height_hill2", "PA")]

reshaping_int <- melt(reshaping_int, id.vars = c("plot_type", "DOY", "volume", "PA"))
reshaping_int$parameter <- "intercept"
reshaping_frac <- melt(reshaping_frac, id.vars = c("plot_type", "DOY", "volume", "PA"))
reshaping_frac$parameter <- "slope"
reshaping_height <- melt(reshaping_height, id.vars = c("plot_type", "DOY", "volume", "PA"))
reshaping_height$parameter <- "ENVh"

reshaping <- rbind(reshaping_int, reshaping_frac, reshaping_height)
reshaping[variable == "Intercept_Hill0" | variable == "Slope_Hill0" | variable == "height_hill0", qhill := "q0"]
reshaping[variable == "Intercept_Hill1" | variable == "Slope_Hill1" | variable == "height_hill1", qhill := "q1"]
reshaping[variable == "Intercept_Hill2" | variable == "Slope_Hill2" | variable == "height_hill2", qhill := "q2"]

reshaping$qhill <- as.factor(reshaping$qhill)
levels(reshaping$qhill) <- c(expression(paste(italic(q)," = 0")),
                             expression(paste(italic(q)," = 1")),
                             expression(paste(italic(q)," = 2")))

qhill_unique <- unique(reshaping$qhill)
reshaping <- subset(reshaping, qhill == qhill_unique[2])

reshaping_frac <- reshaping[parameter == "slope"]
reshaping_frac$parameter <- as.factor(reshaping_frac$parameter)
levels(reshaping_frac$parameter) <-  c("slope" = expression(italic('d')[italic(D)]))

reshaping_int <- reshaping[parameter == "intercept"]
reshaping_int$parameter <- as.factor(reshaping_int$parameter)
levels(reshaping_int$parameter) <-  c("intercept" = expression(ENV*italic(a)[italic(D)]))
reshaping_int$value <- exp(reshaping_int$value)

reshaping_ENVh <- reshaping[parameter == "ENVh"]
reshaping_ENVh$parameter <- as.factor(reshaping_ENVh$parameter)
levels(reshaping_ENVh$parameter) <-  c("ENVh" = expression(ENV*italic(h)[italic(D)]))

# ------------------------------------------------------------------------------
# Get phenological variability
cv_frac <- reshaping_frac[, .(CV = sd(value)/mean(value)), 
                          by = c("volume", "variable", "parameter", "qhill", "PA",
                                 "plot_type")]
cv_inter <- reshaping_int[, .(CV = sd(value)/mean(value)), 
                          by = c("volume", "variable", "parameter", "qhill", "PA",
                                 "plot_type")]
cv_ENVh <- reshaping_ENVh[, .(CV = sd(value)/mean(value)), 
                          by = c("volume", "variable", "parameter", "qhill", "PA",
                                 "plot_type")]

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

# Biomass

plot_cv_fract <- ggplot(cv_frac, aes(volume/1000000, 
                    CV,
                    fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
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
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(" ") +
  ylab(bquote(italic(CV)~italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ parameter, labeller = label_parsed)

plot_cv_inter <- ggplot(cv_inter, aes(volume/1000000, 
                    CV,
                    fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
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
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  #xlab(expression(paste("Wood volume (m"^3, ")"))) +
  xlab(" ") +
  ylab(bquote(italic(CV)~ENV*italic(a)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ parameter, labeller = label_parsed)

plot_cv_height <- ggplot(cv_ENVh, aes(volume/1000000, 
                     CV,
                     fill = PA)) +
  geom_point(shape = 21, colour = "grey", alpha = 0.8) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_fill_carto_c("Proportion of Angiosperms", 
                     type = "diverging", 
                     palette = "Earth",
                     direction = -1,
                     limits = c(0, 1),
                     breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  #xlab(expression(paste("Wood volume (m"^3, ")"))) +
  xlab(" ") +
  ylab(bquote(italic(CV)~ENV*italic(h)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ parameter, labeller = label_parsed)


# Other panel

colours_panel <- c("#1b9e77", "#d95f02", "#7570b3")

fract_comp <- ggplot(cv_frac, aes(volume/1000000, 
                    CV,
                    fill = plot_type,
                    colour = plot_type,
                    shape = plot_type)) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 alpha = 0.1) +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  scale_colour_manual("Plot composition", values = colours_panel, guide = FALSE) +
  scale_fill_manual("Plot composition", values = colours_panel, guide = FALSE) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              alpha = 1,
                                                              fill = colours_panel))) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(" ") +
  ylab(bquote(italic(CV)~italic(d)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ parameter, labeller = label_parsed)

int_comp <- ggplot(cv_inter, aes(volume/1000000, 
                    CV,
                    fill = plot_type,
                    colour = plot_type,
                    shape = plot_type)) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 alpha = 0.1) +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "bottom",
               size = text_size) +
  scale_colour_manual("Plot composition", values = colours_panel, guide = FALSE) +
  scale_fill_manual("Plot composition", values = colours_panel, guide = FALSE) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              alpha = 1,
                                                              fill = colours_panel))) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  xlab(expression(paste("Wood volume (m"^3, ")"))) +
  ylab(bquote(italic(CV)~ENV*italic(a)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ parameter, labeller = label_parsed)

ENVh_comp <- ggplot(cv_ENVh, aes(volume/1000000, 
                     CV,
                     fill = plot_type,
                     colour = plot_type,
                     shape = plot_type)) +
  geom_point(aes(shape = plot_type), colour = "grey", alpha = 0.8) +
  stat_poly_line(method = "lm",
                 se = TRUE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 alpha = 0.1) +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  scale_colour_manual("Plot composition", values = colours_panel, guide = FALSE) +
  scale_fill_manual("Plot composition", values = colours_panel, guide = FALSE) +
  scale_shape_manual("Plot composition", values = c(21, 24, 22),
                     guide = guide_legend(override.aes = list(size = 2,
                                                              alpha = 1,
                                                              fill = colours_panel))) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "bl") +
  #xlab(expression(paste("Wood volume (m"^3, ")"))) +
  xlab(" ") +
  ylab(bquote(italic(CV)~ENV*italic(h)[italic(D)]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ parameter, labeller = label_parsed)

# ------------------------------------------------------------------------------
#Merge panels
A <- ggarrange(plot_cv_fract, plot_cv_inter, plot_cv_height,
               ncol = 3, nrow = 1,  align = "hv", 
               widths = c(2, 2), 
               heights = c(2, 2, 2),
               labels = c("a", "b", "c"), 
               font.label = list(size = 14, 
                                 color = "black", 
                                 face = "plain", 
                                 family = NULL),
               label.x = 0.23,
               label.y = 0.90,
               common.legend = TRUE)

B <- ggarrange(fract_comp, int_comp, ENVh_comp,
                      ncol = 3, nrow = 1,  align = "hv", 
                      widths = c(2, 2), 
                      heights = c(2, 2, 2),
                      labels = c("d", "e", "f"), 
                      font.label = list(size = 14, 
                                        color = "black", 
                                        face = "plain", 
                                        family = NULL),
                      label.x = 0.23,
                      label.y = 0.90,
                      common.legend = TRUE)


Figure_2 <- ggarrange(A, B, ncol = 1, nrow = 2,
                      common.legend = FALSE)


#Export figure
jpeg("Figure_2.jpeg", width = 260, height = 210, units = "mm", res = 600)

Figure_2

dev.off()


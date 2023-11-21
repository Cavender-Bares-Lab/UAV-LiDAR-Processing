################################################################################
#' @title Effect forest volume on LiDAR-derived metrics
################################################################################

#' @description Figure 1 to test the effect of forest volume on LiDAR derived
#' metrics
#' 
#' @return A tiff file and statistical results

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(rcartocolor)
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

data_melt <- melt(data,
                  id.vars = c("DOY", "volume"),
                  measure.vars = c("Slope_Hill1", "Pgap", "cv_maximun_height"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("cv_maximun_height", "Pgap", "Slope_Hill1"),
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
# Plot

plot <- ggplot(data_melt,
               aes(x = volume,
                   y = value,
                   color = DOY,
                   fill = DOY,
                   gruop = as.factor(DOY))) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = text_size) +
  doy_color + doy_fill +
  scale_x_continuous(trans = log10_trans()) +
  annotation_logticks(sides = "b") +
  xlab(bquote(Wood~volume~(m^3))) +
  ylab(bquote(italic(d)[italic(D)]~~~~italic(P)[gap]~~~italic(CH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ ., scales = "free")

# Export figure
jpeg(paste0(root_path, "/Figure_1a.jpeg"), width = 90, height = 180, units = "mm", res = 600)

plot

dev.off()


# ------------------------------------------------------------------------------
# Statistical analysis

library(lme4)
library(sjPlot)
library(sjmisc)
library(stargazer)
library(report)
library(car)

data$DOY <- as.character(data$DOY)

#CHcv
ch <- lmer(cv_maximun_height ~ log(volume)*DOY + (1 | plot_new:Block),
           data = data)
qqnorm(resid(ch))
qqline(resid(ch))
report(ch)
Anova(ch, test.statistic="F")
tab_model(ch, p.val = "kr", show.df = TRUE)

#Pgap
Pgap <- lmer(Pgap ~ log(volume)*DOY + (1 | plot_new:Block),
           data = data)
qqnorm(resid(Pgap))
qqline(resid(Pgap))
report(Pgap)
Anova(Pgap, test.statistic="F")
tab_model(Pgap, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(Slope_Hill1 ~ log(volume)*DOY + (1 | plot_new:Block),
           data = data)
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)



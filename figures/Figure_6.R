################################################################################
#' @title Effect of multiple dimensions of variability on LiDAR derived metrics
################################################################################

#' @description Figure 6 describing the relationships between multiple dimensions 
#'of variability and LiDAR derived metrics
#' 
#' @return A png file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
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
#' Reshape frame

taxa <- frame[, c("DOY", "TD_PSV", "Slope_Hill1", "FC", "cv_maximun_height", "plot_new", "PA", "Block")]
phylo <- frame[, c("DOY", "PD_PSV", "Slope_Hill1", "FC", "cv_maximun_height", "plot_new", "PA", "Block")]
funct <- frame[, c("DOY", "FD_PSV", "Slope_Hill1", "FC", "cv_maximun_height", "plot_new", "PA", "Block")]

taxa$type <- "Taxonomic"
phylo$type <- "Phylogenetic"
funct$type <- "Functional" 

colnames(taxa)[2] <- "PSV"
colnames(phylo)[2] <- "PSV"
colnames(funct)[2] <- "PSV"

data <- rbind(taxa, phylo, funct)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

data_melt <- melt(data, 
                  id.vars = c("DOY", "PSV", "type", "Block", "plot_new", "PA"),
                  measure.vars = c("Slope_Hill1", "FC", "cv_maximun_height"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("cv_maximun_height", "FC", "Slope_Hill1"),
                          labels = c("Height heterogeneity", "Fractional cover", "Structural complexity"))

data_melt <- data_melt[!is.na(PSV),]

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
               aes(x = PSV, 
                   y = value,
                   color = DOY,
                   fill = DOY,
                   gruop = as.factor(DOY))) +
  geom_point(alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5) +
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "left", # "left" "right"
               label.y = "top", # "top" "bottom"
               size = text_size) +
  doy_color + doy_fill + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 3) +
  xlab("Taxonomic variability      Phylogenetic variability       Functional variability") +
  ylab(bquote(italic(d)[italic(D)]~~~~italic(FC)~~~italic(HH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ type, scales = "free")

# ------------------------------------------------------------------------------
#Export figure
jpeg(paste0(root_path, "/Figures/Figure_6_d.jpeg"), 
    width = 210, 
    height = 180, 
    units = "mm", 
    res = 600,
    bg = "transparent")

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

data_melt$DOY <- as.character(data_melt$DOY)

# Taxonomic diversity-----------------------------------------------------------
#HHcv
HH <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Taxonomic" & LiDAR == "Height heterogeneity", ])
qqnorm(resid(HH))
qqline(resid(HH))
report(HH)
Anova(HH, test.statistic="F")
tab_model(HH, p.val = "kr", show.df = TRUE)

#FC
FC <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
             data = data_melt[type == "Taxonomic" & LiDAR == "Fractional cover", ])
qqnorm(resid(FC))
qqline(resid(FC))
report(FC)
Anova(FC, test.statistic="F")
tab_model(FC, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Taxonomic" & LiDAR == "Structural complexity", ])
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)

# Phylogenetic diversity--------------------------------------------------------
#HHcv
ch <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Phylogenetic" & LiDAR == "Height heterogeneity", ])
qqnorm(resid(ch))
qqline(resid(ch))
report(ch)
Anova(ch, test.statistic="F")
tab_model(ch, p.val = "kr", show.df = TRUE)

#FC
FC <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
             data = data_melt[type == "Phylogenetic" & LiDAR == "Fractional cover", ])
qqnorm(resid(FC))
qqline(resid(FC))
report(FC)
Anova(FC, test.statistic="F")
tab_model(FC, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Phylogenetic" & LiDAR == "Structural complexity", ])
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)


# Functional diversity--------------------------------------------------------
#HHcv
HH <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Functional" & LiDAR == "Height heterogeneity", ])
qqnorm(resid(HH))
qqline(resid(HH))
report(HH)
Anova(HH, test.statistic="F")
tab_model(HH, p.val = "kr", show.df = TRUE)

#FC
FC <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
             data = data_melt[type == "Functional" & LiDAR == "Fractional cover", ])
qqnorm(resid(FC))
qqline(resid(FC))
report(FC)
Anova(FC, test.statistic="F")
tab_model(FC, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Functional" & LiDAR == "Structural complexity", ])
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)

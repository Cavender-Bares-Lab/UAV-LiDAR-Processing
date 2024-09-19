################################################################################
#' @title Effect of multiple dimensions of variability on LiDAR derived metrics
################################################################################

#' @description Figure 6 describing the relationships between multiple dimensions 
#'of variability and LiDAR derived metrics
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(rcartocolor)
library(ggplot2)
library(ggpmisc)
library(scales)
library(ggpubr)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))

#' -----------------------------------------------------------------------------
#' Reshape frame

taxa <- frame[, c("DOY", "TD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA", "Block")]
phylo <- frame[, c("DOY", "PD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA", "Block")]
funct <- frame[, c("DOY", "FD_PSV", "Slope_Hill1", "Pgap", "cv_maximun_height", "plot_new", "PA", "Block")]

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
     measure.vars = c("Slope_Hill1", "Pgap", "cv_maximun_height"),
     variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("cv_maximun_height", "Pgap", "Slope_Hill1"),
                          labels = c("Height heterogeneity", "Gap probability", "Structural complexity"))

data_melt <- data_melt[!is.na(PSV),]

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
               aes(x = PSV, 
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
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 3) +
  xlab("Taxonomic variability      Phylogenetic variability       Functional variability") +
  ylab(bquote(italic(d)[italic(D)]~~~~italic(P)[gap]~~~italic(CH)[CV])) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(LiDAR ~ type, scales = "free")

# ------------------------------------------------------------------------------
#Export figure
jpeg(paste0(root_path, "/Figure_6b.jpeg"), width = 210, height = 180, units = "mm", res = 600)

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
#CHcv
ch <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Taxonomic" & LiDAR == "Height heterogeneity", ])
qqnorm(resid(ch))
qqline(resid(ch))
report(ch)
Anova(ch, test.statistic="F")
tab_model(ch, p.val = "kr", show.df = TRUE)

#Pgap
Pgap <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
             data = data_melt[type == "Taxonomic" & LiDAR == "Gap probability", ])
qqnorm(resid(Pgap))
qqline(resid(Pgap))
report(Pgap)
Anova(Pgap, test.statistic="F")
tab_model(Pgap, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Taxonomic" & LiDAR == "Structural complexity", ])
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)

# Phylogenetic diversity--------------------------------------------------------
#CHcv
ch <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Phylogenetic" & LiDAR == "Height heterogeneity", ])
qqnorm(resid(ch))
qqline(resid(ch))
report(ch)
Anova(ch, test.statistic="F")
tab_model(ch, p.val = "kr", show.df = TRUE)

#Pgap
Pgap <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
             data = data_melt[type == "Phylogenetic" & LiDAR == "Gap probability", ])
qqnorm(resid(Pgap))
qqline(resid(Pgap))
report(Pgap)
Anova(Pgap, test.statistic="F")
tab_model(Pgap, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Phylogenetic" & LiDAR == "Structural complexity", ])
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)


# Functional diversity--------------------------------------------------------
#CHcv
ch <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Functional" & LiDAR == "Height heterogeneity", ])
qqnorm(resid(ch))
qqline(resid(ch))
report(ch)
Anova(ch, test.statistic="F")
tab_model(ch, p.val = "kr", show.df = TRUE)

#Pgap
Pgap <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
             data = data_melt[type == "Functional" & LiDAR == "Gap probability", ])
qqnorm(resid(Pgap))
qqline(resid(Pgap))
report(Pgap)
Anova(Pgap, test.statistic="F")
tab_model(Pgap, p.val = "kr", show.df = TRUE)

#dD
dD <- lmer(value ~ PSV*DOY + (1 | plot_new:Block),
           data = data_melt[type == "Functional" & LiDAR == "Structural complexity", ])
qqnorm(resid(dD))
qqline(resid(dD))
report(dD)
Anova(dD, test.statistic="F")
tab_model(dD, p.val = "kr", show.df = TRUE)

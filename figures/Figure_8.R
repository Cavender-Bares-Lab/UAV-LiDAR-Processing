################################################################################
#' @title Effect of seasonal structural stability on overyielding 
################################################################################

#' @description Figure 8 to test the effect of LiDAR seasonal stability 
#' on overyielding
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(viridis)
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

data <- frame[, c("plot_new", "SR_real", "PA", "Block", "DOY", "NE", "CE", "SE",
                  "Slope_Hill1", "FC", "cv_maximun_height")]

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_hh = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_fc = mean(FC)/sd(FC)), 
                   by = c("plot_new", "Block", "SR_real", "NE", "CE", "SE", "PA")]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "Block", "SR_real", "NE", "CE", "SE", "PA"),
                  measure.vars = c("SS_slope", "SS_hh", "SS_fc"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("SS_hh", "SS_fc", "SS_slope"),
                          labels = c("Height heterogeneity", "Fractional cover", "Structural complexity"))

data_melt <- data_melt[SR_real > 1,]

data_melt <- melt(data_melt, 
                  id.vars = c("plot_new", "Block", "SR_real", "PA", "LiDAR", "value"),
                  measure.vars = c("NE", "SE", "CE"),
                  variable.name = "partition",
                  value.name = "effect")

data_melt$partition <- as.factor(data_melt$partition)
data_melt$partition <- factor(data_melt$partition,
                          levels = c("NE", "CE", "SE"),
                          labels = c("Net biodiversity effect", "Complementarity effect", "Selection effect"))


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

plot <- ggplot(data_melt[partition == "Net biodiversity effect"],
               aes(value,
                   effect,
                   fill = PA)) +
  geom_point(colour = "grey25", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 linetype = "dotted",
                 colour = line_col) +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size,
               colour = line_col) +
  colour_PA +
  #coord_cartesian(ylim = c(-1, 25)) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(n.breaks = 3, limits = c(0, 24.2), expand = c(0, 0)) +
  annotation_logticks(sides = "b",
                      colour = line_col) +
  xlab(bquote(italic(SS)[italic(HH)[CV]]~~~~italic(SS)[italic(FC)]~~~italic(SS)[italic(d)[italic(D)]])) +
  ylab(bquote(Net~biodiversity~effect~(m^3~y^-1~ha^-1))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ LiDAR, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figures/Figure_8_b.jpeg"), 
    width = 180, 
    height = 90, 
    units = "mm", 
    res = 600,
    bg = "transparent")

plot

dev.off()

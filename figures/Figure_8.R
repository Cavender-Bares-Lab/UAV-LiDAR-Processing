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
library(RColorBrewer)
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

data <- frame[, c("plot_new", "SR_real", "PA", "Block", "DOY", "NE", "CE", "SE",
                  "Slope_Hill1", "Pgap", "cv_maximun_height")]

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_ch = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_pgap = mean(Pgap)/sd(Pgap)), 
                   by = c("plot_new", "Block", "SR_real", "NE", "CE", "SE", "PA")]

data_melt <- melt(ss_metrics, 
                  id.vars = c("plot_new", "Block", "SR_real", "NE", "CE", "SE", "PA"),
                  measure.vars = c("SS_slope", "SS_ch", "SS_pgap"),
                  variable.name = "LiDAR")

data_melt$LiDAR <- as.factor(data_melt$LiDAR)
data_melt$LiDAR <- factor(data_melt$LiDAR,
                          levels = c("SS_ch", "SS_pgap", "SS_slope"),
                          labels = c("Height heterogeneity", "Gap probability", "Structural complexity"))

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

plot <- ggplot(data_melt[partition == "Net biodiversity effect"],
               aes(value,
                   effect,
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
               label.x = "right",
               label.y = "top",
               size = text_size) +
  colour_PA +
  #coord_cartesian(ylim = c(-0.001, 0.16)) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans(), n.breaks = 4) +
  #scale_y_continuous(n.breaks = 4) +
  annotation_logticks(sides = "bl") +
  xlab(bquote(italic(SS)[italic(CH)[CV]]~~~~italic(SS)[italic(P)[gap]]~~~italic(SS)[italic(d)[italic(D)]])) +
  ylab(bquote(Net~biodiversity~effect~(m^3~y^-1))) +
  #ylab(bquote(NBE~(m^3~y^-1)~~~SE~(m^3~y^-1)~~~CE~(m^3~y^-1))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ LiDAR, scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figure_8a.jpeg"), width = 180, height = 90, units = "mm", res = 600)

plot

dev.off()

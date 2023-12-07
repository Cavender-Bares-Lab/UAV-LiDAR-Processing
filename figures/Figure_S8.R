################################################################################
#' @title Effect of the proportion of angiospemrs on overyilding 
################################################################################

#' @description Figure S8 to test the effect of the proportion of angiosperms on
#' on overyilding
#' 
#' @return A jpeg file

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
frame <- frame[date == "2022-04-10",]
frame <- frame[SR_real != 1,]

#' -----------------------------------------------------------------------------
#' Data reshaping

data <- frame[, c("plot_new", "SR_real", "PA", "Block", "DOY", "NE", "CE", "SE")]

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

colour_PA <- scale_fill_viridis("Species richness",
                                option = "F",
                                direction = 1,
                                limits = c(2, 12),
                                breaks = c(2, 7, 12))

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plot

plot <- ggplot(data,
               aes(PA,
                   NE,
                   fill = SR_real)) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21, size = 2) +
  stat_poly_line(method = "lm",
                 #se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 #linetype = "dotted",
                 colour = "black") +
  stat_poly_eq(use_label(c("R2", "F", "P")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = text_size) +
  colour_PA +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 4) +
  xlab("Proportion of angiosperms") +
  ylab(bquote(Net~biodiversity~effect~(m^3~y^-1))) +
  #ylab(bquote(NBE~(m^3~y^-1)~~~SE~(m^3~y^-1)~~~CE~(m^3~y^-1))) +
  theme_bw(base_size = tamano) +
  th + gui

#Export figure
jpeg(paste0(root_path, "/Figure_S8.jpeg"), width = 110, height = 100, units = "mm", res = 600)

plot

dev.off()

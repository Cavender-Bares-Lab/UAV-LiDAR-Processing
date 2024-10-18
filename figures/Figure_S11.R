################################################################################
#' @title Effect of the proportion of angiosperms on overyielding
################################################################################

#' @description Figure S11 to test the effect of the proportion of angiosperms on
#' on overyielding
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

root_path <- "/media/antonio/work/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean (2024-10-17).csv"))
frame <- frame[date == "2022-04-10",]
frame <- frame[SR_real != 1,]

#' -----------------------------------------------------------------------------
#' Data reshaping

data <- frame[, c("plot_new", "SR_real", "PA", "Block", "DOY", "NE", "CE", "SE")]

data_melt <- melt(data, 
                  id.vars = c("plot_new", "SR_real", "PA", "Block", "DOY"),
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

colour_PA <- scale_fill_viridis("Species richness",
                                option = "F",
                                direction = 1,
                                limits = c(2, 12),
                                breaks = c(2, 7, 12))

alpha_point <- 1.0

# ------------------------------------------------------------------------------
# Plot

plot <- ggplot(data_melt,
               aes(PA,
                   effect,
                   fill = SR_real)) +
  geom_point(colour = "grey25", alpha = alpha_point, shape = 21, size = 2) +
  stat_poly_line(method = "lm",
                 #se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 #linetype = "dotted",
                 colour = "black") +
  stat_poly_eq(use_label(c("eq", "R2")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = text_size) +
  colour_PA +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 3) +
  #scale_y_continuous(trans = log10_trans(), n.breaks = 4) +
  #annotation_logticks(sides = "l") +
  xlab("Proportion of angiosperms") +
  ylab(bquote(SE~(m^3~y^-1~ha^-1)~~~~~~CE~(m^3~y^-1~ha^-1)~~~~~~NBE~(m^3~y^-1~ha^-1))) +
  #ylab(bquote(NBE~(m^3~y^-1)~~~SE~(m^3~y^-1)~~~CE~(m^3~y^-1))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(partition ~ ., scales = "free")

#Export figure
jpeg(paste0(root_path, "/Figures/Figure_S11b.jpeg"), 
     width = 90, 
     height = 180, 
     units = "mm", 
     res = 600)

plot

dev.off()


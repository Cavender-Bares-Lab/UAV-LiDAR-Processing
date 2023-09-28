################################################################################
#' @title Effect of tree community composition
################################################################################

#' @description Relationships between diversity and FSC
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
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean_reshaped.csv"))
frame$type <- as.factor(frame$type)
frame$type <- factor(frame$type, levels = c("Taxonomic", "Phylogenetic", "Functional"))

# Plot details
tamano <- 12
tamano2 <- 10
text_size <- 2.8
th <- theme(plot.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"),
            plot.margin = margin(4, 4, 0, 0, "pt"),
            legend.position= c("top"), 
            legend.direction = "horizontal", 
            legend.background = element_rect(fill = "transparent"), 
            legend.box.background = element_blank())
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Diversity
ggplot(frame, aes((MPD + 1), 
                  Slope_N,
                  color = DOY,
                  fill = DOY,
                  gruop = as.factor(DOY))) +
  geom_point(shape = 21, 
             colour = "grey", 
             alpha = 0.2) +
  stat_ma_line(method = "SMA",
               se = FALSE,
               size = 0.5) +
  stat_ma_eq(use_label(c("eq", "R2")),
             method = "SMA",
             label.x = "right",
             label.y = "bottom",
             size = text_size) +
  scale_color_carto_c("Day of the Year", 
                      type = "diverging", 
                      palette = "Fall",
                      guide = "none") +
  scale_fill_carto_c("Day of the Year", 
                     type = "diverging", 
                     palette = "Fall",
                     limits = c(95, 305),
                     breaks = c(100, 200, 300)) +
  scale_x_continuous(trans = log10_trans()) +
  scale_y_continuous(trans = log10_trans()) +
  #coord_cartesian(expand = FALSE) +
  annotation_logticks(sides = "bl") +
  #xlab(" ") +
  #ylab(expression(SEI[vertical]))  +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(. ~ type, scales = "free_x")



################################################################################
#' @title Effect of diversity on overyilding 
################################################################################

#' @description Figure S7 to test the effect of diversity on overyilding
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
library(ggpubr)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame <- frame[date == "2022-04-10",]
frame <- frame[SR_real != 1, ]

#' -----------------------------------------------------------------------------
#' Data reshaping

data <- frame[, c("plot_new", "PA", "Block", "DOY", "NE",
                  "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                  "TD_PSV", "FD_PSV", "PD_PSV")]

# Melt by diversity
data_diversity <- melt(data, 
             id.vars = c("plot_new", "PA", "Block", "NE"),
             measure.vars = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
             variable.name = "diversity_metric",
             value.name = "diversity")

data_diversity$diversity_metric <- as.factor(data_diversity$diversity_metric)
data_diversity$diversity_metric <- factor(data_diversity$diversity_metric, 
                                      levels = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
                                      labels = c("Taxonomic", "Phylogenetic", "Functional"))
data_diversity$metric <- "Diversity"


# Melt by species variability
data_SV <- melt(data, 
             id.vars = c("plot_new", "PA", "Block", "NE"),
             measure.vars = c("TD_PSV", "FD_PSV", "PD_PSV"),
             variable.name = "SV_metric",
             value.name = "SV")

data_SV$SV_metric <- as.factor(data_SV$SV_metric)
data_SV$SV_metric <- factor(data_SV$SV_metric, 
                         levels = c("TD_PSV", "FD_PSV", "PD_PSV"),
                         labels = c("Taxonomic", "Phylogenetic", "Functional"))
data_SV$metric <- "Species variability"

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

plot_diversity <- ggplot(data_diversity,
               aes(diversity,
                   NE,
                   fill = PA)) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 linetype = "dotted",
                 colour = "black") +
  stat_poly_eq(use_label(c("R2", "F", "P")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = text_size) +
  colour_PA +
  #coord_cartesian(ylim = c(-0.001, 0.16)) +
  scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(n.breaks = 4) +
  annotation_logticks(sides = "b") +
  xlab(bquote(Species~richness~~~~~~italic(PD)~~~~~~italic(FD)))  +
  ylab(bquote(Net~biodiversity~effect~(m^3~y^-1))) +
  #ylab(bquote(NBE~(m^3~y^-1)~~~SE~(m^3~y^-1)~~~CE~(m^3~y^-1))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(metric ~ diversity_metric, scales = "free")


plot_sv <- ggplot(data_SV,
               aes(SV,
                   NE,
                   fill = PA)) +
  geom_point(colour = "grey", alpha = alpha_point, shape = 21) +
  stat_poly_line(method = "lm",
                 #se = FALSE,
                 formula = y ~ x,
                 linewidth = 0.5,
                 #linetype = "dotted",
                 colour = "black") +
  stat_poly_eq(use_label(c("R2", "F", "P")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = text_size) +
  colour_PA +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(n.breaks = 3) +
  #scale_x_continuous(trans = log10_trans()) +
  #scale_y_continuous(n.breaks = 4) +
  #annotation_logticks(sides = "b") +
  xlab("Species variability")  +
  ylab(bquote(Net~biodiversity~effect~(m^3~y^-1))) +
  theme_bw(base_size = tamano) +
  th + gui +
  facet_grid(metric ~ SV_metric, scales = "free")

plot <- ggarrange(plot_diversity, plot_sv,
                  ncol = 1, nrow = 2,
                  common.legend = TRUE)

#Export figure
jpeg(paste0(root_path, "/Figure_S7a.jpeg"), width = 210, height = 150, units = "mm", res = 600)

plot

dev.off()

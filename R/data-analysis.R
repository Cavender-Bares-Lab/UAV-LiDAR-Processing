################################################################################
#' @title Data analysis
################################################################################

#' @description Steps applied to the matrices of FSC and above-ground biomass.

#' -----------------------------------------------------------------------------
#' Libraries

library(data.table)
library(ggplot2)
library(ggpubr)
library(viridis)

#' -----------------------------------------------------------------------------
#' Set working directory 
path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' LiDAR data cleaning

# Read structural complexity
frame <- fread(paste0(path, "/fsc_results.csv"))
diversity <- fread(paste0(path, "/diversity_complete.csv"))
frame <- merge(diversity, frame, by = "plot", all.x = TRUE, all.y = TRUE)
#fwrite(frame, paste0(path, "/master_frame.csv"))
frame <- fread(paste0(path, "/master_frame.csv"))

# Remove plots
# By heights
frame <- frame[area <= 150,]

# By nprofiles
frame <- frame[n_profiles > 1]

# Get plots with complete information over time
observations <- frame[, .N, by = "plot"]
observations <- observations[N == 7,]
observations <- observations[, 1]

frame <- merge(frame, observations, by = "plot", all.x = FALSE, all.y = TRUE)

#' -----------------------------------------------------------------------------
#' Make figures

th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

colors <- c("#3f007d", "#54278f", "#807dba", "#9e9ac8", "#bcbddc")
divergin <- c("#01665e", "#80cdc1", "#c7eae5", "#dfc27d", "#8c510a")

# Field inventory

AGB <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = total_biomass, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PA), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0, 3.5), expand = FALSE) +
  xlab(" ") + 
  ylab("AGB (kg)") +
  #ylab(expression(paste("LAI (m"^2, " m"^-2, ")", sep = ""))) +
  scale_y_continuous(trans= "log10") +
  annotation_logticks(sides = "l") +
  th

ntrees <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = ntrees, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PA), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(85, 105), expand = FALSE) +
  xlab(" ") + 
  ylab("Number of trees") +
  #ylab(expression(paste("LAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

gini <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = tree_gini, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PA), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Tree size inequality") +
  #ylab(expression(paste("LAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

inventory <- ggarrange(AGB, ntrees, gini, ncol = 1, common.legend = TRUE)

jpeg("inventory.jpeg", width = 140, height = 170, units = "mm", res = 600)

inventory

dev.off()


# Remotely sense forest

cx <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = Cx, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PA), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 2.1), expand = FALSE) +
  xlab(" ") + 
  ylab(expression(paste("Cx (m"^2, " m"^-3, ")", sep = ""))) +
  #ylab(expression(paste("LAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

cy <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = Cy, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PA), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(0.5, 4), expand = FALSE) +
  xlab(" ") + 
  #ylab("Cy (m)") +
  ylab("Cy (m)") +
  th

RG <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = RG, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(0.5, 2.8), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("RG") +
  #ylab(expression(paste("LAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

profiles <- ggarrange(cx, cy, RG, ncol = 1, common.legend = TRUE)

jpeg("profiles.jpeg", width = 140, height = 150, units = "mm", res = 600)

profiles

dev.off()

cx_temp <- ggplot(frame, aes(x = date, y = Cx)) +
  #geom_violin(trim = FALSE) +
  geom_point(aes(x = date, y = Cx), colour = "white", fill = "grey", size = 0.8, shape = 21) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = TRUE, alpha = 0.1) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = FALSE) +
  scale_fill_manual("Species Richness", values = colors) +
  scale_colour_manual("Species Richness", values = colors) +
  xlab(" ") + 
  ylab(expression(paste("Cx (m"^2, " m"^-3, ")", sep = ""))) +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 1.8), expand = FALSE) +
  th

cy_temp <- ggplot(frame, aes(x = date, y = Cy)) +
  #geom_violin(trim = FALSE) +
  geom_point(aes(x = date, y = Cy), colour = "white", fill = "grey", size = 0.8, shape = 21) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = TRUE, alpha = 0.1) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = FALSE) +
  scale_fill_manual("Species Richness", values = colors) +
  scale_colour_manual("Species Richness", values = colors) +
  xlab(" ") + 
  ylab("Cy (m)") +
  coord_cartesian(ylim = c(0.5, 4.2), expand = FALSE) +
  theme_bw() + 
  th

rg_temp <- ggplot(frame, aes(x = date, y = RG)) +
  #geom_violin(trim = FALSE) +
  geom_point(aes(x = date, y = RG), colour = "white", fill = "grey", size = 0.8, shape = 21) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = TRUE, alpha = 0.1) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = FALSE) +
  scale_fill_manual("Species Richness", values = colors) +
  scale_colour_manual("Species Richness", values = colors) +
  xlab("Month") + 
  ylab("RG") +
  theme_bw() + 
  coord_cartesian(ylim = c(0.5, 2.8), expand = FALSE) +
  th

temp_profiles <- ggarrange(cx_temp, cy_temp, rg_temp, ncol = 1, common.legend = TRUE)

jpeg("temp_profiles.jpeg", width = 140, height = 150, units = "mm", res = 600)

temp_profiles

dev.off()



# Remotely sense forest complexity

VCI <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = vci, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PA), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab(" ") + 
  ylab("Vertical Complexity Index") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

rumple <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = rumple, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(1, 4.3), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Rumple Index of Roughness") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

complexity <- ggarrange(VCI, rumple, ncol = 1, common.legend = TRUE)

jpeg("complexity.jpeg", width = 140, height = 150, units = "mm", res = 600)

complexity

dev.off()

vci_temp <- ggplot(frame, aes(x = date, y = vci)) +
  #geom_violin(trim = FALSE) +
  geom_point(aes(x = date, y = vci), colour = "white", fill = "grey", size = 1.5, shape = 21) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = TRUE, alpha = 0.1) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = FALSE) +
  scale_fill_manual("Species Richness", values = colors) +
  scale_colour_manual("Species Richness", values = colors) +
  xlab("Month") + ylab("Vertical Complexity Index") +
  theme_bw() + 
  coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  th

rumple_temp <- ggplot(frame, aes(x = date, y = rumple)) +
  #geom_violin(trim = FALSE) +
  geom_point(aes(x = date, y = rumple), colour = "white", fill = "grey", size = 1.5, shape = 21) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = TRUE, alpha = 0.1) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = FALSE) +
  scale_fill_manual("Species Richness", values = colors) +
  scale_colour_manual("Species Richness", values = colors) +
  xlab("Month") + ylab("Rumple Index of Roughness") +
  theme_bw() + 
  coord_cartesian(ylim = c(1, 4.3), expand = FALSE) +
  th

temp_complexity <- ggarrange(vci_temp, rumple_temp, ncol = 1, common.legend = TRUE)

jpeg("temp_complexity.jpeg", width = 140, height = 150, units = "mm", res = 600)

temp_complexity

dev.off()

# LAI

PAI <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = LAIe, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 15), expand = FALSE) +
  xlab("Species Richness") + 
  #ylab("Vertical Complexity Index") +
  ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  theme(legend.position="top") +
  th

PAI_temp <- ggplot(frame, aes(x = date, y = LAIe)) +
  #geom_violin(trim = FALSE) +
  geom_point(aes(x = date, y = LAIe), colour = "white", fill = "grey", size = 1.5, shape = 21) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(SR), fill = as.factor(SR)), formula = y ~ x, se = FALSE) +
  scale_fill_manual("Species Richness", values = colors) +
  scale_colour_manual("Species Richness", values = colors) +
  xlab("Month") + ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  theme_bw() + 
  coord_cartesian(ylim = c(0, 15), expand = FALSE) +
  theme(legend.position="top") +
  th

PAI_export <- ggarrange(PAI, PAI_temp, ncol = 1, common.legend = FALSE)

jpeg("PAI.jpeg", width = 140, height = 150, units = "mm", res = 600)

PAI_export

dev.off()


AGB_PAI <- ggplot(frame, aes(x = total_biomass, y = LAIe, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  xlab("AGB (kg)") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") + 
  scale_y_continuous(trans= "log10") +
  annotation_logticks()

gini_PAI <- ggplot(frame, aes(x = tree_gini, y = LAIe, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  xlab("Tree size inequality") +
  theme_bw() + th 
  #scale_x_continuous(trans= "log10") + 
  #scale_y_continuous(trans= "log10") +
  #annotation_logticks()
  
PAI_relationships <- ggarrange(AGB_PAI, gini_PAI, ncol = 2, common.legend = TRUE)

jpeg("AGB_PAI.jpeg", width = 200, height = 90, units = "mm", res = 600)

PAI_relationships

dev.off()
  

gini_vci <- ggplot(frame, aes(x = tree_gini, y = vci, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab(" ") +
  theme_bw() + th 
  #scale_x_continuous(trans= "log10") + 
  #scale_y_continuous(trans= "log10") +
  #annotation_logticks()

gini_rumple <- ggplot(frame, aes(x = tree_gini, y = rumple, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("Tree size inequality") +
  ylab(" ") +
  theme_bw() + th 


AGB_vci <- ggplot(frame, aes(x = total_biomass, y = vci, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Vertical Complexity Index") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") + 
  #scale_y_continuous(trans= "log10") +
  annotation_logticks(side = "b")

AGB_rumple <- ggplot(frame, aes(x = total_biomass, y = rumple, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("AGB (kg)") +
  ylab("Rumple Index of Roughness") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") + 
  #scale_y_continuous(trans= "log10") +
  annotation_logticks(side = "b")

complexity_relationships <- ggarrange(AGB_vci, gini_vci,
                                      AGB_rumple, gini_rumple, ncol = 2, nrow = 2, common.legend = TRUE)

jpeg("complexity_relationships.jpeg", width = 210, height = 150, units = "mm", res = 600)

complexity_relationships

dev.off()

# Fractal geometry

# Remotely sense forest complexity

fractal <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = Slope_N, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab(" ") + 
  ylab("Vertical Complexity Index") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

intercept <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = Intercept_N, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab(" ") + 
  ylab("Vertical Complexity Index") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

agb_fractal <- ggplot(frame, aes(x = total_biomass, y = Slope_N, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Factal geometry") +
  theme_bw() + th +
  coord_cartesian(ylim = c(0.37, 0.9)) +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")

agb_intercept <- ggplot(frame, aes(x = total_biomass, y = Intercept_N, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("AGB (kg)") +
  ylab("Intercept") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")


abg_geometry <- ggarrange(agb_fractal, agb_intercept, ncol = 1, common.legend = TRUE)

jpeg("abg_geometry.jpeg", width = 140, height = 150, units = "mm", res = 600)

abg_geometry

dev.off()
  
# GINI factral

gini_intercept <- ggplot(frame, aes(x = tree_gini, y = Intercept_N, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("Tree size inequality") +
  ylab(" ") +
  theme_bw() + th 

gini_fractal <- ggplot(frame, aes(x = tree_gini, y = Slope_N, gruop = as.factor(date))) +
  geom_point(aes(fill = as.factor(date)), colour = "white", size = 2, shape = 21, alpha = 0.5) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10) +
  geom_smooth(aes(colour = as.factor(date), fill = as.factor(date)), 
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  ylab(" ") +
  xlab(" ") +
  coord_cartesian(ylim = c(0.37, 0.9)) +
  theme_bw() + th

gini_geometry <- ggarrange(agb_fractal, 
                           gini_fractal, 
                           agb_intercept,
                           gini_intercept, ncol = 2, nrow = 2,
                           common.legend = TRUE)

jpeg("gini_geometry.jpeg", width = 210, height = 150, units = "mm", res = 600)

gini_geometry

dev.off()



fractal <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = Slope_N, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab(" ") + 
  ylab("Fractal geometry") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th


intercept <- ggplot(frame[date == "2022-07-06"], aes(x = as.factor(SR), y = Intercept_N, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Intercept") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

geometry <- ggarrange(fractal, 
                      intercept,
                      ncol = 1, common.legend = TRUE)

jpeg("geometry.jpeg", width = 140, height = 150, units = "mm", res = 600)

geometry

dev.off()

spectra <- fread("/home/antonio/Documents/spectra-diversity.csv")
spectra <- spectra[year_mean < 2017, ]
spectra <- spectra[ntrees > 90, ]
colnames(spectra)[6] <- "SR"

mean_NDVI <- ggplot(spectra, aes(x = as.factor(SR), y = mean_NDVI, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab(" ") + 
  ylab("Average (NDVI)") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

sd_NDVI <- ggplot(spectra, aes(x = as.factor(SR), y = sd_NDVI/mean_NDVI, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Coefficient of Variation (NDVI)") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

mean_NDRI <- ggplot(spectra, aes(x = as.factor(SR), y = mean_NDRI, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab(" ") + 
  ylab("Average (NDRI)") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th

sd_NDRI <- ggplot(spectra, aes(x = as.factor(SR), y = sd_NDRI, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Coefficient of Variation (NDRI)") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th
  


spectral_indices <- ggarrange(mean_NDVI, mean_NDRI,
                              sd_NDVI, sd_NDRI,
                              ncol = 2, nrow = 2, common.legend = TRUE)

jpeg("spectral_indices.jpeg", width = 210, height = 150, units = "mm", res = 600)

spectral_indices

dev.off()



agb_NDVI <- ggplot(spectra, aes(x = total_biomass, y = mean_NDVI)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Average (NDVI)") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")

agb_sd <- ggplot(spectra, aes(x = total_biomass, y = sd_NDVI/mean_NDVI)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("AGB (kg)") +
  ylab("Coeficient of Variation (NDVI)") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")


gini_NDVI <- ggplot(spectra, aes(x = tree_gini, y = mean_NDVI)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab(" ") +
  theme_bw() + th 
  #scale_x_continuous(trans= "log10") +
  #annotation_logticks(sides = "b")

gini_sd <- ggplot(spectra, aes(x = tree_gini, y = sd_NDVI/mean_NDVI)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("Tree size inequality") +
  ylab(" ") +
  theme_bw() + th 


spectral_relations <- ggarrange(agb_NDVI, gini_NDVI,
                                agb_sd, gini_sd,
                              ncol = 2, nrow = 2, common.legend = TRUE)

jpeg("spectral_relations.jpeg", width = 210, height = 150, units = "mm", res = 600)

spectral_relations

dev.off()


spectral_diversity <- ggplot(spectra, aes(x = as.factor(SR), y = cv, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Spectral Diversity") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th


jpeg("spectral_diversity.jpeg", width = 140, height = 90, units = "mm", res = 600)

spectral_diversity

dev.off()

spectral_diverisity_gini <- ggplot(spectra, aes(x = tree_gini, y = cv)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("Tree size inequality") +
  ylab("Spectral diversity") +
  theme_bw() + th 


spectral_diverisity_agb <- ggplot(spectra, aes(x = total_biomass, y = cv)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab("AGB (kg)") +
  ylab("Spectral diversity") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")


diverisity_relations <- ggarrange(spectral_diverisity_agb, spectral_diverisity_gini,
                                ncol = 2, nrow = 1, common.legend = TRUE)

jpeg("diverisity_relations.jpeg", width = 210, height = 90, units = "mm", res = 600)

diverisity_relations

dev.off()





diversity_VN <- ggplot(spectra, aes(x = as.factor(SR), y = ss_VN, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Spectral alpha diversity") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th + labs(subtitle = "Vector Normalized")

diversity_SW <- ggplot(spectra, aes(x = as.factor(SR), y = ss_SW, fill = as.factor(SR), gruop = as.factor(SR))) +
  geom_violin(trim = FALSE) +
  geom_point(aes(colour = PG), position = position_jitterdodge(), size = 0.8, shape = 19) +
  stat_summary(fun.data = mean_sdl, geom="pointrange", color="white", size = 0.5) +
  scale_fill_manual(values = colors, guide="none") + 
  scale_color_gradient2("Proportion of gymnosperms", 
                        midpoint= 0.5, low="#8c510a", mid="#f5f5f5", high= "#01665e", space ="Lab" ) +
  theme_bw() + 
  #coord_cartesian(ylim = c(0.5, 1), expand = FALSE) +
  xlab("Species Richness") + 
  ylab("Spectral alpha diversity") +
  #ylab(expression(paste("PAI (m"^2, " m"^-2, ")", sep = ""))) +
  th + labs(subtitle = "Continuous Wavelet Transformation")

alpha_diverisity <- ggarrange(diversity_VN, diversity_SW,
                                  ncol = 2, common.legend = TRUE)

jpeg("alpha_diverisity.jpeg", width = 210, height = 90, units = "mm", res = 600)

alpha_diverisity

dev.off()


ss_biomass <- ggplot(spectra, aes(x = SR, y = cv_diversity_VN)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Average (NDVI)") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")

ss_biomass <- ggplot(spectra, aes(x = sp_H, y = ss_VN)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Average (NDVI)") +
  theme_bw() + th 
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")

ss_biomass <- ggplot(spectra, aes(x = sp_H, y = ss_VN)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Average (NDVI)") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")

ss_biomass <- ggplot(spectra, aes(x = tree_gini, y = ss_VN)) +
  geom_point(colour = "darkgreen", size = 2, shape = 19, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.10, colour = "black") +
  #scale_fill_viridis("Date", option = "G", discrete = TRUE) +
  #scale_colour_viridis("Date", option = "G", discrete = TRUE) +
  xlab(" ") +
  ylab("Average (NDVI)") +
  theme_bw() + th +
  scale_x_continuous(trans= "log10") +
  annotation_logticks(sides = "b")

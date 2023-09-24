
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position= 'top')

point_size = 1

q0_VCI <- ggplot(frame, aes(x = q0, y = vci, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Vertical Complexity Index") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q1_VCI <- ggplot(frame, aes(x = q1, y = vci, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q2_VCI <- ggplot(frame, aes(x = q2, y = vci, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))



q0_rumple <- ggplot(frame, aes(x = q0, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Roughness Index") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(1, 4.1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q1_rumple <- ggplot(frame, aes(x = q1, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(1, 4.1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q2_rumple <- ggplot(frame, aes(x = q2, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(1, 4.1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))




q0_fractal <- ggplot(frame, aes(x = q0, y = Slope_N, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Fractal geometry") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 0.9), expand = FALSE) +
  xlab(expression({}^0*italic(D))) +
  th +
  scale_y_continuous(n.breaks = 3) +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q1_fractal <- ggplot(frame, aes(x = q1, y = Slope_N, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 0.9), expand = FALSE) +
  xlab(expression({}^1*italic(D))) +
  th +
  scale_y_continuous(n.breaks = 3) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q2_fractal <- ggplot(frame, aes(x = q2, y = Slope_N, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, position = position_jitterdodge()) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 0.9), expand = FALSE) +
  xlab(expression({}^2*italic(D))) +
  th +
  scale_y_continuous(n.breaks = 3) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

plot <- ggarrange(q0_VCI, q1_VCI, q2_VCI,
          q0_rumple, q1_rumple, q2_rumple,
          q0_fractal, q1_fractal, q2_fractal,
          ncol = 3, nrow = 3,
          common.legend = TRUE)

jpeg("complexity_all.jpeg", width = 210, height = 150, units = "mm", res = 600)

plot

dev.off()




AGB_VCI <- ggplot(frame, aes(x = total_biomass, y = vci, fill = DOY, colour = as.factor(DOY))) +
  scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Vertical Complexity Index") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  annotation_logticks(sides = "b") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ntrees_VCI <- ggplot(frame, aes(x = ntrees, y = vci, fill = DOY, colour = as.factor(DOY))) +
  scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  annotation_logticks(sides = "b") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ine_VCI <- ggplot(frame, aes(x = tree_gini, y = vci, fill = DOY, colour = as.factor(DOY))) +
  scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  annotation_logticks(sides = "b") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


AGB_rumple <- ggplot(frame, aes(x = total_biomass, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Roughness Index") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  annotation_logticks(sides = "b") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ntrees_rumple <- ggplot(frame, aes(x = ntrees, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ine_rumple <- ggplot(frame, aes(x = tree_gini, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  #scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(n.breaks = 4) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))




AGB_rumple <- ggplot(frame, aes(x = total_biomass, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Roughness Index") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +  
  annotation_logticks(sides = "b") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ntrees_rumple <- ggplot(frame, aes(x = ntrees, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +  
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ine_rumple <- ggplot(frame, aes(x = tree_gini, y = rumple, fill = DOY, colour = as.factor(DOY))) +
  #scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))




AGB_fractal <- ggplot(frame, aes(x = total_biomass, y = Slope_N, fill = DOY, colour = as.factor(DOY))) +
  scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Fractal geometry") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab("Above-ground biomass (kg)") +
  th +
  coord_cartesian(ylim = c(0.4, 0.9), expand = FALSE) +
  scale_y_continuous(n.breaks = 3) + 
  annotation_logticks(sides = "b") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ntrees_fractal <- ggplot(frame, aes(x = ntrees, y = Slope_N, fill = DOY, colour = as.factor(DOY))) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  coord_cartesian(ylim = c(0.4, 0.9), expand = FALSE) +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab("Number of trees") +
  th +
  scale_y_continuous(n.breaks = 3) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


ine_fractal <- ggplot(frame, aes(x = tree_gini, y = Slope_N, fill = DOY, colour = as.factor(DOY))) +
  #scale_x_continuous(trans= "log10") +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "lm", 
  #            formula = y ~ poly(x, 2, raw=TRUE), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab("Tree size inequality") +
  th +
  coord_cartesian(ylim = c(0.4, 0.9), expand = FALSE) +
  scale_y_continuous(n.breaks = 3) + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))



plot <- ggarrange(AGB_VCI, ntrees_VCI, ine_VCI,
                  AGB_rumple, ntrees_rumple, ine_rumple,
                  AGB_fractal, ntrees_fractal, ine_fractal,  
                  ncol = 3, nrow = 3,
                  common.legend = TRUE)

jpeg("stand-metrics_all.jpeg", width = 210, height = 150, units = "mm", res = 600)

plot

dev.off()















point_size <- 2

q0_agb <- ggplot(frame[DOY == 100,], aes(x = q0, y = total_biomass)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21, fill = "grey") +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1,
              colour = "black") +
  theme_bw() +
  ylab("Above-ground biomass (kg)") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  annotation_logticks(sides = "l") +
  scale_y_continuous(trans= "log10") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q1_agb <- ggplot(frame[DOY == 100,], aes(x = q1, y = total_biomass)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  annotation_logticks(sides = "l") +
  scale_y_continuous(trans= "log10") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))

q2_agb <- ggplot(frame[DOY == 100,], aes(x = q2, y = total_biomass)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th +
  annotation_logticks(sides = "l") +
  scale_y_continuous(trans= "log10") +
  guides(fill = guide_colourbar(barwidth = 30, 
                                barheight = 0.5,
                                title.position = "top",
                                title.hjust = 0.5))


q0_ntrees <- ggplot(frame[DOY == 100,], aes(x = q0, y = ntrees)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Number of trees") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th

q1_ntrees <- ggplot(frame[DOY == 100,], aes(x = q1, y = ntrees)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th

q2_ntrees <- ggplot(frame[DOY == 100,], aes(x = q2, y = ntrees)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th


q0_gini <- ggplot(frame[DOY == 100,], aes(x = q0, y = tree_gini)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab("Number of trees") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th

q1_gini <- ggplot(frame[DOY == 100,], aes(x = q1, y = tree_gini)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  #geom_smooth(method = "nls", 
  #            formula = y ~ a + b * log10(x + 0.1),
  #            method.args = list(start = list(a = 1, b = 10)), 
  #            se = F) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th

q2_gini <- ggplot(frame[DOY == 100,], aes(x = q2, y = tree_gini)) +
  geom_point(size = point_size, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(log10(x), 2, raw=TRUE), 
              se = T, alpha = 0.1) +
  theme_bw() +
  ylab(" ") +
  scale_fill_carto_c("Day of the Year", type = "diverging", palette = "Earth") + 
  scale_colour_carto_d("Day of the Year", type = "diverging", palette = "Earth", guide = NULL) +
  #coord_cartesian(ylim = c(0.4, 1), expand = FALSE) +
  xlab(" ") +
  th

plot <- ggarrange(q0_VCI, q1_VCI, q2_VCI,
                  q0_rumple, q1_rumple, q2_rumple,
                  q0_fractal, q1_fractal, q2_fractal,
                  ncol = 3, nrow = 3,
                  common.legend = TRUE)

jpeg("complexity_all.jpeg", width = 210, height = 150, units = "mm", res = 600)

plot

dev.off()

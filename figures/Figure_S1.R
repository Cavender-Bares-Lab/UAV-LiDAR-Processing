################################################################################
#' @title Taxonomic, Phylogenetic, and Functional trees
################################################################################

#' @description Figure S1 describing the clustering or phylogenetic trees. The 
#' script '02-plot_diversity.R' most be runned first to create this figure
#' 
#' @return A jpeg file with the figure

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

# ------------------------------------------------------------------------------
# Plot details



# Export figure
jpeg(paste0(root_path, "/Figures/Figure_S1.jpeg"), width = 210, height = 90, units = "mm", res = 600)

par(mfrow=c(1,3), oma = c(1, 1, 1, 1))
plot(taxonomic_matched$phy, cex = 1.15, cex.main = 1.5, main = "Taxonomic")
plot(phylogenetic_matched$phy, cex = 1.15, cex.main = 1.5, main = "Phylogenetic")
plot(functional_matched$phy, cex = 1.15, cex.main = 1.5, main = "Functional")

dev.off()

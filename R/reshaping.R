################################################################################
#' @title Data reshaping
################################################################################

#' @description Effect of biomass, tree size inequality, and PA on FSC
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Reshape diversity

# Load
frame <- fread(paste0(root_path, "/diversity.csv"))

# Variables to select
tax <- c("plot", "TD_faith", "TD_MPD", "TD_raoD", 
         "TD_PSV", "TD_PSR", "TD_PSE", "TD_PSC")

phy <- c("plot", "PD_faith", "PD_MPD", "PD_raoD", 
         "PD_PSV",  "PD_PSR", "PD_PSE", "PD_PSC")

func <- c("plot", "FD_faith", "FD_MPD", "FD_raoD", 
          "FD_PSV", "FD_PSR", "FD_PSE", "FD_PSC")

# Variables extraction
taxonomic <- frame[, ..tax]
phylogenetic <- frame[, ..phy]
functional <- frame[, ..func]

# Rename
colnames(taxonomic) <- c("plot", "faith", "MPD", "raoD", "PSV", "PSR", "PSE", "PSC")
colnames(phylogenetic) <- c("plot", "faith", "MPD", "raoD", "PSV", "PSR", "PSE", "PSC")
colnames(functional) <- c("plot", "faith", "MPD", "raoD", "PSV", "PSR", "PSE", "PSC")

# Add column description
taxonomic$type <- "Taxonomic"
phylogenetic$type <- "Phylogenetic"
functional$type <- "Functional"

#Merge and export
diversity_reshape <- rbind(taxonomic, phylogenetic, functional)
diversity_reshape$plot <- as.character(diversity_reshape$plot)
fwrite(diversity_reshape, paste0(root_path, "/diversity_reshaped.csv"))

#' -----------------------------------------------------------------------------
#' Complexity reshaped

# Load
frame <- fread(paste0(root_path, "/master_clean.csv"))

# Variables to select
var <- c("plot", "DOY", "shannon_vertical", "shannon_horizontal", "Slope_N")

# Variables extraction
FSC <- frame[, ..var]
FSC <- melt(FSC, 
            id.vars = c("plot", "DOY"),
            measure.vars = c("shannon_vertical", "shannon_horizontal", "Slope_N"),
            variable.name = "FSC")
FSC$plot <- as.character(FSC$plot)

# Export
fwrite(FSC, paste0(root_path, "/fsc_reshaped.csv"))

#' -----------------------------------------------------------------------------
#' All together

reshaped <- merge(diversity_reshape, 
                  FSC, 
                  by = "plot", 
                  all.x = TRUE, 
                  all.y = TRUE,
                  allow.cartesian=TRUE)

# Export
fwrite(reshaped, paste0(root_path, "/master_reshaped.csv"))

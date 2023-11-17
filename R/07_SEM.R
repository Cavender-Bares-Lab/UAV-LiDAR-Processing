################################################################################
#' @title Structural Equation Models
################################################################################

#' @description Structural Equation Models to explain the role of different levels
#' of diversity in seasonal structural stability and overyielding
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lavaan)
library(lavaanPlot)
library(piecewiseSEM)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))

#' -----------------------------------------------------------------------------
#' Data reshaping

data <- frame[, c("plot_new", "PA", "Block", "DOY", "overyielding",
                  "cv_maximun_height", "Pgap", "Slope_Hill1", 
                  "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                  "TD_PSV", "FD_PSV", "PD_PSV")]

cv_metrics <- data[, .(CV_slope = 1/(sd(Slope_Hill1)/mean(Slope_Hill1)),
                       CV_ch = 1/(sd(cv_maximun_height)/mean(cv_maximun_height)),
                       CV_pgap = 1/(sd(Pgap)/mean(Pgap))), 
                   by = c("plot_new", "PA", "Block", "overyielding",
                          "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                          "TD_PSV", "FD_PSV", "PD_PSV")]

# PSV != 0
cv_metrics <- na.exclude(cv_metrics)

# Transformations
cv_metrics$hill0_phylo <- log(cv_metrics$hill0_phylo)
cv_metrics$hill0_FD_q <- log(cv_metrics$hill0_FD_q)

# Melt by LiDAR
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "overyielding", 
                               "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("CV_slope", "CV_ch", "CV_pgap"),
                   variable.name = "LiDAR_metric",
                   value.name = "LiDAR")

cv_metrics$LiDAR_metric <- as.factor(cv_metrics$LiDAR_metric)
cv_metrics$LiDAR_metric <- factor(cv_metrics$LiDAR_metric, 
                                  levels = c("CV_ch", "CV_pgap", "CV_slope"),
                                  labels = c("Height heterogeneity", "Gap probability", "Structural complexity"))

# Melt by diversity
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "overyielding", 
                               "LiDAR_metric", "LiDAR", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
                   variable.name = "diversity_metric",
                   value.name = "diversity")

cv_metrics$diversity_metric <- as.factor(cv_metrics$diversity_metric)
cv_metrics$diversity_metric <- factor(cv_metrics$diversity_metric, 
                                      levels = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
                                      labels = c("Taxonomic", "Phylogenetic", "Functional"))

# Melt by species variability
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "overyielding", 
                               "LiDAR_metric", "LiDAR", 
                               "diversity_metric", "diversity"),
                   measure.vars = c("TD_PSV", "FD_PSV", "PD_PSV"),
                   variable.name = "SV_metric",
                   value.name = "SV")

cv_metrics$SV_metric <- as.factor(cv_metrics$SV_metric)
cv_metrics$SV_metric <- factor(cv_metrics$SV_metric, 
                               levels = c("TD_PSV", "FD_PSV", "PD_PSV"),
                               labels = c("Taxonomic", "Phylogenetic", "Functional"))

# ------------------------------------------------------------------------------
# Structural Equation Modeling

# Transform
cv_metrics$LiDAR <- log(cv_metrics$LiDAR)

  
# Taxonomic model
taxonomic <- cv_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), taxonomic),
  lmer(overyielding ~ LiDAR + diversity + SV + (1|Block), taxonomic)
  )


summary(taxonomic_model)

# Phylogenetic model
phylogenetic <- cv_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Phylogenetic" &
                          SV_metric == "Phylogenetic", ]

phylogenetic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), phylogenetic),
  lmer(overyielding ~ LiDAR + diversity + SV + (1|Block), phylogenetic)
)

summary(phylogenetic_model)


# Functional model
functional <- cv_metrics[LiDAR_metric == "Structural complexity" &
                             diversity_metric == "Phylogenetic" &
                             SV_metric == "Phylogenetic", ]

functional_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), functional),
  lmer(overyielding ~ LiDAR + diversity + SV + (1|Block), functional)
)

summary(functional_model)

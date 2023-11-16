################################################################################
#' @title Phenological effects
################################################################################

#' @description Phenological effect on FSC and their association with 
#' plot metrics
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lavaan)
library(lavaanPlot)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean.csv"))
frame[PA == 1, plot_type := "Angiosperms"]
frame[PA == 0, plot_type := "Gymnosperms"]
frame[PA > 0 & PA < 1, plot_type := "Mixture"]

#' -----------------------------------------------------------------------------
#' Data reshaping

AWP <- frame[, c("DOY", "total_AWP", "cv_maximun_height", "Pgap", "Slope_Hill1", "plot_type", "plot_new", "PA", "hill0_taxa", "hill0_phylo", "hill0_FD_q", "TD_PSV", "FD_PSV", "PD_PSV")]
sigmaAWPD <- frame[, c("DOY", "sd_AWP", "cv_maximun_height", "Pgap", "Slope_Hill1", "plot_type", "plot_new", "PA", "hill0_taxa", "hill0_phylo", "hill0_FD_q", "TD_PSV", "FD_PSV", "PD_PSV")]

AWP$type <- "Annual wood productivity"
sigmaAWPD$type <- "Tree growth variability" 

colnames(AWP)[2] <- "metric"
colnames(sigmaAWPD)[2] <- "metric"

data <- rbind(AWP, sigmaAWPD)
data$type <- as.factor(data$type)
data$type <- factor(data$type, levels = c("Annual wood productivity", "Tree growth variability"))

cv_metrics <- data[, .(CV_slope = sd(Slope_Hill1)/mean(Slope_Hill1),
                       CV_ch = sd(cv_maximun_height)/mean(cv_maximun_height),
                       CV_pgap = sd(Pgap)/mean(Pgap)), 
                   by = c("plot_new", "metric", "plot_type", "type", "PA", 
                          "hill0_taxa", "hill0_phylo", "hill0_FD_q", "TD_PSV", 
                          "FD_PSV", "PD_PSV")]

# PSV != 0
cv_metrics <- na.exclude(cv_metrics)

# Melt by LiDAR
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "plot_type", "type", "metric", "PA", 
                               "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("CV_slope", "CV_ch", "CV_pgap"),
                   variable.name = "LiDAR_metric",
                   value.name = "LiDAR")

cv_metrics[LiDAR_metric == "CV_ch", LiDAR_metric := "Height heterogeneity"]
cv_metrics[LiDAR_metric == "CV_pgap", LiDAR_metric := "Gap probability"]
cv_metrics[LiDAR_metric == "CV_slope", LiDAR_metric := "Structural complexity"]

cv_metrics$LiDAR_metric <- as.factor(cv_metrics$LiDAR_metric)
cv_metrics$LiDAR_metric <- factor(cv_metrics$LiDAR_metric, levels = c("Height heterogeneity",
                                                              "Gap probability",
                                                              "Structural complexity"))

# Melt by diversity
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "plot_type", "type", "metric", "PA", 
                               "LiDAR_metric", "LiDAR", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
                   variable.name = "diversity_metric",
                   value.name = "diversity")

cv_metrics[diversity_metric == "hill0_taxa", diversity_metric := "Taxonomic"]
cv_metrics[diversity_metric == "hill0_phylo", diversity_metric := "Phylogenetic"]
cv_metrics[diversity_metric == "hill0_FD_q", diversity_metric := "Functional"]

cv_metrics$diversity_metric <- as.factor(cv_metrics$diversity_metric)
cv_metrics$diversity_metric <- factor(cv_metrics$diversity_metric, levels = c("Taxonomic",
                                                                              "Phylogenetic",
                                                                              "Functional"))

# Melt by species releatness
cv_metrics <- melt(cv_metrics, 
                   id.vars = c("plot_new", "plot_type", "type", "metric", "PA", 
                               "LiDAR_metric", "LiDAR", 
                               "diversity_metric", "diversity"),
                   measure.vars = c("TD_PSV", "FD_PSV", "PD_PSV"),
                   variable.name = "SV_metric",
                   value.name = "SV")

cv_metrics[SV_metric == "TD_PSV", SV_metric := "Taxonomic"]
cv_metrics[SV_metric == "FD_PSV", SV_metric := "Phylogenetic"]
cv_metrics[SV_metric == "PD_PSV", SV_metric := "Functional"]

cv_metrics$SV_metric <- as.factor(cv_metrics$SV_metric)
cv_metrics$SV_metric <- factor(cv_metrics$SV_metric, levels = c("Taxonomic",
                                                                "Phylogenetic",
                                                                "Functional"))

# ------------------------------------------------------------------------------
# Structural Equation Modeling

# Transform
cv_metrics$LiDAR <- log(cv_metrics$LiDAR)
cv_metrics$metric <- log(cv_metrics$metric)

# Model
model <- '
  LiDAR ~ diversity + SV
  metric ~ LiDAR + diversity + SV
  ' #diversity ~~ SV
  
# Taxonomic model
taxonomic <- cv_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic_AWP <- sem(model, 
                     data = taxonomic[type == "Annual wood productivity",])
lavaanPlot(name = "model1", taxonomic_AWP, coefs = TRUE)
summary(taxonomic_AWP, rsq = TRUE, fit.measures = TRUE) 

taxonomic_sAWP <- sem(model, 
                     data = taxonomic[type == "Tree growth variability",])
lavaanPlot(name = "model1", taxonomic_sAWP, coefs = TRUE)
summary(taxonomic_sAWP, rsq = TRUE, fit.measures = TRUE) 

# Phylogenetic model
phylogenetic <- cv_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Phylogenetic" &
                          SV_metric == "Phylogenetic", ]

phylogenetic_AWP <- sem(model, 
                     data = phylogenetic[type == "Annual wood productivity",])
lavaanPlot(name = "model1", phylogenetic_AWP, coefs = TRUE)
summary(phylogenetic_AWP, rsq = TRUE, fit.measures = TRUE) 

phylogenetic_sAWP <- sem(model, 
                      data = phylogenetic[type == "Tree growth variability",])
lavaanPlot(name = "model1", phylogenetic_sAWP, coefs = TRUE)
summary(phylogenetic_sAWP, rsq = TRUE, fit.measures = TRUE) 

# Functional model
functional <- cv_metrics[LiDAR_metric == "Structural complexity" &
                         diversity_metric == "Functional" &
                         SV_metric == "Functional", ]

functional_AWP <- sem(model, 
                      data = functional[type == "Annual wood productivity",])
lavaanPlot(name = "model1", functional_AWP, coefs = TRUE)
summary(functional_AWP, rsq = TRUE, fit.measures = TRUE) 

functional_sAWP <- sem(model, 
                       data = functional[type == "Tree growth variability",])
lavaanPlot(name = "model1", functional_sAWP, coefs = TRUE)
summary(functional_sAWP, rsq = TRUE, fit.measures = TRUE) 

# ------------------------------------------------------------------------------
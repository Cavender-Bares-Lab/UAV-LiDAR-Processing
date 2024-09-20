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
library(piecewiseSEM)
library(lme4)
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

data <- frame[, c("plot_new", "PA", "Block", "DOY", "NE",
                  "cv_maximun_height", "Pgap", "Slope_Hill1", 
                  "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                  "TD_PSV", "FD_PSV", "PD_PSV")]

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_ch = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_pgap = mean(Pgap)/sd(Pgap)), 
                   by = c("plot_new", "PA", "Block", "NE",
                          "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                          "TD_PSV", "FD_PSV", "PD_PSV")]

# PSV != 0
ss_metrics <- na.exclude(ss_metrics)

# Transformations
ss_metrics$hill0_taxa <- log(ss_metrics$hill0_taxa)
ss_metrics$hill0_phylo <- log(ss_metrics$hill0_phylo)
ss_metrics$hill0_FD_q <- log(ss_metrics$hill0_FD_q)
ss_metrics$NE <- log(ss_metrics$NE)

# Melt by LiDAR
ss_metrics <- melt(ss_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "NE", 
                               "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("SS_slope", "SS_ch", "SS_pgap"),
                   variable.name = "LiDAR_metric",
                   value.name = "LiDAR")

ss_metrics$LiDAR_metric <- as.factor(ss_metrics$LiDAR_metric)
ss_metrics$LiDAR_metric <- factor(ss_metrics$LiDAR_metric, 
                                  levels = c("SS_ch", "SS_pgap", "SS_slope"),
                                  labels = c("Height heterogeneity", "Gap probability", "Structural complexity"))

# Melt by diversity
ss_metrics <- melt(ss_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "NE", 
                               "LiDAR_metric", "LiDAR", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
                   variable.name = "diversity_metric",
                   value.name = "diversity")

ss_metrics$diversity_metric <- as.factor(ss_metrics$diversity_metric)
ss_metrics$diversity_metric <- factor(ss_metrics$diversity_metric, 
                                      levels = c("hill0_taxa", "hill0_phylo", "hill0_FD_q"),
                                      labels = c("Taxonomic", "Phylogenetic", "Functional"))

# Melt by species variability
ss_metrics <- melt(ss_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "NE", 
                               "LiDAR_metric", "LiDAR", 
                               "diversity_metric", "diversity"),
                   measure.vars = c("TD_PSV", "FD_PSV", "PD_PSV"),
                   variable.name = "SV_metric",
                   value.name = "SV")

ss_metrics$SV_metric <- as.factor(ss_metrics$SV_metric)
ss_metrics$SV_metric <- factor(ss_metrics$SV_metric, 
                               levels = c("TD_PSV", "FD_PSV", "PD_PSV"),
                               labels = c("Taxonomic", "Phylogenetic", "Functional"))

# Transform
ss_metrics$LiDAR <- log(ss_metrics$LiDAR)


# ------------------------------------------------------------------------------
# Structural Equation Modeling

################################################################################
# Fractal geometry

# Taxonomic model
taxonomic <- ss_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic <- taxonomic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
taxonomic <- as.data.table(scale(taxonomic))
taxonomic$Block <- as.character(taxonomic$Block)

taxonomic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), taxonomic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), taxonomic)
  )

summary(taxonomic_model)

# Phylogenetic model
phylogenetic <- ss_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Phylogenetic" &
                          SV_metric == "Phylogenetic", ]

phylogenetic <- phylogenetic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
phylogenetic <- as.data.table(scale(phylogenetic))
phylogenetic$Block <- as.character(phylogenetic$Block)

phylogenetic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), phylogenetic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), phylogenetic)
)

summary(phylogenetic_model)


# Functional model
functional <- ss_metrics[LiDAR_metric == "Structural complexity" &
                             diversity_metric == "Functional" &
                             SV_metric == "Functional", ]

functional <- functional[, c("LiDAR", "diversity", "SV", "NE", "Block")]
functional <- as.data.table(scale(functional))
functional$Block <- as.character(functional$Block)

functional_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), functional),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), functional)
)

summary(functional_model)

################################################################################
# Height heterogeneity

# Taxonomic model
taxonomic <- ss_metrics[LiDAR_metric == "Height heterogeneity" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic <- taxonomic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
taxonomic <- as.data.table(scale(taxonomic))
taxonomic$Block <- as.character(taxonomic$Block)

taxonomic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), taxonomic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), taxonomic)
)

summary(taxonomic_model)

# Phylogenetic model
phylogenetic <- ss_metrics[LiDAR_metric == "Height heterogeneity" &
                             diversity_metric == "Phylogenetic" &
                             SV_metric == "Phylogenetic", ]

phylogenetic <- phylogenetic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
phylogenetic <- as.data.table(scale(phylogenetic))
phylogenetic$Block <- as.character(phylogenetic$Block)

phylogenetic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), phylogenetic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), phylogenetic)
)

summary(phylogenetic_model)


# Functional model
functional <- ss_metrics[LiDAR_metric == "Height heterogeneity" &
                           diversity_metric == "Functional" &
                           SV_metric == "Functional", ]

functional <- functional[, c("LiDAR", "diversity", "SV", "NE", "Block")]
functional <- as.data.table(scale(functional))
functional$Block <- as.character(functional$Block)

functional_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), functional),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), functional)
)

summary(functional_model)

################################################################################
# Gap probability

# Taxonomic model
taxonomic <- ss_metrics[LiDAR_metric == "Gap probability" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic <- taxonomic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
taxonomic <- as.data.table(scale(taxonomic))
taxonomic$Block <- as.character(taxonomic$Block)

taxonomic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), taxonomic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), taxonomic)
)

summary(taxonomic_model)

# Phylogenetic model
phylogenetic <- ss_metrics[LiDAR_metric == "Gap probability" &
                             diversity_metric == "Phylogenetic" &
                             SV_metric == "Phylogenetic", ]

phylogenetic <- phylogenetic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
phylogenetic <- as.data.table(scale(phylogenetic))
phylogenetic$Block <- as.character(phylogenetic$Block)

phylogenetic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), phylogenetic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), phylogenetic)
)

summary(phylogenetic_model)


# Functional model
functional <- ss_metrics[LiDAR_metric == "Gap probability" &
                           diversity_metric == "Functional" &
                           SV_metric == "Functional", ]

functional <- functional[, c("LiDAR", "diversity", "SV", "NE", "Block")]
functional <- as.data.table(scale(functional))
functional$Block <- as.character(functional$Block)

functional_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), functional),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), functional)
)

summary(functional_model)


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
library(MASS)
options(scipen = 99999)

#' -----------------------------------------------------------------------------
#' Working path

#root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Load data

frame <- fread(paste0(root_path, "/master_clean (2024-09-19).csv"))

#' -----------------------------------------------------------------------------
#' Data reshaping

data <- frame[, c("plot_new", "PA", "Block", "DOY", "NE",
                  "cv_maximun_height", "FC", "Slope_Hill1", 
                  "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                  "TD_PSV", "FD_PSV", "PD_PSV")]

ss_metrics <- data[, .(SS_slope = mean(Slope_Hill1)/sd(Slope_Hill1),
                       SS_hh = mean(cv_maximun_height)/sd(cv_maximun_height),
                       SS_fc = mean(FC)/sd(FC)), 
                   by = c("plot_new", "PA", "Block", "NE",
                          "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                          "TD_PSV", "FD_PSV", "PD_PSV")]

# PSV != 0
ss_metrics <- na.exclude(ss_metrics)

# Box-Cox transformation
for(i in 4:ncol(ss_metrics)) {
  
  variable <- as.numeric(as.matrix(ss_metrics[, .SD, .SDcols = i]))
  test1 <- shapiro.test(variable)$p.value
  
  #Log
  log_variable <- log10(variable)
  test2 <- shapiro.test(log_variable)$p.value
  
  # Boxcox
  b <- boxcox(lm(variable ~ 1))
  lambda <- b$x[which.max(b$y)]
  box_variable <- (variable ^ lambda - 1) / lambda
  test3 <- shapiro.test(box_variable)$p.value
  
  # Sqrt
  sqrt_variable <- sqrt(variable)
  test4 <- shapiro.test(sqrt_variable)$p.value
  
  max_p <- which.max(c(test1, test2, test3, test4))
  
  if(max_p == 2) {
    ss_metrics[, i] <- log_variable
  } else if(max_p == 3) {
    ss_metrics[, i] <- box_variable
  } else if(max_p == 4) {
    ss_metrics[, i] <- sqrt_variable
  }
}

# Melt by LiDAR
ss_metrics <- melt(ss_metrics, 
                   id.vars = c("plot_new", "PA", "Block", "NE", 
                               "hill0_taxa", "hill0_phylo", "hill0_FD_q", 
                               "TD_PSV", "FD_PSV", "PD_PSV"),
                   measure.vars = c("SS_slope", "SS_hh", "SS_fc"),
                   variable.name = "LiDAR_metric",
                   value.name = "LiDAR")

ss_metrics$LiDAR_metric <- as.factor(ss_metrics$LiDAR_metric)
ss_metrics$LiDAR_metric <- factor(ss_metrics$LiDAR_metric, 
                                  levels = c("SS_hh", "SS_fc", "SS_slope"),
                                  labels = c("Height heterogeneity", "Fractional cover", "Structural complexity"))

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
#ss_metrics$LiDAR <- log(ss_metrics$LiDAR)


# ------------------------------------------------------------------------------
# Structural Equation Modeling

################################################################################
# Fractal geometry

# Taxonomic model
taxonomic <- ss_metrics[LiDAR_metric == "Structural complexity" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic <- taxonomic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
taxonomic[, 1:4] <- as.data.table(scale(taxonomic[, 1:4]))

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
phylogenetic[, 1:4] <- as.data.table(scale(phylogenetic[, 1:4]))

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
functional[, 1:4] <- as.data.table(scale(functional[, 1:4]))


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
taxonomic[, 1:4] <- as.data.table(scale(taxonomic[, 1:4]))

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
phylogenetic[, 1:4] <- as.data.table(scale(phylogenetic[, 1:4]))

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
functional[, 1:4] <- as.data.table(scale(functional[, 1:4]))

functional_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), functional),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), functional)
)

summary(functional_model)

################################################################################
# Fractional cover

# Taxonomic model
taxonomic <- ss_metrics[LiDAR_metric == "Fractional cover" &
                          diversity_metric == "Taxonomic" &
                          SV_metric == "Taxonomic", ]

taxonomic <- taxonomic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
taxonomic[, 1:4] <- as.data.table(scale(taxonomic[, 1:4]))

taxonomic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), taxonomic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), taxonomic)
)

summary(taxonomic_model)

# Phylogenetic model
phylogenetic <- ss_metrics[LiDAR_metric == "Fractional cover" &
                             diversity_metric == "Phylogenetic" &
                             SV_metric == "Phylogenetic", ]

phylogenetic <- phylogenetic[, c("LiDAR", "diversity", "SV", "NE", "Block")]
phylogenetic[, 1:4] <- as.data.table(scale(phylogenetic[, 1:4]))

phylogenetic_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), phylogenetic),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), phylogenetic)
)

summary(phylogenetic_model)

# Functional model
functional <- ss_metrics[LiDAR_metric == "Fractional cover" &
                           diversity_metric == "Functional" &
                           SV_metric == "Functional", ]

functional <- functional[, c("LiDAR", "diversity", "SV", "NE", "Block")]
functional[, 1:4] <- as.data.table(scale(functional[, 1:4]))

functional_model <- psem(
  lmer(LiDAR ~  diversity + SV + (1|Block), functional),
  lmer(NE ~ LiDAR + diversity + SV + (1|Block), functional)
)

summary(functional_model)


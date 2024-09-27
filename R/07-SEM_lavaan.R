################################################################################
#' @title Partial Least Square Path Modelling
################################################################################

#' @description It performs a Partial Least Square Path Modelling for predicting
#' the effect of diversity, variability, and seasonal structural stability on NBE.
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(lavaan)
library(scales)
library(semPlot)

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

# Transformations
ss_metrics$hill0_taxa <- log10(ss_metrics$hill0_taxa)
#ss_metrics$hill0_phylo <- log10(ss_metrics$hill0_phylo)
#ss_metrics$hill0_FD_q <- log10(ss_metrics$hill0_FD_q)
#ss_metrics$NE <- log10(ss_metrics$NE)

ss_metrics$SS_slope <- log10(ss_metrics$SS_slope)
ss_metrics$SS_hh <- log10(ss_metrics$SS_hh)
ss_metrics$SS_fc <- log10(ss_metrics$SS_fc)

ss_metrics$NE <- sqrt(ss_metrics$NE)

# Scale variables
ss_metrics <- as.data.table(scale(ss_metrics[, 4:13], center = FALSE, scale = TRUE))

# ------------------------------------------------------------------------------
# Structural Regression with One Endogenous Variable

#--------------------------------------------
# Canopy height heterogeneity

height_heterogeneity <- '
# measurement model
Diversity =~ hill0_taxa + hill0_phylo + hill0_FD_q
Variability =~ TD_PSV + FD_PSV + PD_PSV
Stability =~ SS_hh 
NBE =~ NE

# regressions
Stability ~ Diversity + Variability
NBE ~ Stability + Diversity + Variability
'

fit_SShh <- lavaan(model = height_heterogeneity,
                   data = ss_metrics,
                   model.type = "sem",
                   estimator = "ML",
                   se = "bootstrap",
                   bootstrap = 10)


fit_SShh <- lavaan(model = height_heterogeneity,
                   data = ss_metrics,
                   model.type = "sem")

varTable(fit_SShh)
semPaths(fit_SShh)
summary(fit_SShh, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

#--------------------------------------------
# Fractional plant cover

fractional_cover <- '
# measurement model
Diversity =~ hill0_taxa + hill0_phylo + hill0_FD_q
Variability =~ TD_PSV + FD_PSV + PD_PSV
Stability =~ SS_fc 
NBE =~ NE

# regressions
Stability ~ Diversity + Variability
NBE ~ Stability + Diversity + Variability
'

fit_SSfc <- lavaan(model = fractional_cover,
                   data = ss_metrics,
                   model.type = "sem",
                   estimator = "ML",
                   se = "bootstrap",
                   bootstrap = 1000)

varTable(fit_SSfc)
semPaths(fit_SSfc)
summary(fit_SSfc, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

#--------------------------------------------
# Structural complexity

structural_complexity <- '
# measurement model
Diversity =~ hill0_taxa + hill0_phylo + hill0_FD_q
Variability =~ TD_PSV + FD_PSV + PD_PSV
Stability =~ SS_slope 
NBE =~ NE

# regressions
Stability ~ Diversity + Variability
NBE ~ Stability + Diversity + Variability
'

fit_SSdD <- lavaan(model = structural_complexity,
                   data = ss_metrics,
                   model.type = "sem",
                   estimator = "ML",
                   se = "bootstrap",
                   bootstrap = 1000)

varTable(fit_SSdD)
semPaths(fit_SSdD)
summary(fit_SSdD, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

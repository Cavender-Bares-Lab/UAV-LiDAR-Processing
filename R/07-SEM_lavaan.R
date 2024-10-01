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
library(MASS)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/work/Projects/LiDAR/data"
#root_path <- "G:/Projects/LiDAR/data"

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
ss_metrics <- as.data.frame(ss_metrics[,c(-1,-2, -3)])

# Box-Cox transformation
for(i in 1:ncol(ss_metrics)) {
  
  variable <- ss_metrics[, i]
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

# Scale variables
ss_metrics <- as.data.table(scale(ss_metrics, center = FALSE, scale = TRUE))

# ------------------------------------------------------------------------------
# Structural Regression with One Endogenous Variable

#--------------------------------------------
# Canopy height heterogeneity

height_heterogeneity <- '
# measurement model
Diversity =~ hill0_taxa + hill0_phylo + hill0_FD_q
Variability =~ TD_PSV + PD_PSV + FD_PSV
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

varTable(fit_SShh)
semPaths(fit_SShh)
summary(fit_SShh, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

#--------------------------------------------
# Fractional plant cover

fractional_cover <- '
# measurement model
Diversity =~ hill0_taxa + hill0_phylo + hill0_FD_q
Variability =~ TD_PSV + PD_PSV + FD_PSV
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
                   bootstrap = 10)

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
                   se = "bootstrap",
                   bootstrap = 10)

varTable(fit_SSdD)
semPaths(fit_SSdD)
summary(fit_SSdD, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

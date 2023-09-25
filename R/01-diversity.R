################################################################################
#' @title Cleaning of field inventories and estimation of diversity metrics
################################################################################

#' @description Data cleaning for the plot biomass inventory and estimate 
#' diversity and yield features
#' 
#' @return A .csv file

#' --------------------------------------------------------------------------
#' Libraries

library(data.table)
library(picante)
library(Taxonstand)
library(V.PhyloMaker)
library(vegan)
library(DescTools)
library(lefse)
library(vegetarian)

#' -----------------------------------------------------------------------------
#' Set working directory 
root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Processing

# Read inventory file
data <- fread(paste0(root_path, "/fab2_allometry2.csv"))

#Set date and subset by year
data[, measurement_date := as.IDate(measurement_date), by = seq_along(1:nrow(data))]
data[, measurement_year := year(measurement_date), by = seq_along(1:nrow(data))]
data <- data[measurement_year == 2022,]

#Removing columns
data <- data[, c("year_planted", 
         "species_richness", 
         "species", 
         "treatment", 
         "Area_m2",
         "block",
         "plot",
         "row",
         "column",
         "position",
         "survey",
         "measurement_date",
         "deadmissing",
         "Biomass.conoid_conoidoid_infill")]

unique(data$species)
data[species == "Juniperus virginia", species := "Juniperus virginiana"]
data[species == "Tilia america", species := "Tilia americana"]

#-------------------------------------------------------------------------------
# Filtering trees

data <- data[Area_m2 == 100,] #Small plots
data <- data[deadmissing == "No",] #Alive
# data <- data[row != 1 | row != 10, ] #Alive

#-------------------------------------------------------------------------------
# Summary of metrics by individual.

plot_summary <- data[,
                     .(SR_real = length(unique(species)),
                       year_mean = mean(year_planted),
                       year_cv = sd(year_planted)/mean(year_planted), 
                       ntrees = .N, 
                       total_biomass = sum(Biomass.conoid_conoidoid_infill)/1000,
                       tree_gini = Gini(Biomass.conoid_conoidoid_infill,
                                   na.rm = TRUE, unbiased = TRUE,
                                   conf.level=0.95, type = "basic")[1],
                       tree_gini_lwr = Gini(Biomass.conoid_conoidoid_infill,
                                       na.rm = TRUE, unbiased = TRUE,
                                       conf.level=0.95, type = "basic")[2],
                       tree_gini_upr = Gini(Biomass.conoid_conoidoid_infill,
                                       na.rm = TRUE, unbiased = TRUE,
                                       conf.level=0.95, type = "basic")[3]),
                     by = "plot"]

#-------------------------------------------------------------------------------
# Summary of metrics by species in the plot.

species_summary <- data[, .(ntrees = .N,
                            total_biomass = sum(Biomass.conoid_conoidoid_infill/1000, na.rm = TRUE)),
                        by = c("plot", "species")]

shannon <- function(sp) {
  p.i <- sp/sum(sp)
  H <- (-1) * sum(p.i * log10(p.i))
  return(H)
}

feature_plot <- species_summary[, .(sp_H = shannon(total_biomass),
                                    sp_Hmax = shannon(rep(1, length(total_biomass))),
                                    sp_Equitavility = shannon(total_biomass)/shannon(rep(1, length(total_biomass))),
                                    sp_Negentropy = shannon(rep(1, length(total_biomass))) - shannon(total_biomass)),
                                by = "plot"]

complete <- merge(plot_summary, feature_plot, by = "plot", all.x = TRUE, all.y = TRUE)
com <- sample2matrix(species_summary[, c("plot", "total_biomass", "species")])
com <- decostand(com, method = "total")

diversity <- data.table(plot = rownames(com),
                        q0 = NA,
                        q0_std = NA,
                        q1 = NA,
                        q1_std = NA,
                        q2 = NA,
                        q2_std = NA)

for(i in 1:nrow(com)) {

  q0 <- d(com[i, ], q = 0, lev = "alpha",  boot = TRUE, boot.arg = list(num.iter = 1000))
  q1 <- d(com[i, ], q = 1, lev = "alpha",  boot = TRUE, boot.arg = list(num.iter = 1000))
  q2 <- d(com[i, ], q = 2, lev = "alpha",  boot = TRUE, boot.arg = list(num.iter = 1000))
    
  diversity$q0[i] <- q0$D.Value
  diversity$q0_std[i] <- q0$StdErr
  diversity$q1[i] <- q1$D.Value
  diversity$q1_std[i] <- q1$StdErr
  diversity$q2[i] <- q2$D.Value
  diversity$q2_std[i] <- q2$StdErr
  
}

# Merge complete and diversity
complete <- merge(complete, diversity, by = "plot")
fwrite(complete, paste0(root_path, "/diversity.csv"))

#-------------------------------------------------------------------------------
# Get proportions
traits <- fread(paste0(root_path, "/traits.csv"))
traits <- traits[, c(5, 11)]
species <- species_summary[, c(1,2,4)]
colnames(species)[2] <- "Species"

proportions <- merge(traits, species, by = "Species", all.x = TRUE, all.y = TRUE)
p <- proportions[, sum(total_biomass), by = c("plot", "Gymnosperm")]
colnames(p)[3] <- "biomass"
proportions <- data.frame(plot = "1", PA = 0)
unique_plots <- unique(p$plot)

for(i in 1:length(unique_plots)) {
  
  sub <- subset(p, plot == unique_plots[i])
  
  if(nrow(sub) == 1) {
    if(sub$Gymnosperm[1] == "N") {
      proportions[i, 1] <- unique_plots[i]
      proportions[i, 2] <- 1.00
    } else {
      proportions[i, 1] <- unique_plots[i]
      proportions[i, 2] <- 0.00
    }
  } else {
    gym <- sub[Gymnosperm == "Y", biomass]
    ang <- sub[Gymnosperm == "N", biomass]
    proportions[i, 1] <- unique_plots[i]
    proportions[i, 2] <- ang/(gym + ang)
    }
}

complete <- merge(complete, proportions, by = "plot")

FD_PD <- fread(paste0(root_path, "/FD_PD.csv"))
final <- merge(complete, FD_PD, by = "plot", all.x = TRUE, all.y = TRUE)
fwrite(final, paste0(root_path, "/diversity_complete.csv"))

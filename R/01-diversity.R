################################################################################
#' @title Cleaning of field inventories and estimation of diversity metrics
################################################################################

#' @description Data cleaning for the plot biomass files and estimate diversity 
#' features
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
library(phytools)
library(ape)
library(lefse)

#' -----------------------------------------------------------------------------
#' Processing

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
data <- fread(paste0(root_path, "/fab2_allometry2.csv"))

# Transform to date and get year
data$measurement_date <- as.IDate(data$measurement_date)
data$measurement_year <- year(data$measurement_date)

# Remove data minor errors
data[species == "Juniperus virginia", species := "Juniperus virginiana"]
data[species == "Tilia america", species := "Tilia americana"]

data[plot == "1020-154", plot := "1020_154"]
data[plot == "1020-2016", plot := "1020_2016"]
data[plot == "1020-2017", plot := "1020_2017"]
data[plot == "1020-2018", plot := "1020_2018"]
data[plot == "1011-149", plot := "1011_149"]
data[plot == "1011-150", plot := "1011_150"]
data[plot == "1019-152", plot := "1019_152"]
data[plot == "1019-153", plot := "1019_153"]

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

# Select 2022 inventory
data <- subset(data, year(measurement_date) == 2022)

#-------------------------------------------------------------------------------
# Summary of metrics by plot.

plot_summary <- data[deadmissing == "No", 
                     .(SR_real = length(unique(species)),
                       year_mean = mean(year_planted),
                       year_cv = sd(year_planted)/mean(year_planted), 
                       ntrees = .N, 
                       biomass = sum(Biomass.conoid_conoidoid_infill, na.rm = TRUE),
                       tree_size_inequality = Gini(Biomass.conoid_conoidoid_infill,
                                                   na.rm = TRUE, unbiased = TRUE,
                                                   conf.level=0.95, type = "basic")[1]),
                     by = "plot"]

#-------------------------------------------------------------------------------
# Summary of metrics by species in the plot.

species_summary <- data[deadmissing == "No", .(ntrees = .N,
                                               biomass = sum(Biomass.conoid_conoidoid_infill, na.rm = TRUE)),
                        by = c("plot", "species")]

shannon <- function(sp) {
  p.i <- sp/sum(sp)
  H <- (-1) * sum(p.i * log10(p.i))
  return(H)
}

feature_plot <- species_summary[, .(H = shannon(biomass),
                                    H_normalized = shannon(biomass)/shannon(rep(1, length(biomass)))),
                                by = "plot"]

complete <- merge(plot_summary, feature_plot, 
                  by = "plot", 
                  all.x = TRUE, 
                  all.y = TRUE)

fwrite(complete, "diversity.csv")

#-------------------------------------------------------------------------------
# Get proportion of angiosperms
traits <- fread(paste0(root_path, "/traits.csv"))
traits <- traits[, c(5, 11)]
species <- species_summary[, c(1,2,4)]
colnames(species)[2] <- "Species"

proportions <- merge(traits, species, by = "Species", all.x = TRUE, all.y = TRUE)
p <- proportions[, sum(biomass), by = c("plot", "Gymnosperm")]
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

complete <- merge(complete, proportions, 
                  by = "plot", 
                  all.x = TRUE, 
                  all.y = TRUE)

#' -----------------------------------------------------------------------------
#' Taxonomic data cleaning
species <- fread(paste0(root_path, "/traits.csv"))
species_names <- species$species
species <- as.matrix(species[, c(1:5, 7)])
rownames(species) <- species_names
taxonomic <- taxa2dist(species, varstep=TRUE)
taxonomic <- hclust(taxonomic)
plot(taxonomic, hang = -1)

#' -----------------------------------------------------------------------------
#' Phylogenetic data cleaning

# Read and select species
species <- fread(paste0(root_path, "/traits.csv"))
species <- species[, c(6,4,3)]
colnames(species) <- c("species", "family", "genus")

# Get tree
hypotheses <- phylo.maker(species, scenarios = "S3")

# Resolve Multichotomies
phylo <- multi2di(hypotheses$scenario.3)
is.binary.phylo(phylo) #Test for Binary Tree
is.ultrametric(phylo) #Test if a Tree is Ultrametric
phylo$edge.length[phylo$edge.length <= 0] <- tol
is.ultrametric(phylo)
plot(phylo)

#' -----------------------------------------------------------------------------
#' Functional data cleaning

# Read and select species
species <- fread(paste0(root_path, "/traits.csv"))
species_names <- species$species
traits <- as.matrix(species[, c("Wood_Density",
                                "LMA",
                                "Leaf_habit_Decid=0",
                                "HT_quantile",
                                "CR_quantile",
                                "slenderness_quantile")])
rownames(traits) <- species_names
traits <- as.matrix(scale(traits, center = TRUE, scale = TRUE))
functional <- hclust(dist(traits))
plot(functional, hang = -1)

#' -----------------------------------------------------------------------------
#' Biomass data 

community <- species_summary[, c("plot", "biomass", "species")]
community$species <- chartr(" ", "_", community$species)
community <- sample2matrix(community)
master_matrix <- decostand(community, method = "hellinger") 

#' -----------------------------------------------------------------------------
#' Diversity metrics

# Standardize formats to phylo
taxonomic <- as.phylo(taxonomic)
taxonomic$edge.length[taxonomic$edge.length <= 0] <- tol
phylogenetic <- phylo
functional <- as.phylo(functional)
functional$edge.length[functional$edge.length <= 0] <- tol

# Maching with communities
taxonomic_matched <- match.phylo.comm(phy = taxonomic,
                                      comm = master_matrix)

phylogenetic_matched <- match.phylo.comm(phy = phylogenetic,
                                         comm = master_matrix)

functional_matched <- match.phylo.comm(phy = functional,
                                       comm = master_matrix)

# Plot

plot(taxonomic_matched$phy)
plot(phylogenetic_matched$phy)
plot(functional_matched$phy)

# Faiths index

TD_faith <- pd(taxonomic_matched$comm, taxonomic_matched$phy)$PD

PD_faith <- pd(phylogenetic_matched$comm, phylogenetic_matched$phy)$PD

FD_faith <- pd(functional_matched$comm, functional_matched$phy)$PD

# Standardized effect size of MPD
TD_MPD <- ses.mpd(taxonomic_matched$comm, 
                  cophenetic(taxonomic_matched$phy), 
                  null.model = "taxa.labels", 
                  runs = 99,
                  abundance.weighted = TRUE)

PD_MPD <- ses.mpd(phylogenetic_matched$comm, 
                  cophenetic(phylogenetic_matched$phy), 
                  null.model = "taxa.labels", 
                  runs = 99,
                  abundance.weighted = TRUE)

FD_MPD <- ses.mpd(functional_matched$comm, 
                  cophenetic(functional_matched$phy), 
                  null.model = "taxa.labels", 
                  runs = 99,
                  abundance.weighted = TRUE)

# Rao's quadratic entropy
TD_raoD <- raoD(taxonomic_matched$comm, taxonomic_matched$phy)$Dkk

PD_raoD <- raoD(phylogenetic_matched$comm, phylogenetic_matched$phy)$Dkk

FD_raoD <- raoD(functional_matched$comm, functional_matched$phy)$Dkk

# Species variability

TD_PSV <- psv(taxonomic_matched$comm, 
              taxonomic_matched$phy)$PSVs

PD_PSV <- psv(phylogenetic_matched$comm, 
              phylogenetic_matched$phy)$PSVs

FD_PSV <- psv(functional_matched$comm, 
              functional_matched$phy)$PSVs

# Species richness

TD_PSR <- psr(taxonomic_matched$comm, 
              taxonomic_matched$phy)$PSR

PD_PSR <- psr(phylogenetic_matched$comm, 
              phylogenetic_matched$phy)$PSR

FD_PSR <- psr(functional_matched$comm, 
              functional_matched$phy)$PSR

# Species evenness

TD_PSE <- pse(taxonomic_matched$comm, 
              taxonomic_matched$phy)$PSEs

PD_PSE <- pse(phylogenetic_matched$comm, 
              phylogenetic_matched$phy)$PSEs

FD_PSE <- pse(functional_matched$comm, 
              functional_matched$phy)$PSEs

# Species clustering

TD_PSC <- psc(taxonomic_matched$comm, 
              taxonomic_matched$phy)$PSCs

PD_PSC <- psc(phylogenetic_matched$comm, 
              phylogenetic_matched$phy)$PSCs

FD_PSC <- psc(functional_matched$comm, 
              functional_matched$phy)$PSCs

# Capture diversity in a frame

diversity <- data.table(plot = rownames(community),
                        TD_faith = TD_faith,
                        PD_faith = PD_faith,
                        FD_faith = FD_faith,
                        TD_MPD = TD_MPD$mpd.obs,
                        PD_MPD = PD_MPD$mpd.obs,
                        FD_MPD = FD_MPD$mpd.obs,
                        TD_raoD = TD_raoD,
                        PD_raoD = PD_raoD,
                        FD_raoD = FD_raoD,
                        TD_PSV = TD_PSV,
                        PD_PSV = PD_PSV,
                        FD_PSV = FD_PSV,
                        TD_PSR = TD_PSR,
                        PD_PSR = PD_PSR,
                        FD_PSR = FD_PSR,
                        TD_PSE = TD_PSE,
                        PD_PSE = PD_PSE,
                        FD_PSE = FD_PSE,
                        TD_PSC = TD_PSC,
                        PD_PSC = PD_PSC,
                        FD_PSC = FD_PSC)

complete <- merge(complete, diversity, by = "plot", all.x = TRUE, all.y = TRUE)
fwrite(complete, paste0(root_path, "/diversity.csv"))



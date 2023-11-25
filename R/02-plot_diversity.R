################################################################################
#' @title Estimation of diversity metrics from plots
################################################################################

#' @description Steps for getting diversity metrics at different levels 
#' 
#' @return A .csv file

#' --------------------------------------------------------------------------
#' Libraries

library(data.table)
library(picante)
library(Taxonstand)
library(V.PhyloMaker)
library(hillR)

#' -----------------------------------------------------------------------------
#' Working path

root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"
#root_path <- "F:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Functions

hill <- function(n_points, q) {
  p <- n_points/sum(n_points)
  return((sum(p^q)^(1 / (1 - q))))
}

#devtools::source_url("https://github.com/ShanKothari/DecomposingFD/blob/master/R/AlphaFD.R?raw=TRUE")

#' -----------------------------------------------------------------------------
#' Processing

# Load data
species_summary <- fread(paste0(root_path, "/species_summary.csv"))

#' -----------------------------------------------------------------------------
#' Biomass data per species

community <- species_summary[, c("plot_new", "volume", "species")]
#community$volume <- log(community$volume + 1)
community$species <- chartr(" ", "_", community$species)
community <- sample2matrix(community)
master_matrix <- decostand(community, method = "total") 

#' -----------------------------------------------------------------------------
#' Create file for compile results

diversity <- data.table(plot_new = rownames(community))

#' -----------------------------------------------------------------------------
#' Taxonomic alpha diversity

diversity$hill0_taxa <- hill_taxa(comm = master_matrix, q = 0)
diversity$hill1_taxa <- hill_taxa(comm = master_matrix, q = 1)
diversity$hill2_taxa <- hill_taxa(comm = master_matrix, q = 2)

#' -----------------------------------------------------------------------------
#' Phylogenetic diversity

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
tol = 1e-9
phylo$edge.length[phylo$edge.length <= 0] <- tol
is.ultrametric(phylo)

# Match communities and phylo
match <- match.phylo.comm(phy = phylo,
                          comm = master_matrix)

# Hill phylo
diversity$hill0_phylo <- hill_phylo(comm = match$comm, tree = match$phy, q = 0)
diversity$hill1_phylo <- hill_phylo(comm = match$comm, tree = match$phy, q = 1)
diversity$hill2_phylo <- hill_phylo(comm = match$comm, tree = match$phy, q = 2)

#' -----------------------------------------------------------------------------
#' Basic Functional diversity

# Read and select species
species <- fread(paste0(root_path, "/traits.csv"))
species_names <- species$species
traits <- as.matrix(species[, c("Wood_Density",
                                "LMA",
                                "HT_quantile",
                                #"CR_quantile",
                                #"slenderness_quantile",
                                "RGR",
                                "shade_tolerance")])
rownames(traits) <- species_names
traits <- as.matrix(scale(traits, center = TRUE, scale = TRUE))

hill0_func <- hill_func(comm = master_matrix, traits = traits, q = 0)
hill1_func <- hill_func(comm = master_matrix, traits = traits, q = 1)
hill2_func <- hill_func(comm = master_matrix, traits = traits, q = 2)

hill0_func <- t(hill0_func)
hill1_func <- t(hill1_func)
hill2_func <- t(hill2_func)

colnames(hill0_func) <- paste0("hill0_", colnames(hill0_func))
colnames(hill1_func) <- paste0("hill1_", colnames(hill1_func))
colnames(hill2_func) <- paste0("hill2_", colnames(hill2_func))

diversity <- cbind(diversity, hill0_func, hill1_func, hill2_func)

#' -----------------------------------------------------------------------------
#' Functional trait dispersion (Scheiner et al. 2016)

# Get trait distance
trait_distance <- dist(traits, diag = TRUE, upper = TRUE)

# Rescale
trait_distance <- (trait_distance-min(trait_distance))/
                  (max(trait_distance)-min(trait_distance))

#FTD_hill0 <- FTD.comm(tdmat = trait_distance, 
#                      spmat = master_matrix, 
#                      q = 0,
#                      abund = TRUE,
#                      match.names = TRUE)$com.FTD

#FTD_hill1 <- FTD.comm(tdmat = trait_distance, 
#                      spmat = master_matrix, 
#                      q = 1,
#                      abund = TRUE,
#                      match.names = TRUE)$com.FTD

#FTD_hill2 <- FTD.comm(tdmat = trait_distance, 
#                      spmat = master_matrix, 
#                      q = 2,
#                      abund = TRUE,
#                      match.names = TRUE)$com.FTD

#colnames(FTD_hill0) <- paste0("hill0_", colnames(FTD_hill0))
#colnames(FTD_hill1) <- paste0("hill1_", colnames(FTD_hill1))
#colnames(FTD_hill2) <- paste0("hill2_", colnames(FTD_hill2))

#diversity <- cbind(diversity, FTD_hill0, FTD_hill1, FTD_hill2)

#' -----------------------------------------------------------------------------
#' Get the species variability

#' Taxonomic data cleaning
species <- fread(paste0(root_path, "/traits.csv"))
species_names <- species$species
species <- as.matrix(species[, c(1:5)])
rownames(species) <- species_names
taxonomic <- taxa2dist(species, varstep=TRUE)
taxonomic <- hclust(taxonomic)
plot(taxonomic, hang = -1)

#' Functional data cleaning
functional <- hclust(trait_distance)
plot(functional, hang = -1)

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

# Species variability
TD_PSV <- psv(taxonomic_matched$comm, 
              taxonomic_matched$phy)$PSVs

PD_PSV <- psv(phylogenetic_matched$comm, 
              phylogenetic_matched$phy)$PSVs

FD_PSV <- psv(functional_matched$comm, 
              functional_matched$phy)$PSVs

diversity <- cbind(diversity, TD_PSV, PD_PSV, FD_PSV)

# Export
fwrite(diversity, paste0(root_path, "/diversity.csv"))



################################################################################
#' @title Cleaning of field inventories and estimation of diversity metrics
################################################################################

#' @description Data cleaning for the plot biomass files and estimate diversity 
#' and yield features
#' 
#' @return A .csv file

#' --------------------------------------------------------------------------
#' Libraries

library(data.table)

#' -----------------------------------------------------------------------------
#' Set working directory 
root_path <- "/media/antonio/Extreme_Pro/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Processing

### Read inventory file
data <- fread(paste0(root_path, "/fab2_allometry2.csv"))

### Select columns of interest
# Select small plots
data <- subset(data, Area_m2 == 100)

# Short columns
colOI <- c("ID_row_file", "block", "plot", "treatment", "year_planted", "species_richness", 
           "individual_id", "species", "species_code", "row", "column", "position", 
           "survey", "measurement_date", "deadmissing", "Biomass.conoid_conoidoid_infill")
data <- data[, ..colOI]
colnames(data)[16] <- "biomass"

# Transform to date and get year
data[, measurement_date := paste0(as.numeric(strsplit(measurement_date, "/")[[1]][3]), "-",
                                  sprintf("%02d", as.numeric(strsplit(measurement_date, "/")[[1]][1])), "-",
                                  sprintf("%02d", as.numeric(strsplit(measurement_date, "/")[[1]][2]))),
     by = seq_along(1:nrow(data))]
data$measurement_year <- year(data$measurement_date)

# Order by individual
data <- data[order(individual_id)]

# Select plots where the majority of individuals were planted in 2016
summary_data <- data[, .(mean_year = mean(year_planted),
                         sd_year = sd(year_planted),
                         cv_year = sd(year_planted)/mean(year_planted)), 
                     by = "plot"]


id <- unique(data$individual_id)
test <- data[individual_id == id[10027]]
plot(test$biomass ~ test$measurement_year)







#Set date and subset by year
data[, measurement_year := paste0(strsplit(measurement_date, "/")[[1]][3]),
     by = seq_along(1:nrow(data))]

data[, measurement_date := paste0(strsplit(measurement_date, "/")[[1]][3], "-",
                                  strsplit(measurement_date, "/")[[1]][1], "-",
                                  strsplit(measurement_date, "/")[[1]][2]),
     by = seq_along(1:nrow(data))]

data <- subset(data, measurement_year == 2022)

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
# Summary of metrics by individual.

plot_summary <- data[deadmissing == "No", 
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

species_summary <- data[deadmissing == "No", .(ntrees = .N,
                                               total_biomass = sum(Biomass.conoid_conoidoid_infill, na.rm = TRUE)),
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

fwrite(diversity, paste0(root_path, "/diversity_hill.csv"))


#-------------------------------------------------------------------------------
# Get proportions

species <- fread(paste0(root_path, "/traits.csv"))

proportions <- merge(species_summary, species[, c("Species", "Gymnosperm")], 
                     by.x = "species", by.y = "Species", all.x = TRUE, all.y = TRUE)

# Get unique plot
unique_plot <- unique(proportions$plot)

# Loop to get proportions
results <- data.table()

for(i in 1:length(unique_plot)) {
  
  plot <- subset(proportions, plot == unique_plot[i])
  
  A <- as.numeric(sum(plot[Gymnosperm == "N", "total_biomass"]))
  G <- as.numeric(sum(plot[Gymnosperm == "Y", "total_biomass"]))
  
  if(G == 0) {
    proportion_A <- 1
    proportion_G <- 0
  } else if(A == 0) {
    proportion_A <- 0
    proportion_G <- 1
  } else {
    Total <- A + G
    proportion_G <- round(G/Total, 2)
  }
  
  to_results <- data.table(plot = unique_plot[i],
                           PG = proportion_G)
  
  results <- rbind(results, to_results)
  
}

complete <- merge(complete, results, by = "plot", all.x = TRUE, all.y = TRUE)

#' -----------------------------------------------------------------------------
#' Taxonomic data cleaning
species <- fread(paste0(root_path, "/traits.csv"))
species_names <- species$species
species <- as.matrix(species[, c(1:4, 6)])
rownames(species) <- species_names
taxonomic <- taxa2dist(species, varstep=TRUE)
plot(hclust(taxonomic), hang = -1)

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
plot(phylo)

#' -----------------------------------------------------------------------------
#' Functional data cleaning

# Read and select species
species <- fread(paste0(root_path, "/traits.csv"))
species_names <- species$species
traits <- as.matrix(species[, c(12, 14, 16)])
rownames(traits) <- species_names

#' -----------------------------------------------------------------------------
#' Biomass data 

community <- species_summary[, c("plot", "total_biomass", "species")]
community$species <- chartr(" ", "_", community$species)
community <- sample2matrix(community)

colnames(community)[12] <- "Tilia_americana"
colnames(community)[4] <- "Juniperus_virginiana"
master_matrix <- decostand(community, method = "hellinger") 
AB <- rowSums(community)/10000

#' -----------------------------------------------------------------------------
#' Diversity metrics

# Taxonomic diversity
colnames(master_matrix) == colnames(as.matrix(taxonomic))
taxonomic <- as.matrix(taxonomic)
taxonomic <- taxonomic[c(1, 2, 3, 4, 9, 10, 11, 5, 6, 7, 8, 12), c(1, 2, 3, 4, 9, 10, 11, 5, 6, 7, 8, 12)]
colnames(master_matrix) == colnames(taxonomic)

TD_MPD <- Fmpd.a(taxonomic, master_matrix)

# Phylogenetic diversity
matched <- match.phylo.comm(phy = phylo, 
                            comm = master_matrix)

PD_MPD <- Fmpd.a(as.matrix(cophenetic(matched$phy)), matched$comm)

# Functional diversity
colnames(master_matrix) == rownames(traits)
traits <- traits[c(1, 2, 3, 4, 9, 10, 11, 5, 6, 7, 8, 12),]
colnames(master_matrix) == rownames(traits)

FD_MPD <- Fmpd.a(as.matrix(dist(traits)), master_matrix)

diversity <- data.table(plot = names(TD_MPD),
                        TD = TD_MPD,
                        PD = PD_MPD,
                        FD = FD_MPD,
                        AB = AB)

complete <- merge(diversity, complete, by = "plot", all.x = TRUE, all.y = TRUE)
fwrite(complete, paste0(root_path, "/diversity.csv"))

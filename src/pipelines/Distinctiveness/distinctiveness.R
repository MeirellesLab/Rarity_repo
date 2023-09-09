#######################################################################################
# This script calculates Distinctiveness for each genus in each sample in the database,#
# then calculates the mean and standard error of Distinctiveness for each sample,      #
# habitat, ecosystem and lifestyle.                                                             #
#                                                                                      #
########################################################################################

# Load packages
library(tidyverse)
library(funrar)
library(Matrix)
library(vegan)
library(FD)

source("src/data_wrangling/merge_annotation_metadata/merge_annotation_metadata.R")

# Load data
taxon <- read_csv("input/kraken_biomedb_relative_genera.csv")

traits <- read_csv("input/traits_by_genus.csv")

metadata <- read_csv("input/biome_classification.csv")

#There is no diference in the genus
#filtering samples that are not in metadata
taxon <- taxon %>% filter(taxon$samples %in% metadata$samples)

#The input must be numeric
samples <- taxon$samples
genus_in_traits <- traits$genus

taxon <- taxon %>%
  select(-samples)

rownames(taxon) <- samples

taxon <- as.matrix(taxon)

traits <- traits %>%
  select(-genus)

rownames(traits) <- genus_in_traits

traits <- as.matrix(traits)

#calculating richness per sample
richness <- rowSums(taxon > 0)

#Generating distance matrix with traits
dist_traits <-compute_dist_matrix(traits, metric = "euclidean")

#scaling the distances between 0 and 1
min_dist <- min(dist_traits)
max_dist <- max(dist_traits)

dist_traits <- (dist_traits - min_dist)/(max_dist - min_dist)


# Calculate distinctiveness  
di_df <- distinctiveness(taxon, dist_traits)

di_df_stack <- matrix_to_stack(di_df,
                                value_col = "di",
                                row_to_col = "samples",
                                col_to_col = "genus")   

di_sample_means <- di_df_stack %>%
  group_by(samples) %>%
  summarise(mean_di = mean(di, na.rm = TRUE))

#including FunRao, Sinpson and FunRedundancy from syncsa
#loading syncsa result
syncsa <- read_csv("input/syncsa_result.csv")

#merging syncsa result with di_sample_means
di_sample_means <- merge(
  x = di_sample_means,
  y = syncsa,
  by = "samples",
  all.x = TRUE
)

di_sample_means$...1 <- NULL

#adding metadata to di_sample_means

di_sample_means <- merge_annotation_metadata(
                  annotation_df = di_sample_means,
                  metadata_df = metadata,
                  metadata_variables = c("samples", "life_style", "ecosystem", "habitat")
)


di_sample_means <- cbind(di_sample_means, richness = richness)

#write the di_sample_means table
write_csv(di_sample_means, "input/distinctiveness_means_persample_08_09_2023.csv")

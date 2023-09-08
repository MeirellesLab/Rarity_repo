########################################################################################
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

#There is no diference in the genus
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
 
#Generating distance matrix with traits
dist_traits <-compute_dist_matrix(traits, metric = "euclidean")

#calculating functional diversity per samples
fd <- dbFD(traits, taxon)


# Calculate distinctiveness  
di_df <- distinctiveness(taxon, traits)

di_df_stack <- matrix_to_stack(di_df,
                                value_col = "di",
                                row_to_col = "samples",
                                col_to_col = "genus")   

##Script for calculating diversity indices##

#Load Libraries
library(tidyverse)

# 1. Data Wrangling
source("Source/data_wrangling/merge_annotation_metadata.R")

sp_data = merge_annotation_metadata(
    annotation_df = read.csv("Input/metagenomes_species.csv"),
    metadata_df =  read.csv("Input/metadata.csv"),
    metadata_variables = c("samples", "life_style", "environment", "habitat")
)   

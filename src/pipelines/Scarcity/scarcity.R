########################################################################################
# This script calculates scarcity for each genus in each sample in the database, then  #
# calculates the mean and standard error of scarcity for each sample, habitat,         #
# ecosystem and lifestyle.                                                             #
#                                                                                      #
########################################################################################
source("src/data_wrangling/merge_annotation_metadata/merge_annotation_metadata.R")

# Load packages
library(tidyverse)
library(funrar)
library(Matrix)
library(vegan)

# Load data
df <- read_csv("input/kraken_biomedb_relative_genera.csv")

# Temporary: remove samples not present in atualized metadata
meta <- read_csv("input/biome_classification.csv")
str(meta)
df <- df %>% filter(df$samples %in% meta$samples)
nrow(df)

#merging metadata and taxonomy df
df <- merge_annotation_metadata(
  annotation_df = df,
  metadata_df = meta,
  metadata_variables = c("samples", "life_style", "ecosystem", "habitat")
)

# Create the df with the samples names.
df_char <- df %>%
  select(
    samples, 
    life_style, 
    ecosystem,  
    habitat
  )

## The way that funrar works is that it is based on matrixes. These matrixes
## only accept one character variable. So we need to choose the site level to be
## used. Here, I will use at sample level, so no need for prior summarizations. 

# Create the df with the numeric variables.
df_clean <- df %>%
  select( 
    -habitat,
    -life_style,
    -ecosystem,
  )

# Check the data structure of the character variables in df_clean
df_clean %>%
  select_if(is.character) %>% 
  str()

# Let's separate the numeric variables from the character variables in order to
# construct the matrix.
df_clean_numeric <- df_clean %>%
  select_if(is.numeric)

df_clean_character <- df_clean %>%
  select_if(is.character)

# Lets create our matrix using numeric variables
df_matrix <- as.matrix(df_clean_numeric)

is.numeric(df_matrix)
# Now we are going to set the rownames of our matrix based on the character
# variables.
rownames(df_matrix) <- paste0(df_clean_character$samples)

## Our matrix was created. Let's clear the global environment to free up memory.

df_filling <- 1 - sum(df_matrix == 0) / (ncol(df_matrix) * nrow(df_matrix))

df_filling

rm(list = setdiff(
  ls(),
  c("df_matrix",   
    "df_char")
))

sum(is.na(df_matrix))

sum(df_matrix == 0)
#sum of non zero values for each row (richness per sample):
rich_persample <- rowSums(df_matrix != 0)

#diversity per sample
div_persample <- diversity(df_matrix, index = "simpson")


####### Run Scarcity function #############

si_df <- scarcity(df_matrix)

#check if si_df have NA values
sum(is.na(si_df))


si_means <- rowMeans(si_df, na.rm = TRUE) #taking mean si for each sample

#convert to a dataframe creating a column named samples with the row names
si_df <- as.data.frame(si_df)


#creating a samples column with the row names
si_df$samples <- rownames(si_df)  
#putting the samples column in the first 
si_df <- si_df[, c(ncol(si_df), 1:(ncol(si_df)-1))]

#Put the means in the dataframe
si_df <- cbind(si_df[, 1, drop = F], mean_si = si_means, si_df[, -1])

#Put the richness in the dataframe
si_df <- cbind(si_df[, 1:2, drop = F], richness = rich_persample, si_df[, -c(1:3)])

#Put the diversity in the dataframe
si_df <- cbind(si_df[, 1:3, drop = F], diversity = div_persample, si_df[, -c(1:4)])

View(head(si_df))

sum(si_df == "Invalid Number")

#order the dataframe by the mean si
si_df_ord <- si_df[order(si_df$mean_si), ]

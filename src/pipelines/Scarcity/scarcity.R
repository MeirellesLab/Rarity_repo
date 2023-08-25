########################################################################################
# This script calculates scarcity for each genus in each sample in the database, then  #
# calculates the mean and standard error of scarcity for each sample, habitat,         #
# ecosystem and lifestyle.                                                             #
#                                                                                      #
########################################################################################


# Load packages
library(tidyverse)
library(funrar)
library(Matrix)

# Load data
df <- read_csv("input/tabela_taxonomia_funrar_final_relative_2022_11_14.csv")

# Temporary: remove samples not present in atualized metadata
meta <- read_csv("input/biome_classification.csv")
str(meta)
df <- df %>% filter(samples %in% meta$samples)
nrow(df)

# Rename columns
df <- df %>%
  rename(
    life_style = level_1,
    ecosystem = level_2,
    habitat = level_3
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

# Now we are going to set the rownames of our matrix based on the character
# variables.

rownames(df_matrix) <- paste0(df_clean_character$samples)

## Our matrix was created. Let's clear the global environment to free up memory.

rm(list = setdiff(
  ls(),
  c("df_matrix",   
    "df_char")
))

####### Run Scarcity function #############

si_df <- scarcity(df_matrix)

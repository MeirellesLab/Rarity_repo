########################################################################################
# This script calculates Distinctiveness for each genus in each sample in the database,#
# then calculates the mean and standard error of Distinctiveness for each sample,      #
# habitat, ecosystem and lifestyle.                                                             #
#                                                                                      #
########################################################################################

#load taxonomic data frame
source("src/data_wrangling/merge_annotation_metadata/merge_annotation_metadata.R")

# Load data
df <- read_csv("input/kraken_biomedb_relative_genera.csv")

#remove samples not present in atualized metadata
meta <- read_csv("input/biome_classification.csv")
str(meta)
df <- df %>% filter(df$samples %in% meta$samples)
nrow(df)

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
is.numeric(df_matrix)
sum(is.na(df_matrix))
sum(df_matrix == 0)
dim(df_matrix)

#load traits table
dftraits <- read_csv("input/traits_by_genus_random.csv")
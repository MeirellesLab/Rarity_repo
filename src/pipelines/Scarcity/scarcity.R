########################################################################################
# This script calculates scarcity for each genus in each sample in the database, then  #
# calculates the mean and standard error of scarcity for each sample, habitat,         #
# ecosystem and lifestyle.                                                             #
#                                                                                      #
########################################################################################

source("src/data_wrangling/merge_annotation_metadata/merge_annotation_metadata.R")

# Load data
df <- read_csv("input/kraken_biomedb_relative_genera.csv")

# Temporary: remove samples not present in atualized metadata
meta <- read_csv("input/final_metadata.csv")

df <- df %>% filter(df$samples %in% meta$samples)


#merging metadata and taxonomy df
df <- merge_annotation_metadata(
  annotation_df = df,
  metadata_df = meta,
  metadata_variables = c("samples", "life_style", "ecosystem", "habitat", "antropization")
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
is.numeric(df_matrix)
sum(is.na(df_matrix))
sum(df_matrix == 0)
dim(df_matrix)


## Our matrix was created. Let's clear the global environment to free up memory.
rm(list = setdiff(
  ls(),
  c("df_matrix",   
    "df_char")
))

is.numeric(df_matrix)
#sum of non zero values for each row (richness per sample):
rich_persample <- rowSums(df_matrix != 0)


####### Run Scarcity function #############

#transform the matrix in stack
df_stack <- matrix_to_stack(df_matrix, 
                        value_col = "value", 
                        row_to_col = "samples", 
                        col_to_col = "genus")

si_stack <- scarcity_stack(df_stack,
                        sp_col = "genus",
                        com = "samples",
                        abund = "value")
View(head(si_stack))
#check if si_df have NA values
sum(is.na(si_stack))

si_sample_means <- si_stack %>%
  group_by(samples) %>%
  summarise(mean_si = mean(Si, na.rm = TRUE))

nrow(si_sample_means)

#including FunRao, Sinpson and FunRedundancy from syncsa
#load syncsa
syncsa <- read_csv("input/syncsa_result.csv")

#merging syncsa result with si_sample_means
si_sample_means <- merge(
  x = si_sample_means,
  y = syncsa,
  by = "samples",
  all.x = TRUE
)

#puting the richness in the si_sample_means df
si_sample_means <- cbind(si_sample_means, richness = rich_persample)

#removing rownames
rownames(si_sample_means) <- NULL

#removing column ...1
si_sample_means$...1 <- NULL

#adding habitat, ecosystem and life_style to the si_sample_means df
si_sample_means <- merge(
  x = si_sample_means,
  y = df_char,
  by = "samples",
  all.x = TRUE
)

#reorder columns to be samples, then life_style, then ecosystem, then habitat, then mean_si, then richness, then diversity
si_sample_means <- si_sample_means[, c(1, 7, 8, 9, 2, 3, 4, 5, 6)]

#saving the df
write_csv(si_sample_means, "input/scarcity_means_persample_test.csv")

#saving the stack df
write_csv(si_stack, "input/scarcity_stack_df_test.csv")

rm(list = setdiff(
  ls(),
  c("si_sample_means")
))

########################################################################################
#Here is the part of the script for calculating scarcity withot using the stack matrix##
#but it was returning some columns as "Invalid Number", and I don't know how to fix it##
########################################################################################

#si_df <- scarcity(df_matrix)

#si_means <- rowMeans(si_df, na.rm = TRUE) #taking mean si for each sample

#convert to a dataframe creating a column named samples with the row names
#si_df <- as.data.frame(si_df)


#creating a samples column with the row names
#si_df$samples <- rownames(si_df)  
#putting the samples column in the first 
#si_df <- si_df[, c(ncol(si_df), 1:(ncol(si_df)-1))]

#Put the means in the dataframe
#si_df <- cbind(si_df[, 1, drop = F], mean_si = si_means, si_df[, -1])

#Put the richness in the dataframe
#si_df <- cbind(si_df[, 1:2, drop = F], richness = rich_persample, si_df[, -c(1:3)])

#Put the diversity in the dataframe
#si_df <- cbind(si_df[, 1:3, drop = F], diversity = div_persample, si_df[, -c(1:4)])

#View(head(si_df))

#sum(si_df == "Invalid Number") #this line returns 0 for some reason, but there is cells with this value

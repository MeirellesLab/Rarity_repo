################## funrar Distinctiveness test ##########################################

#----- 1. Load librarys and clean environment -------------------------------#
library(tidyverse)
library(Matrix)
library(funrar)

rm(list = setdiff(
  ls(),          
  c()                 #limpa o environment
))

#---- 2. Load the data ------------------------------------------------------#

#load data frame
df <- read.csv("Inputs/tabela_taxonomia_funrar_final_relative_2022_11_14.csv")
dftraits <- read.csv("Inputs/metabolism_features_summarized_2022_09_08.csv")

dfnum <- df %>% select_if(is.numeric)
#check number of rows and columns
dim(df)
dim(dftraits)
#check data structure
str(df)
str(dftraits)
#check the character variables
df %>%
  select_if(is.character) %>%
  str()
  
dftraits %>%
  select_if(is.character) %>%
  str()  
  
#------- 3. Data Processing --------------------------------------------------#
## Summarize data frame by averaging all numeric variables. We group these
## variables by the character variables.
df <-  rename( df, replace = c(
  "level_1" = "life_form",
  "level_2" = "environment",
  "level_3" = "habitat")
  )

df_clean_mean <- df %>%
  group_by(
    life_form,
    environment,
    habitat
  ) %>%
  summarise_if(is.numeric, mean, na.rm = FALSE) %>%
  ungroup()


df_clean_numeric <- df_clean_mean %>%
  select_if(is.numeric)

# Create the df with the samples names.
df_char <- df_clean_mean %>%
  select(
    life_form,
    environment,
    habitat
  )

habitats <- df_clean_mean %>%
  select(habitat)

dfh <- cbind(habitats,df_clean_numeric)

nrow(dftraits)
length(dftraits[,1])
any(is.na(dftraits[,1]))
dftraits <- na.omit(dftraits)
nrow(dftraits)
any(is.na(dftraits[,1]))

nrow(dfh)
length(dfh[,1])
any(is.na(dfh[,1]))
dfh <- na.omit(dfh)
nrow(dfh)
any(is.na(dfh[,1]))
#------------ 6. Check if matrix is abu relative ------------------------------#
#funrar:::is_relative(df_matrix) # Check if the matrix is abu relative
#df_matrix = make_relative(df_matrix) # Make the matrix abu relative  
round(apply(dfnum,1,sum),7) #chacking if sum of rows is equal to 1 (is relative abundance)
#--------- 7. Compute distance matrix ------------------------------------------#

genus_traits <- dftraits$genus_new
genus_names <- colnames (dfh)
dfh_subset <- dfh[,(genus_names %in% genus_traits)]

rownames(dftraits) =  genus_traits
dftraits <- dftraits[,-1]

trait_distance <- compute_dist_matrix(dftraits, metric = "gower")

dfh <- cbind(habitats, dfh_subset)
  
dfh <- column_to_rownames(dfh, var = "habitat")
round(apply(dfh,1,sum),7) #chacking if sum of rows is equal to 1 (is relative abundance)

dfh <- as.matrix(dfh)

di_df <- distinctiveness(dfh, trait_distance, relative = TRUE)

di_df <- as.data.frame(di_df)

di_df <- rownames_to_column(di_df, var = "habitat")

di_df_f <- merge(df_char, di_df, by = "habitat")

d <- Sys.Date() %>% str_replace_all("-","_")

write.csv(di_df_f, 
          file = paste("Outputs/Tables/Distinctiveness_generaandCandidatus_x_habitats_",d,".csv"),
          row.names = FALSE)


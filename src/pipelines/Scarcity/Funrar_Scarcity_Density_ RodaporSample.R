################################## ReadMe ######################################

# This is a test file for the rarity function. We are going to use abundance

# data from the microbiota community. Note that the rarity function is run

# on a matrix and not a data frame. We provide a step by step way to create

# the matrix and run the rarity function.


################################## Pipeline ####################################

## 1. Load the library

## 2. Load the data and check structure

## 3. Data preprocessing

## 4. Create the matrix

## 5. Check the matrix and increase benchmark

## 6. Run the rarity function

## 7. Plot the results

## 8. Save the results and export to excel

############################# 1. Load the library ##############################

# Library
library(Matrix)
library(tidyverse)
library(funrar)
############################# 2. Load the data #################################

#Load data frame
df <- read_csv("input/tabela_taxonomia_funrar_final_relative_2022_11_14.csv")
#Check number of rows and columns
dim(df)
#check data structure
str(df)
#Check the character variables

df %>% 
  select_if(is.character) %>% 
  str()

############################# 3. Data preprocessing ############################

## Summarize data frame by averaging all numeric variables. We group these

## variables by the character variables. The idea is to have our samples per

## habitat.

# Change character variables names. Save the character variable names for

# match the samples names later. This df will be used to merge and create the

# scarcity plot.

df <- df %>%
  rename(
    life_form = level_1, 
    environment = level_2,
    habitat = level_3
  )



# Create the df with the samples names.
df_char <- df %>%
  select(
    samples, 
    life_form, 
    environment,  
    habitat
    
  )



## The way that funrar works is that it is based on matrixes. These matrixes
## only accept one character variable. So we need to choose the site level to be
## used. Here, I will use at sample level. 

df_clean <- df %>%
  select( 
    -habitat,
    -life_form,
    -environment,
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

############################# 4. Create the matrix #############################

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


############################# 5. Check the matrix ##############################

## As stated in funrar documentation, if our matix is filled with 0 values, it

## will be quicker to use sparse matrixes instead. The documentation says that

## we should compute the percentage of non-zero cells in our matrix in order

## to decide if we should use sparse matrixes or not.

df_filling <- 1 - sum(df_matrix == 0) / (ncol(df_matrix) * nrow(df_matrix))

df_filling # Our results shows that we have  46% of the cells filled with




# Create the sparse matrix

sparse_mat <- as(df_matrix, "sparseMatrix")


# Now let's check the first matrix size

is(df_matrix, "sparseMatrix") # Check if the matrix is a sparse matrix

print(object.size(df_matrix), units = "Kb") # Normal matrix size is about 105 Mb

is(sparse_mat, "sparseMatrix") # Check if the matrix is a sparse matrix

print(object.size(sparse_mat), units = "Kb") # Sparse matrix 102 Mb



############################# 6. Run the rarity function #######################

# First we need to calculate the distance matrix.

df_stack <- matrix_to_stack(
  df_matrix, 
  "value", 
  "samples",
  "species"
)

# check the data structure of the stack
str(df_stack)

# Compute scarcity for each species on each habitat

si_df <- scarcity_stack(  
  df_stack,
  "species",
  "samples",
  "value"  
)

head(si_df)

# Check NA values in si_df

si_df %>%
  
  filter(is.na(value))

si_df <- si_df[,-3]





# Density of scarcity values total graphic

quant <- quantile(si_df$Si, probs = seq(0, 1))

labels_quant <- paste(names(quant)[-length(quant)], names(quant)[-1], sep = "-")

si_density <- data.frame(density(si_df$Si)[c("x", "y")])

si_density <- subset(si_density, x >= quant[[1]] & x <= quant[[length(quant)]])

si_density$quant <- cut(si_density$x, breaks = quant)




si_dens <- ggplot(data = si_density, aes(x = x, y = y)) +
  
  geom_area(aes(fill = quant)) +
  
  scale_fill_brewer(palette = "RdYlBu", labels = labels_quant,
                    
                    name = "Quantile") +
  
  geom_line(size = 1) +
  
  xlab("Scarcity values") +
  
  ylab("Frequency") +
  
  ggtitle("Density of scarcity values") +
  
  theme_classic()



si_dens





################################################################################

######################### 7. Ploting the scarcity index ########################


# First we need to create variables in the si_df. Those variables should

# have the samples names matching life_form, environment and habitat that are

# found in df_char.

si_df <- merge(si_df, df_char, by = "samples")


# Check df_final dimementions and structure

head(si_df, n = 5)



# Clear environment and free up memory

rm(list = setdiff(
  
  ls(),
  
  c(
    
    "si_df"
    
  )
  
))


# Now let's plot a barplot with mean and std of si index per habitat facet by

# environment with


# First calculate the mean and std of si index per habitat facet by environment

si_df_mean <- si_df %>%
  
  group_by(
    
    life_form,
    
    environment,
    
    habitat
    
  ) %>%
  
  summarise(
    
    mean_si = mean(Si),
    
    std_si = sd(Si),
    
    se_si = std_si / sqrt(n())
    
  ) %>%
  
  ungroup()



head(si_df_mean, n = 5)


# Plot a barplot with mean and se of si index per habitat facet by environment

# This plot shows the average scartcity index for each habitat for each

# environment.


si_barplot <- si_df %>%
  
  group_by(
    
    life_form,
    
    environment,
    
    habitat
    
  ) %>%
  
  summarise(
    
    mean_si = mean(Si),
    
    std_si = sd(Si),
    
    se_si = std_si / sqrt(n())
    
  ) %>%
  
  ungroup() %>%
  
  ggplot(
    
    aes(
      
      x = habitat,
      
      y = mean_si,
      
      fill = "grey"
      
    )
    
  ) +
  
  facet_wrap(
    
    ~environment,
    
    nrow = 2,
    
    scales = "free"
    
  ) +
  
  labs(
    
    x = "Environment",
    
    y = "Mean of SI index",
    
    title = "Mean of SI index per habitat facet by environment"
    
  ) +
  
  theme_classic() +
  
  theme(
    
    axis.text.x = element_text(
      
      angle = 90,
      
      vjust = 0.5,
      
      hjust = 1
      
    ),
    
    legend.position = "none",
    
    strip.background = element_blank()
    
  ) +
  
  geom_bar(stat = "identity") +
  
  geom_errorbar(
    
    aes(
      
      ymin = mean_si,
      
      ymax = mean_si + se_si,
      
    ),
    
    width = 0.5,
    
    color = "black"
    
  )

si_plot


# Simple scatter plot of si index vs abundance

si_scatterplot <- si_df %>%
  
  group_by(
    
    life_form,
    
    environment,
    
    habitat,
    
    species
    
  ) %>%
  
  summarise(
    
    mean_si = mean(Si),
    
    mean_abundance = mean(value)
    
  ) %>%
  
  ggplot(
    
    aes(
      
      x = mean_abundance,
      
      y = mean_si,
      
      fill = "grey"
      
    )
    
  ) +
  
  geom_point() +
  
  labs(
    
    x = "Mean Abundance",
    
    y = "Mean of SI index",
    
    title = "Mean of SI index vs abundance"
    
  ) +
  
  facet_wrap(
    
    ~environment,
    
    nrow = 2
    
  ) +
  
  theme_classic() +
  
  theme(
    
    legend.position = "none",
    
    strip.background = element_blank()
    
  )

si_scatterplot



# Clear graphics and free up memory

graphics.off()

############################# 8. Import spread table ###########################

head(si_df, n = 5)



# Spread table by Si values

si_spread <- si_df %>%
  
  select(-value) %>%
  
  spread(
    
    species,
    
    Si
    
  )


# Write csv file with the spread table

write.csv(si_spread, "scarcity_index-taxon.csv")


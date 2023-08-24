############################# 1. Load the library ##############################
# Library
library(Matrix)
library(tidyverse)
library(funrar)

############################# 2. Load the data #################################
#Load data frame
df <- read_csv("genero_taxonomia_samples_resistoma.csv")

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


## The way that funrar works is that it is based on matrixes. These matrixes
## only accept one character variable. So we need to choose the site level to be
## used. Here, I will use at habitat level. Later we will need to build a new
## data frame with the other levels (life_form and environment)

# Check the data structure of the character variables in df_clean
df %>%
  select_if(is.character) %>%
  str()

# Let's separate the numeric variables from the character variables in order to
# construct the matrix.
df_clean_mean <- df %>%
  group_by(
    life_form,
    environment,
    habitat
  ) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
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

df_clean_samples <- df_clean_mean %>%
  select(environment)



############################# 4. Create the matrix #############################
# Lets create our matrix using numeric variables
df_matrix <- as.matrix(df_clean_numeric)

# Now we are going to set the rownames of our matrix based on the character
# variables.
rownames(df_matrix) <- paste0(df_clean_samples$environment)

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

## The filling percentage is 0.1%
df_filling # Our results shows that we have  46% of the cells filled with
# 0 values. Need to use sparse matrixes.

# Create the sparse matrix
sparse_mat <- as(df_matrix, "sparseMatrix")

# Now let's check the first matrix size
is(df_matrix, "sparseMatrix") # Check if the matrix is a sparse matrix
print(object.size(df_matrix), units = "Kb") # Normal matrix size is about 105 Mb
is(sparse_mat, "sparseMatrix") # Check if the matrix is a sparse matrix
print(object.size(sparse_mat), units = "Kb") # Sparse matrix 102 Mb

######################## 6. Chack if matrix is abu relative ####################
funrar:::is_relative(df_matrix) # Check if the matrix is abu relative
df_matrix = make_relative(df_matrix) # Make the matrix abu relative

# First we need to calculate the distance matrix.

df_stack <- matrix_to_stack(
  
  df_matrix,
  
  "value",
  
  "environment",
  
  "species"
  
)





# check the data structure of the stack

str(df_stack)



# Compute scarcity for each species on each habitat

si_df <- scarcity_stack(
  
  df_stack,
  
  "species",
  
  "environment",
  
  "value"
  
)



head(si_df)



# Check NA values in si_df

si_df %>%
  
  filter(value == is.na)


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

si_df <- merge(si_df, df_char, by = "environment")


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
      
      x = environment,
      
      y = mean_si,
      
      fill = "grey"
      
    )
    
  ) +
  
  facet_wrap(
    
    ~life_form,
    
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

si_barplot


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
    
    ~life_form,
    
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

write.csv(si_spread, "scarcity_index-taxon_perEnvironment.csv")


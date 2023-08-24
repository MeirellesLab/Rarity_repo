#### README----
# Here lies the code that provides insights for the major metabolisms in each
#environment

#### Packages required ----
library(tidyverse)
library(janitor)

setwd("~/Felipe/keystones-paper/Functional_analysis/")

d <- Sys.Date() %>% str_replace_all("-", "_")

#Read your metabolism features table and select the columns you wanna preserve
metabolic_features <- read_csv("inputs/metabolism_features_FAB_ATS_PMM_update_27_02_2021.csv") %>% 
  select(phylum, genus_new, x1_1_1_trichloro_2_2_bis_4_chlorophenyl_ethane_ddt_degradation:zeatin_biosynthesis)


# Here, we summarise each metabolism by the mode: @amanda's idea.

## Create a function to make the mode of our data
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){ # A quick check 
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

##Calculate mode
###Provide the variable that you want to summarise your data (group_by)
###Then, provide the columns with numbers that you want to summarise_at

mode_metabolic_features <- metabolic_features %>% group_by(genus_new) %>% 
  summarise_at(vars(x1_1_1_trichloro_2_2_bis_4_chlorophenyl_ethane_ddt_degradation:zeatin_biosynthesis),
               .funs = Mode)

#Save the summarized table
write.csv(mode_metabolic_features, 
          file = paste("metabolism_features_summarized_",d,".csv", sep = ""), 
          row.names = FALSE)

#Now we want to select only the genus that we annotated taxonomicaly

##Read your input table
tabela_anot_tax <- read.csv("~/Felipe/keystones-paper/taxonomic tables all samples/genus.general.relative.matrix_2022_03_31.csv")

#Create vectors with the genus your have in both tables
genus_genomic_traits <- mode_metabolic_features$genus_new
genus_names <- colnames(tabela_anot_tax)

#Now, you create one vector only with the genus you have annotated and that you
#have metabolic traits
tabela_genus_select <- tabela_anot_tax[,(genus_names %in% genus_genomic_traits)]

#Then, you select these genus in the metabolic traits table
mode_metabolic_features_genus <- mode_metabolic_features %>% select(which(mode_metabolic_features$genus_new %in% colnames(tabela_genus_select)))

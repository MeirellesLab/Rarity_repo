#### README----
# Here lies the code that provides insights for the major metabolisms in each
#environment

#### Packages required ----
library(tidyverse)
library(janitor)
library(funrar)

#setwd("~/Felipe/keystones-paper/Functional_analysis/")

#------------------------- Summarising by genus: -----------------------------#

d <- Sys.Date() %>% str_replace_all("-", "_")

#Read your metabolism features table and select the columns you wanna preserve
metabolic_features <- read_csv("metabolism_features_FAB_ATS_PMM_update_27_02_2021.csv") %>% 
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


rm(list = setdiff(
  ls(),          
  c("d")                 #limpa o environment
))


#--------Create taxon table with the genus in features table-------------------#

dftaxo <- read.csv("Inputs/genus.general.absolute.matrix_2019-06-11.csv")
dftraits <- read.csv("Inputs/metabolism_features_summarized_2022_09_08.csv")

#Nessa parte tento fazer com que as colunas
#na tabela de anotação taxonomica, que são os gêneros
#sejam as mesmas da coluna genus_new da tabela
#com os metabolismos.

genus_genomic_traits <- dftraits$genus_new  #Cria uma string so com os gêneros da dftraits
genus_names <- colnames(dftaxo)             #Cria um df só com os generos do dftaxo
tabela_genus_select <- dftaxo[,(genus_names %in% genus_genomic_traits)] #Cria um df com 
                                                                    #apenas os gêneros  
                                                                    #que tem no dftraits 
                                                                    #e suas abundâncias
                                                                    #mas sem as samples 
                                                                    #como categorias
                                                                    

dftaxo_char <- dftaxo %>%                 #Cria um df com apenas as 3 primeiras colunas
  select_if(is.character)   #do df taxo que serão as categorias 

rm(list = setdiff(
  ls(),                             #Clear the environment ro free up memory
  c("dftaxo_char",
    "tabela_genus_select",
    "d")
))

vec_samples <- dftaxo_char$samples   #Creates a vector with the samples
tabela_genus_select$samples <- vec_samples  #Puts the samples vector in the genus table


dftaxo_final <- inner_join(dftaxo_char, tabela_genus_select, by = "samples") #Crates the 
#final taxon
#df by joining
#the tables by
# 'samples' column

#--Create csv file with the final taxon table
write.csv(dftaxo_final, 
          file = paste("Inputs/tabela_taxonomia_generos_funrar_absolute_",d,".csv", sep = ""),
          row.names = FALSE)

#make relative abundance 
dfnum <- dftaxo_final %>% select_if(is.numeric)
dfmatrix <- as.matrix(dfnum)
dfrelative <- make_relative(dfmatrix)
round(apply(dfrelative,1,sum),6) #check if row sum is 1, meaning is relative abundance
dfrelative_final <- cbind(dftaxo_char, dfrelative)

#--Create csv file with the final taxon table
write.csv(dfrelative_final, 
          file = paste("Inputs/tabela_taxonomia_generos_funrar_relative_",d,".csv", sep = ""),
          row.names = FALSE)

#------------------------------------------------------------------------------#          

rm(list = setdiff(
  ls(),          
  c("d")                 #limpa o environment
))

#------------------------------- Summarising by phylum: ----------------------#

#Read your metabolism features table and select the columns you wanna preserve
metabolic_features <- read_csv("metabolism_features_FAB_ATS_PMM_update_27_02_2021.csv") %>% 
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

mode_metabolic_features <- metabolic_features %>% group_by(phylum) %>% 
  summarise_at(vars(x1_1_1_trichloro_2_2_bis_4_chlorophenyl_ethane_ddt_degradation:zeatin_biosynthesis),
               .funs = Mode)

candidatus <- grep("Candidatus",  mode_metabolic_features$phylum)

candidatus_final <- mode_metabolic_features[candidatus, ]

#Save the summarized table
write.csv(candidatus_final, 
          file = paste("Candidatus_metabolism_features_summarized_by_phylum",d,".csv", sep = ""), 
          row.names = FALSE)

#------------------------------------------------------------------------------#          

rm(list = setdiff(
  ls(),          
  c()                 #limpa o environment
  ))

#---------------------- Juntar as duas tabelas de taxonomia ------------------#

dfgen <- read.csv("Inputs/tabela_taxonomia_generos_funrar_absolute_2022_11_14.csv")
fila <- read.csv("phyla.general.absolute.matrix_2019-06-11.csv")

dfgen <- dfgen[,-1]

candidatus <- fila %>% select_at(vars(starts_with("Candidatus")))

dftaxo_final <- cbind(dfgen, candidatus)

write.csv(dftaxo_final,
          file = paste("Inputs/tabela_taxonomia_funrar_final_absolute_",d,".csv"),
          row.names = FALSE)

dfnum <- dftaxo_final %>% select_if(is.numeric)
dfchar <- dftaxo_final %>% select_if(is.character)
dfmatrix <- as.matrix(dfnum)
dfrelative <- make_relative(dfmatrix)
round(apply(dfrelative,1,sum),6) #check if row sum is 1, meaning is relative abundance
dfrelative_final <- cbind(dfchar, dfrelative)

write.csv(dfrelative_final,
          file = paste("Inputs/tabela_taxonomia_funrar_final_relative_",d,".csv"),
          row.names = FALSE)

#------------------------------------------------------------------------------#          

rm(list = setdiff(
  ls(),          
  c("d")                 #limpa o environment
))

#---------------------- Juntar as duas tabelas de metabolismos ------------------#

dfgen <- read.csv("metabolism_features_summarized_2022_09_08.csv")
candidatus <- read.csv("Candidatus_metabolism_features_summarized_by_phylum2022_09_08.csv")

names(dfgen)[1] <- "taxon"
names(candidatus)[1] <- "taxon"

dfmetabol <- rbind(dfgen, candidatus)

d <- Sys.Date() %>% str_replace_all("-", "_")

write.csv(dfmetabol,
          file = paste("tabela_metabolismos_funrar_final_",d,".csv"),
          row.names = FALSE)

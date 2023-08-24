####### Script para fazer a tabela de anot. taxo. ficar com ################
####### as mesmas linhas da tabela de genomic traits #######################

#-----------------------Load librarys and df's---------------------------------#

library(tidyverse)

rm(list = setdiff(
  ls(),          
  c()                 #limpa o environment
))

dftaxo <- read.csv("genero_taxonomia_samples_resistoma.csv")
dftraits <- read.csv("metabolism_features_summarized_2022_08_24.csv")

#-------------Create taxon table with the genus in features table--------------#

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
#df1 <- dftraits$genus_new
#dftaxo %>% select(colnames(dftaxo) %in% df1) #modo que tentamos antes e não deu certo



dftaxo_char <- dftaxo %>%                 #Cria um df com apenas as 3 primeiras colunas
                select_if(is.character)   #do df taxo que serão as categorias 

rm(list = setdiff(
    ls(),                             #Clear the environment ro free up memory
    c("dftaxo_char",
      "tabela_genus_select")
  ))

vec_samples <- dftaxo_char$samples   #Creates a vector with the samples
tabela_genus_select$samples <- vec_samples  #Puts the samples vector in the genus table


dftaxo_final <- inner_join(dftaxo_char, tabela_genus_select, by = "samples") #Crates the 
                                                                             #final taxon
                                                                             #df by joining
                                                                             #the tables by
                                                                             # 'samples' column
#--------------Create csv file with the final taxon table----------------------#
write.csv(dftaxo_final, "tabela_taxonomia_generos_funrar.csv")
#------------------------------------------------------------------------------#          

rm(list = setdiff(
  ls(),          
     c()                 #limpa o environment
))



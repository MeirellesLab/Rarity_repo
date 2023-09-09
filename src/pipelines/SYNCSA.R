##script for running syncsa to obtainthe diversity, functional diversity and functional redundancy per sample. 

install.packages("SYNCSA")
library(SYNCSA)

#loading the data
taxon <- read_csv("input/kraken_biomedb_relative_genera.csv")

traits <- read_csv("input/traits_by_genus.csv")

#There is no diference in the genus

#The input must be numeric
samples <- taxon$samples
genus_in_traits <- traits$genus

taxon <- taxon %>%
  select(-samples)

rownames(taxon) <- samples

taxon <- as.matrix(taxon)

traits <- traits %>%
  select(-genus)

rownames(traits) <- genus_in_traits

traits <- as.matrix(traits)

#running SYNCSA

SYNCSA <- rao.diversity(
    comm = taxon,
    traits = traits,
    phylodist = NULL,
    checkdata = TRUE,
    ord = "metric",
    put.together = NULL,
    standardize = TRUE)
  
final_table <- cbind(samples, SYNCSA$FunRedundancy, SYNCSA$FunRao, SYNCSA$Simpson)
colnames(final_table) <- c("samples", "FunRedundancy", "FunRao", "Simpson")
rownames(final_table) <- NULL
write.csv(final_table, "input/syncsa_result.csv")

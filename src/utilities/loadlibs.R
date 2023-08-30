#Script for installing and loading necessary lybraries

packages <- c(
  "tidyverse",
  "funrar", 
  "ggplot2",
  "dplyr",
  "RColorBrewer",
  "ggpubr",
  "cowplot",
  "vegan",
  "devtools"
)


# Installing the necessary packages from CRAN
for (lib in seq_along(packages)) {
  if (packages[lib] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packages[lib], dependencies = TRUE)
  }
}

#install.packages("funrar")

lapply(packages, require, character.only = TRUE)

#Script for installing and loading necessary lybraries

packages <- c(
  "tidyverse",
  "funrar", 
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "Hmisc",
  "plyr",
  "RColorBrewer",
  "reshape2",
  "ggforce",
  "forcats",
  "extrafont",
  "ggpubr",
  "patchwork",
  "gridExtra",
  "cowplot",
  "PupillometryR",
  "ggthemes",
  "plotly",
  "stringr",
  "ggpmisc",
  "vegan",
  "factoextra",
  "roxygen2",
  "hillR",
  "devtools",
  "reticulate"
)


# Installing the necessary packages from CRAN
for (lib in seq_along(packages)) {
  if (packages[lib] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packages[lib], dependencies = TRUE)
  }
}

install.packages("funrar")

lapply(packages, require, character.only = TRUE)

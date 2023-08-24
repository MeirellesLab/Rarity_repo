#Script for installing and loading necessary lybraries

packages <- c(
    "tidyverse",
    "funrar", 
    "Matrix"
)


# Installing the necessary packages from CRAN
for (lib in seq_along(packages)) {
  if (packages[lib] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packages[lib], dependencies = TRUE)
  }
}

lapply(packages, require, character.only = TRUE)

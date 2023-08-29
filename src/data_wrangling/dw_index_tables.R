########################################################################################################
# This script summarize the si_means_persample table by life_style, ecosystem and habitat to be used  ##
# in the plots. The si_means_persample table was created in the script scarcity.R, where the si values##
# of all taxons were calculated for each sample. The si_means_persample table has the mean si values  ##
# of each sample, the richness and the diversity of each sample.                                      ##
########################################################################################################

# load si_means_persample table
si <- read_csv("input/scarcity_means_persample_29_08_2023.csv")

#life_style summarized si means
si_life_style <- si %>%
  group_by(life_style) %>%
  summarise(mean_si = mean(mean_si, na.rm = TRUE),
            mean_richness = mean(richness, na.rm = TRUE),
            mean_diversity = mean(diversity, na.rm = TRUE))


#ecosystem summarized si means
si_ecosystem <- si %>%
  group_by(ecosystem) %>%
  summarise(mean_si = mean(mean_si, na.rm = TRUE),
            mean_richness = mean(richness, na.rm = TRUE),
            mean_diversity = mean(diversity, na.rm = TRUE))


#habitat summarized si means
si_habitat <- si %>%
  group_by(habitat) %>%
  summarise(mean_si = mean(mean_si, na.rm = TRUE),
            mean_richness = mean(richness, na.rm = TRUE),
            mean_diversity = mean(diversity, na.rm = TRUE))


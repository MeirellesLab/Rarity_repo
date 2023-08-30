########################################################################################################
# This script summarize the si_means_persample table by life_style, ecosystem and habitat to be used  ##
# in the plots. The si_means_persample table was created in the script scarcity.R, where the si values##
# of all taxons were calculated for each sample. The si_means_persample table has the mean si values  ##
# of each sample, the richness and the diversity of each sample.                                      ##
########################################################################################################
set.seed(123)

# load si_means_persample table
si <- read_csv("input/scarcity_means_persample_29_08_2023.csv")

si$life_style <- as.factor(si$life_style)
## There is something very strange with this function. But it was the only way
## I found to reorder the DF based in the life style, ecosystem, then the mean,
## nested in this order.
si <- si %>%

  group_by(life_style) %>%
  mutate(life_style = fct_relevel(life_style, "host-associated", "free-living")) %>%
  ungroup() %>%

  group_by(ecosystem) %>%
  mutate(mean = mean(mean_si)) %>%
  ungroup() %>%
  mutate(ecosystem = fct_reorder(ecosystem, mean)) %>%

#   group_by(life_style) %>%
#   mutate(ecosystem = fct_reorder(ecosystem, mean)) %>%
#   select(-mean) %>%
#   ungroup() %>%

  group_by(ecosystem, habitat) %>%
  mutate(mean = mean(mean_si)) %>%
  ungroup() %>%

  group_by(ecosystem) %>%
  mutate(habitat = fct_reorder(habitat, mean)) %>%
  select(-mean)

si$life_style <- as.factor(si$life_style)
si$ecosystem <- as.factor(si$ecosystem)
si$habitat <- as.factor(si$habitat)

#life_style summarized si means
si_life_style <- si %>%
      group_by(life_style) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_si = median(mean_si),
        mean_mean_si = mean(mean_si),
        sd_mean_si = sd(mean_si),
        se_mean_si = sd_mean_si / sqrt(n)) %>%
      select(-n) 


#ecosystem summarized si means
si_ecosystem <- si  %>%
      group_by(life_style, ecosystem) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_si = median(mean_si),
        mean_mean_si = mean(mean_si),
        sd_mean_si = sd(mean_si),
        se_mean_si = sd_mean_si / sqrt(n)) %>%
      select(-n)


#habitat summarized si means
si_ecosystem <- si  %>%
      group_by(life_style, habitat, ecosystem) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_si = median(mean_si),
        mean_mean_si = mean(mean_si),
        sd_mean_si = sd(mean_si),
        se_mean_si = sd_mean_si / sqrt(n)) %>%
      select(-n)


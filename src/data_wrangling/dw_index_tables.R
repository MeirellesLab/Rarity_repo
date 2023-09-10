########################################################################################################
# This script summarize the si_means_persample table by life_style, ecosystem and habitat to be used  ##
# in the plots. The si_means_persample table was created in the script scarcity.R, where the si values##
# of all taxons were calculated for each sample. The si_means_persample table has the mean si values  ##
# of each sample, the richness and the diversity of each sample.                                      ##
########################################################################################################
set.seed(123)

# load si_means_persample table
si <- read_csv("input/scarcity_means_persample_08_09_2023.csv")

si$life_style <- as.factor(si$life_style)
## There is something very strange with this function. But it was the only way
## I found to reorder the DF based in the life style, ecosystem, then the mean,
## nested in this order.
#si <- si %>%

#  group_by(life_style) %>%
#  mutate(life_style = fct_relevel(life_style, "host-associated", "free-living")) %>%
#  ungroup() %>%

# group_by(ecosystem) %>%
#  mutate(mean = mean(mean_si)) %>%
#  ungroup() %>%
#  mutate(ecosystem = fct_reorder(ecosystem, mean)) %>%

#   group_by(life_style) %>%
#   mutate(ecosystem = fct_reorder(ecosystem, mean)) %>%
#   select(-mean) %>%
#   ungroup() %>%

#  group_by(ecosystem, habitat) %>%
#  mutate(mean = mean(mean_si)) %>%
#   ungroup() %>%

#  group_by(ecosystem) %>%
#  mutate(habitat = fct_reorder(habitat, mean)) %>%
#  select(-mean)

#si$life_style <- as.factor(si$life_style)
#si$habitat <- as.factor(si$habitat)

si <- si %>% 
      mutate(
            ecosystem = case_when(
            ecosystem== "human_host-associated" ~ "Human host",
            ecosystem == "animal_host-associated" ~ "Animal host",
            ecosystem == "plant_associated" ~ "Plant host",
            ecosystem == "groundwater" ~ "Groundwater",
            ecosystem == "freshwater" ~ "Freshwater",
            ecosystem == "wastewater" ~ "Wastewater",
            ecosystem == "saline_water" ~ "Saline Water",
            ecosystem == "sediment" ~ "Sediment",
            ecosystem == "soil" ~ "Soil"))
si <- si %>% 
      mutate(
            life_style = case_when(
              life_style == "host-associated" ~ "Host associated",
              life_style == "free-living" ~ "Free living"
            ))

si$ecosystem <- as.factor(si$ecosystem)

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
si_habitat <- si  %>%
      group_by(life_style, habitat, ecosystem) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_si = median(mean_si),
        mean_mean_si = mean(mean_si),
        sd_mean_si = sd(mean_si),
        se_mean_si = sd_mean_si / sqrt(n)) %>%
      select(-n)


#Now with distinctiveness
di <- read_csv("input/distinctiveness_means_persample_08_09_2023.csv")

di$life_style <- as.factor(di$life_style)

## There is something very strange with this function. But it was the only way
## I found to reorder the DF based in the life style, ecosystem, then the mean,
## nested in this order.
di <- di %>%

  group_by(life_style) %>%
  mutate(life_style = fct_relevel(life_style, "host-associated", "free-living")) %>%
  ungroup() %>%

  group_by(ecosystem) %>%
  mutate(mean = mean(mean_di)) %>%
  ungroup() %>%
  mutate(ecosystem = fct_reorder(ecosystem, mean)) %>%

#   group_by(life_style) %>%
#   mutate(ecosystem = fct_reorder(ecosystem, mean)) %>%
#   select(-mean) %>%

  group_by(ecosystem, habitat) %>%
  mutate(mean = mean(mean_di)) %>%
  ungroup() %>%

  group_by(ecosystem) %>%
  mutate(habitat = fct_reorder(habitat, mean)) %>%
  select(-mean)

di$life_style <- as.factor(di$life_style)

di <- di %>% 
      mutate(
            ecosystem = case_when(
            ecosystem== "human_host-associated" ~ "Human host",
            ecosystem == "animal_host-associated" ~ "Animal host",
            ecosystem == "plant_associated" ~ "Plant host",
            ecosystem == "groundwater" ~ "Groundwater",
            ecosystem == "freshwater" ~ "Freshwater",
            ecosystem == "wastewater" ~ "Wastewater",
            ecosystem == "saline_water" ~ "Saline Water",
            ecosystem == "sediment" ~ "Sediment",
            ecosystem == "soil" ~ "Soil"))

di <- di %>%
      mutate(
            life_style = case_when(
              life_style == "host-associated" ~ "Host associated",
              life_style == "free-living" ~ "Free living"
            ))

di$ecosystem <- as.factor(di$ecosystem)

#life_style summarized di means
di_life_style <- di %>%
      group_by(life_style) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_di = median(mean_di),
        mean_mean_di = mean(mean_di),
        sd_mean_di = sd(mean_di),
        se_mean_di = sd_mean_di / sqrt(n)) %>%
      select(-n)


#ecosystem summarized di means
di_ecosystem <- di  %>%
      group_by(life_style, ecosystem) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_di = median(mean_di),
        mean_mean_di = mean(mean_di),
        sd_mean_di = sd(mean_di),
        se_mean_di = sd_mean_di / sqrt(n)) %>%
      select(-n)
    
#habitat summarized di means
di_habitat <- di  %>%
      group_by(life_style, habitat, ecosystem) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median_mean_di = median(mean_di),
        mean_mean_di = mean(mean_di),
        sd_mean_di = sd(mean_di),
        se_mean_di = sd_mean_di / sqrt(n)) %>%
      select(-n)


#Changing names of the habitats to not have the underlines

#Colors
eco_colors <- c(
  "Human host" = "#DA70D6",
  "Animal host" = "#FFD700",
  "Plant host" = "#228B22",
  "Groundwater" = "#3A5FCD",
  "Freshwater" = "#87CEFA",
  "Wastewater" = "#000000",
  "Saline Water" = "#20B2AA",
  "Sediment" = "#F4A460",
  "Soil" = "#8B4513"
)

#removin underlines from the names
redundance_index$habitat <- gsub("_", " ", redundance_index$habitat)


#Sumarization 
habitat_sumirized_redundance <- redundance_index  %>%
      group_by(life_style, habitat, ecosystem) %>%
      dplyr::summarise(
        n = dplyr::n(),
        median = median(FunRedundancy),
        mean_redundance = mean(FunRedundancy),
        sd_redundance = sd(FunRedundancy),
        se_redundance = sd_redundance / sqrt(n)) %>%
      select(-n)

#Ploting
plot <- ggplot(
    data = redundance_index,
    aes(
        y = FunRedundancy,
        x = reorder(redundance_index$habitat, redundance_index$FunRedundancy),
        color = ecosystem)) +

    ## theme
    theme_pubr() +
    theme(
      text = element_text(size = unit(10, "cm")),
      axis.text.x = element_text(angle = 80, vjust = 1, hjust = 1,
       size=15,face ="plain", margin=margin(t= .2)),
      axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)),
      axis.text.y = element_text(size = 10, face = "plain"),
      axis.line = element_line(
        colour = "black", size = 0.5, linetype = "solid")) +
      scale_color_manual(values = eco_colors) +

    labs(x = "",
        y = "Functional Redundancy") +

    ## blank elements
    theme(
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_blank()) +

    ## points
    geom_jitter(
        aes(
            y = FunRedundancy),
    alpha = 0.5,
    width = 0.1) +

    ##error bars
    geom_errorbar(
    data = habitat_sumirized_redundance,
    aes(
     ymin = .data[["mean_redundance"]] - .data[["sd_redundance"]],
     ymax = .data[[ "mean_redundance"]] + .data[["sd_redundance"]],
     y = .data[["mean_redundance"]],
     x = .data[["habitat"]]),
    position = position_nudge(x = 0.0),
    width = 0.5,
    color = "#ee5c42") +

    ## mean
    geom_point(
    data = habitat_sumirized_redundance,
    aes(
        x = .data[["habitat"]],
        y = .data[["mean_redundance"]]),
    size = 2,
    color = "#ee5c42") +

    ## facet grid
    facet_grid( .~ ecosystem, scales = "free_x", space = "free_x")

plot <- plot +
    theme(legend.position = "none")

plot
 
#########################################################################################################
# Description: This script contains the code to generate the plots for the project.                    ##                         
# Need to source dw_index_tables.                                                                                                      ##
#########################################################################################################

source("src/data_wrangling/dw_index_tables.R")
source("src/pipelines/plot_functions/draw_barplot.R")

#Colors
dual_pallete <- c(
  "host-associated" = "#ffd700",
  "free-living" = "#00b2ef")
pallete <- dual_pallete

axis_text_angle = 45

eco_colors <- c(
  "human_host-associated" = "#DA70D6",
  "animal_host-associated" = "#FFD700",
  "plant_associated" = "#228B22",
  "groundwater" = "#3A5FCD",
  "freshwater" = "#87CEFA",
  "wastewater" = "#000000",
  "saline_water" = "#20B2AA",
  "sediment" = "#F4A460",
  "soil" = "#8B4513"
)

#order ecosystem factor in ascending order 
si_ecosystem$ecosystem <- factor(si_ecosystem$ecosystem, levels = c("plant_associated",
                                                                  "wastewater",
                                                                  "soil",
                                                                  "sediment", 
                                                                  "freshwater", 
                                                                  "groundwater",
                                                                  "saline_water",
                                                                  "human_host-associated",
                                                                  "animal_host-associated"))


ecosystem_plot <- bar_plot(
    data = si,
    x_var = "ecosystem",
    y_var = "mean_si",
    fill_var = "ecosystem",
    summarized_data = si_ecosystem,
    average_var = "mean_mean_si",
    error_var = "se_mean_si",
    fill_pallete = eco_colors
)
ecosystem_plot

ggsave("results/plots/ecosystem_plot.png", width = 15, height = 10, units = "in")


#life_style_plot
life_style_plot <- bar_plot(
    data = si,
    x_var = "life_style",
    y_var = "mean_si",
    fill_var = "life_style",
    summarized_data = si_life_style,
    average_var = "mean_mean_si",
    error_var = "se_mean_si",
    fill_pallete = pallete
)

life_style_plot

ggsave("results/plots/life_style_plot.png", width = 10, height = 15, units = "in")

#mean_si versus diversity regression plot
si_diversity_plot <- ggplot(si, aes(x = mean_si, y = diversity)) +
  geom_point(aes(
    color = life_style
  )) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Scarcity", y = "Diversity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1)) +
  scale_color_manual(values = pallete)
si_diversity_plot

ggsave("results/plots/si_x_diversity_plot.png", width = 10, height = 10, units = "in")

#mean_si versus richness regression plot
si_richness_plot <- ggplot(si, aes(x = mean_si, y = richness)) +
  geom_point(
    aes(
      color = life_style
    )
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Scarcity", y = "Richness") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1)) +
  scale_color_manual(values = pallete)
si_richness_plot

ggsave("results/plots/si_x_richness_plot.png", width = 10, height = 10, units = "in")

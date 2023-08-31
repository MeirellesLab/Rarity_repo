#########################################################################################################
# Description: This script contains the code to generate the plots for the project.                    ##                         
# Need to source dw_index_tables.                                                                                                      ##
#########################################################################################################

source("src/data_wrangling/dw_index_tables.R")
source("src/pipelines/plot_functions/draw_barplot.R")

#Colors
dual_pallete <- c(
  "Host associated" = "#ffd700",
  "Free living" = "#00b2ef")
pallete <- dual_pallete

axis_text_angle = 45

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

#order ecosystem factor in ascending order 
si_ecosystem$ecosystem <- factor(si_ecosystem$ecosystem, levels = c("Plant host",
                                                                  "Wastewater",
                                                                  "Soil",
                                                                  "Sediment", 
                                                                  "Freshwater", 
                                                                  "Groundwater",
                                                                  "Saline Water",
                                                                  "Human host",
                                                                  "Animal host"))


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

ggsave("results/plots/mean-si_x_diversity_plot.png", width = 10, height = 10, units = "in")

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

ggsave("results/plots/mean-si_x_richness_plot.png", width = 10, height = 10, units = "in")


### Tryinng boxplot instead of barplot for ecosystem and life_style plots
eco_boxplot <- ggplot() +
  geom_boxplot(
    data = si,
    aes(
      x = reorder(ecosystem, mean_si, median),
      y = mean_si,
      fill = ecosystem
    ),
    width = 0.5
  ) +
  theme_pubr() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()) +
  theme(axis.line = element_line()) +
  #axist title and text
  theme(
    axis.text.y = element_text(size = unit(9, "cm")),
    axis.text.x = element_text(
      size = unit(12, "cm"),
      angle = 45,
      vjust = 0.6)) +
  scale_fill_manual(values = eco_colors) +
  labs(y = "Mean Scarcity", x = "Ecosystem") +
  theme(
    axis.title.x = element_text(
    size = unit(15, "cm"),
    face = "bold"),
    axis.title.y = element_text(
    size = unit(15, "cm"),
    face = "bold")) +
  theme(legend.position = "none")
eco_boxplot

ggsave("results/plots/ecosystem_boxplot.png", width = 10, height = 12, units = "in")

lfst_boxplot <- ggplot() +
  geom_boxplot(
    data = si,
    aes(
      x = reorder(life_style, mean_si, median),
      y = mean_si,
      fill = life_style
    ),
    width = 0.5
  ) +
  theme_pubr() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()) +
  theme(axis.line = element_line()) +
  #axist title and text
  theme(
    axis.text.y = element_text(size = unit(9, "cm")),
    axis.text.x = element_text(
      size = unit(12, "cm"),
      angle = 45,
      vjust = 0.6)) +
  scale_fill_manual(values = pallete) +
  labs(y = "Mean Scarcity", x = "Life Style") +
  theme(
    axis.title.x = element_text(
    size = unit(15, "cm"),
    face = "bold"),
    axis.title.y = element_text(
    size = unit(15, "cm"),
    face = "bold")) +
  theme(legend.position = "none")
lfst_boxplot

ggsave("results/plots/life-style_boxplot.png", width = 10, height = 12, units = "in")

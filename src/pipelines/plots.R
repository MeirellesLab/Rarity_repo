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

set.seed(123)
#order ecosystem factor in ascending order 
si_ecosystem$ecosystem <- factor(si_ecosystem$ecosystem, levels = c("Sediment",
                                                                  "Freshwater",
                                                                  "Soil",
                                                                  "Wastewater",
                                                                  "Plant host",                                                                   
                                                                  "Groundwater",
                                                                  "Saline Water",
                                                                  "Animal host",
                                                                  "Human host"))


ecosystem_si_barplot <- bar_plot(
    data = si,
    x_var = "ecosystem",
    y_var = "mean_si",
    fill_var = "ecosystem",
    summarized_data = si_ecosystem,
    average_var = "mean_mean_si",
    error_var = "se_mean_si",
    fill_pallete = eco_colors
)
ecosystem_si_barplot

ggsave("results/plots/ecosystem_si_barplot.png", width = 15, height = 10, units = "in")


#life_style_plot
life_style_si_barplot <- bar_plot(
    data = si,
    x_var = "life_style",
    y_var = "mean_si",
    fill_var = "life_style",
    summarized_data = si_life_style,
    average_var = "mean_mean_si",
    error_var = "se_mean_si",
    fill_pallete = pallete
)

life_style_si_barplot

ggsave("results/plots/life_style_si_barplot.png", width = 10, height = 15, units = "in")

#mean_si versus diversity regression plot
si_diversity_plot <- ggplot(si, aes(x = mean_si, y = Simpson)) +
  geom_point(aes()) +
  labs(x = "Mean Scarcity", y = "Diversity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1))
si_diversity_plot

ggsave("results/plots/mean-si_x_diversity_plot.png", width = 10, height = 10, units = "in")

#mean_si versus richness regression plot
si_richness_plot <- ggplot(si, aes(x = mean_si, y = richness)) +
  geom_point(
    aes()
  ) +
  labs(x = "Mean Scarcity", y = "Richness") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1)) 
si_richness_plot

ggsave("results/plots/mean-si_x_richness_plot.png", width = 10, height = 10, units = "in")


### Tryinng boxplot instead of barplot for ecosystem and life_style plots
eco_si_boxplot <- ggplot() +
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
eco_si_boxplot

ggsave("results/plots/ecosystem_si_boxplot.png", width = 10, height = 12, units = "in")

lfst_si_boxplot <- ggplot() +
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
lfst_si_boxplot

ggsave("results/plots/life-style_si_boxplot.png", width = 10, height = 12, units = "in")


###########################################################################################################################
## Distinctiveness plots

di_ecosystem$ecosystem <- factor(di_ecosystem$ecosystem, levels = c("Sediment",
                                                                  "Freshwater",
                                                                  "Soil",
                                                                  "Wastewater",
                                                                  "Plant host",                                                                   
                                                                  "Groundwater",
                                                                  "Saline Water",
                                                                  "Animal host",
                                                                  "Human host"))

ecosystem_di_plot <- bar_plot(
    data = di,
    x_var = "ecosystem",
    y_var = "mean_di",
    fill_var = "ecosystem",
    summarized_data = di_ecosystem,
    average_var = "mean_mean_di",
    error_var = "se_mean_di",
    fill_pallete = eco_colors
)
ecosystem_di_plot

ggsave("results/plots/ecosystem_distinctiveness_barplot.png", width = 15, height = 10, units = "in")


#life_style_plot
life_style_di_plot <- bar_plot(
    data = di,
    x_var = "life_style",
    y_var = "mean_di",
    fill_var = "life_style",
    summarized_data = di_life_style,
    average_var = "mean_mean_di",
    error_var = "se_mean_di",
    fill_pallete = pallete
)

life_style_di_plot

ggsave("results/plots/life_style_distinctiveness_barplot.png", width = 10, height = 15, units = "in")

#mean_di versus diversity regression plot
di_diversity_plot <- ggplot(di, aes(x = mean_di, y = Simpson)) +
  geom_point(aes()) +
  labs(x = "Mean Distinctiveness", y = "Diversity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1))
di_diversity_plot

ggsave("results/plots/mean-di_x_diversity_plot.png", width = 10, height = 10, units = "in")

#mean_di versus richness regression plot
di_richness_plot <- ggplot(di, aes(x = mean_di, y = richness)) +
  geom_point(
    aes()
  ) +
  labs(x = "Mean Distinctiveness", y = "Richness") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1))
di_richness_plot

ggsave("results/plots/mean-di_x_richness_plot.png", width = 10, height = 10, units = "in")

#mean_di versus Fnctional diversity (rao diversity)
di_rao_plot <- ggplot(di, aes(x = mean_di, y = FunRao)) +
  geom_point(
    aes()
  ) +
  labs(x = "Mean Distinctiveness", y = "Functional Diversity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = axis_text_angle, hjust = 1))
di_rao_plot

ggsave("results/plots/mean-di_x_rao_plot.png", width = 10, height = 10, units = "in")

### Trying boxplot instead of barplot for ecosystem and life_style plots
di$ecosystem <- factor(di$ecosystem, levels = c("Sediment",
                                                                  "Freshwater",
                                                                  "Soil",
                                                                  "Wastewater",
                                                                  "Plant host", 
                                                                  "Saline Water",
                                                                   "Groundwater",
                                                                  "Animal host",
                                                                  "Human host"))



eco_di_boxplot <- ggplot() +
  geom_boxplot(
    data = di,
    aes(
      x = ecosystem,
      y = mean_di,
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
  labs(y = "Mean Distinctiveness", x = "Ecosystem") +
  theme(
    axis.title.x = element_text(
    size = unit(15, "cm"),
    face = "bold"),
    axis.title.y = element_text(
    size = unit(15, "cm"),
    face = "bold")) +
  theme(legend.position = "none")
eco_di_boxplot

ggsave("results/plots/ecosystem_di_boxplot.png", width = 10, height = 12, units = "in")

lfst_di_boxplot <- ggplot() +
  geom_boxplot(
    data = di,
    aes(
      x = life_style,
      y = mean_di,
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
  labs(y = "Mean Distinctiveness", x = "Life Style") +
  theme(
    axis.title.x = element_text(
    size = unit(15, "cm"),
    face = "bold"),
    axis.title.y = element_text(
    size = unit(15, "cm"),
    face = "bold")) +
  theme(legend.position = "none")
lfst_di_boxplot

ggsave("results/plots/life-style_di_boxplot.png", width = 10, height = 12, units = "in")


######## Arranging plots ###################

#Si plots
si_boxplots <- ggarrange(
  eco_si_boxplot,
  lfst_si_boxplot,
  labels = c("A", "B"),
  ncol = 2,
  nrow = 1
)
si_boxplots

ggsave("results/plots/si_boxplots.png", width = 20, height = 12, units = "in")

#Di plots
di_boxplots <- ggarrange(
  eco_di_boxplot,
  lfst_di_boxplot,
  labels = c("C", "D"),
  ncol = 2,
  nrow = 1
)
di_boxplots

ggsave("results/plots/di_boxplots.png", width = 20, height = 12, units = "in")

#Si and Di plots (Panel 1)
si_di_boxplots <- ggarrange(
  si_boxplots,
  di_boxplots,
  ncol = 1,
  nrow = 2
)
si_di_boxplots

ggsave("results/plots/si_di_boxplots_panel.png", width = 20, height = 18, units = "in")

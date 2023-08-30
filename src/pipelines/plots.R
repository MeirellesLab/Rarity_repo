#########################################################################################################
# Description: This script contains the code to generate the plots for the project.                    ##                         
# Need to source dw_index_tables.                                                                                                      ##
#########################################################################################################

source("src/data_wrangling/dw_index_tables.R")
source("src/pipelines/plot_functions/draw_barplot.R")

#Colors
dual_pallete <- c(
  "host_associated" = "#ffd700",
  "free_living" = "#00b2ef")
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

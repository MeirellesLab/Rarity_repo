stacked_bar_plot <- function(
  data,
  x_var,
  y_var,
  fill_var,
  summarized_data,
  average_var,
  error_var,
  fill_pallete) {

 plot <- ggplot(
    data = data,
    aes(
      y = .data[[y_var]],
      x =.data[[x_var]]
      )) +

  ##theme and legend
    theme_pubr() +
    theme(
      text = element_text(size = 12, face =  "bold"),
      axis.text.x = element_text(angle = axis_text_angle, vjust = 1, hjust = 1,
       size=15,face ="plain", margin=margin(t= .2)),
      axis.text.y = element_text(size=15,face ="plain"),
      legend.title = element_text(size = 10),
        legend.text = element_text(size=8,face ="plain"),
      legend.spacing.x = unit(0.3, "cm"),
      legend.spacing.y = unit(0.3, "cm")) +
    guides(fill = guide_legend(title = "Life Style",
                        title.position = "left",
                        shape = 16)) +
    theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold", size = 16))+
    theme(axis.title.x = element_text(margin = margin(t = 6 )))+
    theme(axis.title.y = element_text(margin = margin(r = 10 )))+
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'))+
    theme(axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
    theme(axis.ticks.length = unit(0.2,"cm"))+
    theme(legend.justification = "top")+
    theme(plot.margin = unit(c(1,1,0.5,1), "cm")) +
    guides(color = FALSE) + 
    scale_color_brewer(palette = "Paired") +

    ###blanking elements
    theme(
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()#,
      #axis.ticks = element_blank()
      ) +

    ##colors
    scale_fill_manual(values = fill_pallete) +

    ##labs
    labs(
      x = " ",
      y = "Functional Redundancy") +

    ##error bars
    #geom_errorbar(
    #  data = summarized_data,
    #  aes(
    #    ymin = .data[[average_var]] - .data[[error_var]],
    #    ymax = .data[[average_var]] + .data[[error_var]],
    #    y = .data[[average_var]],
    #    x = .data[[x_var]]),
    #    color = "#ee5c42",
    #    position = position_nudge(x = 0.0),
    #    width = 0.1 
    #    ) +
      

    geom_bar(
      data = summarized_data,
      stat = "identity",
      aes(
        y = .data[[average_var]],
        x = reorder(.data[[x_var]], +.data[[average_var]]),
        fill = .data[[fill_var]]),
        position = "stack",
        width = 0.5,
        show.legend = FALSE)
  }


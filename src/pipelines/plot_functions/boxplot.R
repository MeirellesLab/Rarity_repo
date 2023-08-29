draw_boxplot <- function(data, x_data, y_data, y_lab, x_lab, color){

    plot <- ggplot(
                data,
                aes(
                    x = reorder(.data[[x_data]], .data[[y_data]], FUN = median),
                    y = .data[[y_data]])) +
                geom_boxplot(fill = color, width = 0.5) +

                #general theme and blank elements
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
                        size = unit(9, "cm"),
                        angle = 45,
                        vjust = 1)) +
                labs(y = y_lab, x = x_lab)
                theme(
                    axis.title.x = element_text(
                    size = unit(9, "cm"),
                    face = "bold"),
                  axis.title.y = element_text(
                    size = unit(9, "cm"),
                    face = "bold")
                    )


return(plot)
}

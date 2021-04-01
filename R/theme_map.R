theme_map <- 
  function(base_size = 11, 
           base_family = "",
           ...){
    ggplot2::theme_minimal(base_size = base_size, 
                      base_family = base_family) %+replace%
      ggplot2::theme(
        legend.justification = "left",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.width = unit(0, 'npc'),
        legend.key.height = unit(0.02, 'npc'),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "npc"),
        ...
      )
  }
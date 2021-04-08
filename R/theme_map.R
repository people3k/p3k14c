#' A custom theme for ggplot maps.
#'
#' @param ... Parameters passed on to `theme`
#'
#' @importFrom ggplot2 `%+replace%`
#' @export
theme_map <- 
  function(...){
    ggplot2::theme_minimal() %+replace%
      ggplot2::theme(
        legend.justification = "left",
        legend.title = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        legend.key.width = ggplot2::unit(0, 'npc'),
        legend.key.height = ggplot2::unit(0.02, 'npc'),
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
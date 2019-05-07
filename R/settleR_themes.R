#' Modified ggplot2 theme
#'
#' This just returns a modifed version of \link[ggplot2]{theme_void}
#' @param base_size font size
#'
#' @return
#' @export
#'
#' @examples
theme_empty <- function(base_size=15){
  half_line <- base_size/2
  custom_theme <-
    theme(line = element_blank(),
          rect = element_blank(),
          text = element_text(
            face = "bold",
            colour = "black",
            size = base_size,
            lineheight = 0.9,
            hjust = 0.5,
            vjust = 0.5,
            angle = 0,
            margin = margin(),
            debug = FALSE),

          axis.title = element_blank(),
          axis.ticks.length = unit(0, "pt"),


          legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "lines")
    )
  return(custom_theme)
}


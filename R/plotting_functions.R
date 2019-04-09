#' Creates ggplot bars of set size
#'
#' bottom left plot which shows the size of the sets
#'
#' @param set_totals
#'
#' @return
#' @export
#'
#' @examples
set_totals_bar_plot <- function(set_totals){
  p <- ggplot(data=set_totals, aes(x=set_names, y=total_set_size)) +
    geom_bar(stat='identity', width = .5) +
    coord_flip() +
    ggrepel::geom_label_repel(aes(y=total_set_size,
                   label=total_set_size),
               hjust=1) +
    # theme(aspect.ratio = 2/1) +
    # scale_y_reverse() +
    theme_void() +

    theme(aspect.ratio = .5,
          plot.margin = margin(0,0,0,0))
  # p <- p + labs(title = 'Size of set')
    # theme(plot.margin=margin(0, 0, 0, -5, "line"),
    #       axis.text = element_blank(),
    #       axis.title = element_blank(),
    #       axis.line = element_blank(),
    #       panel.grid = element_blank()
    #       )
  axis_title <-  grobTree(textGrob("Size of set",
                                   x=0.1,
                                   y=1.05,
                                   hjust=0,
                                   gp=gpar(col="black",
                                           fontsize=20,
                                           fontface="bold")))
  p <- p + annotation_custom(axis_title)

  return(p)

}


#' Creates bar plot of the intersects
#'
#' @param intersect_data
#'
#' @return
#' @export
#'
#' @examples
intersect_bar_plot <- function(intersect_data){

  p <- ggplot(data=intersect_data, aes(x=intersect_id, y=intersect_set_size)) +
    geom_bar(stat='identity', width = .5) +
    ggrepel::geom_label_repel(aes(y=intersect_set_size,label=intersect_set_size), vjust=1) +

    theme_void() +

    theme(plot.margin = margin(0,0,0,0),
          aspect.ratio = .5)

  axis_title <-  grobTree(textGrob("Size of overlap",
                                   x=0.25,
                                   y=1.05,
                                   hjust=0,
                                   gp=gpar(col="black",
                                           fontsize=20,
                                           fontface="bold")))
  p <- p + annotation_custom(axis_title)
  return(p)

}

#' Plotting showing the grids
#'
#' @param grid_data
#'
#' @return
#' @export
#'
#' @examples
grid_dot_plot <- function(grid_data, dot_size=5, col_map=NULL){
  v_breaks <- seq(1,length(unique(grid_data$intersect_id)) -1, by = 1)+ .5
  h_breaks <- seq(1,length(unique(grid_data$set_names)) - 1, by = 1)+ .5
  p <-  ggplot(data=grid_data, aes(x=intersect_id, y=set_names)) +
    geom_point(size=dot_size, alpha=.25) +
    theme_void() # just plots empty dots

  p <- p +
    geom_line(data=grid_data[grid_data$observed,], # adds the lines
                     aes(x=intersect_id, y=set_names, group=intersect_id),
                     size=dot_size/3,
                     inherit.aes = FALSE)
  p <- p +
    geom_point(data=grid_data[grid_data$observed,], # Add `filled in points`
                      aes(x=intersect_id, y=set_names),
                      size=dot_size,
                      inherit.aes = FALSE)

  p <- p +
    geom_vline(xintercept = v_breaks) +
    geom_hline(yintercept = h_breaks)
  p <- p + theme(aspect.ratio = .5,
                 # plot.margin=margin(1, 1, 1, 1, "line"),
                 axis.text.y = element_text(size=dot_size*3,
                                            face='bold'))# +
    # scale_y_discrete(label=function(x){
    #   stringr::str_trunc(x, width = 20, side = 'center')
    # })
  if(!is.null(col_map)){
    # text_colours <- col_map[match(names(col_map), levels(grid_data$set_names))]
    text_colours <- col_map[match( levels(grid_data$set_names), names(col_map))]
    p <- p + theme(axis.text.y = element_text(colour=text_colours))
  }

  return(p)
}

#' Wrapper to align and put the plots together
#'
#' @param ol
#'
#' @return
#' @export
#'
#' @examples
upset_wrapper <- function(ol){
  # p1 <- grid_dot_plot(ol$grid_data)
  # p2 <- intersect_bar_plot(ol$intersect_data)
  # p3 <- set_totals_bar_plot(ol$set_totals)
  plt_list <- list(
    intersect_bar_plot(ol$intersect_data),
    set_totals_bar_plot(ol$set_totals),
    grid_dot_plot(ol$grid_data)
  )

  return(plt_list)
}

#' Title
#'
#' @param plt_list
#'
#' @return
#' @export
#'
#' @examples
make_upset_plot <- function(plt_list){
  # lapply(., ggplotGrob)
  # plt_list <- cowplot::align_plots(plotlist = plt_list, align = 'hv')
  # # l_mat <- matrix(c(NA, 1, 2, 3), ncol = 2, byrow = TRUE)
  # # l_mat <- matrix(c(1, NA, 2, 3), ncol = 2, byrow = TRUE)
  # l_mat <- matrix(c(1, NA, 3, 2), ncol = 2, byrow = TRUE)
  # out_plt <-  arrangeGrob(grobs=plt_list, layout_matrix = l_mat)
  return(out_plt)

}

#' Wrapper for upset plot
#'
#' @param upset_list
#' @param grid_size
#'
#' @return
#' @export
#'
#' @examples
upset_canvas_wrapper <- function(upset_list, grid_size=.75){

  pmain <- grid_dot_plot(upset_list$grid_data)
  # x_marg_plot <- axis_canvas(pmain, axis = 'x', data=upset_list$intersect_data, mapping = aes(x=x)) +
  #   geom_bar(aes(y=intersect_set_size), stat='identity') +
  #   ggrepel::geom_label_repel(aes(y=intersect_set_size,label=intersect_set_size))
  x_marg_plot <- intersect_bar_plot(upset_list$intersect_data)

  # y_marg_plot <- axis_canvas(pmain, axis = 'y',coord_flip = TRUE, data=upset_list$set_totals, mapping = aes(x=y)) +
  #   geom_bar(aes(y=total_set_size), stat='identity') +
  #   ggrepel::geom_label_repel(aes(y=total_set_size,label=total_set_size)) +
  #   coord_flip()

  y_marg_plot <- set_totals_bar_plot(upset_list$set_totals)

  plt_w_marg <-
    cowplot::insert_xaxis_grob(pmain, x_marg_plot, position = 'top', height = grid::unit(grid_size, "null") ) %>%
    cowplot::insert_yaxis_grob(., y_marg_plot, position = 'right', width = grid::unit(grid_size, "null"))
  return(plt_w_marg)
}


# l_mat <- matrix(c(NA, 1, 2, 3), ncol = 2, byrow = TRUE)
#
# p1 <- settleR:::grid_dot_plot(ol$grid_data)
# empty <- ggplot() + geom_blank()
# p2 <- ggplot(data=intersect_data, aes(x=intersect_id, y=freq)) + geom_bar(stat='identity', width = .4) + theme(aspect.ratio = 2/1) + theme_void()
# p3 <- ggplot(data=set_totals, aes( x=set_names, y=set_size)) + geom_bar(stat='identity', width = .4) + coord_flip() + theme_void() + theme(aspect.ratio = 2/1) + scale_y_reverse() + theme_void()
#
# gl <- cowplot::align_plots(plotlist = list( p2, p3, p1), align = 'hv')
# grid.arrange(grobs=gl, layout_matrix=l_mat)

# tmp_theme <- theme(axis.line.x = element_blank(),
#                    axis.title.x = element_blank(),
#                    axis.ticks.x = element_blank(),
#                    # axis.ticks.length = grid::unit(-5, "cm"),
#                    axis.ticks.y = element_blank(),
#                    axis.text.x = element_blank(),
#                    axis.text.y = element_blank(),
#                    axis.line.y = element_blank(),
#                    panel.background = element_blank(),
#                    plot.title = element_text(size = rel(1.5), face='bold'),
#                    plot.margin=margin(0, 0, 0, 0, "line")) ; x_marg_plot <-  ggplot(data=upset_list$set_totals, aes(x=y, y=total_set_size)) + geom_bar(stat='identity') + tmp_theme + labs(x=NULL, y=NULL, title = 'Size of overlap')

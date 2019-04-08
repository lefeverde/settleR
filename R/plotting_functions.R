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
    geom_bar(stat='identity', width = .4) +
    coord_flip() +
    theme_void() +
    # theme(aspect.ratio = 2/1) +
    # scale_y_reverse() +
    theme_void() +
    theme(plot.margin=margin(0, 0, 0, -1, "line"))
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
    geom_bar(stat='identity', width = .4) +
    theme(aspect.ratio = 2/1) +
    theme_void() +
    theme(plot.margin=margin(0, 0, -1, 0, "line"))
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
grid_dot_plot <- function(grid_data,dot_size=5){
  v_breaks <- seq(1,length(unique(grid_data$intersect_id)) -1, by = 1)+ .5
  h_breaks <- seq(1,length(unique(grid_data$set_names)) - 1, by = 1)+ .5
  p <-  ggplot(data=grid_data, aes(x=intersect_id, y=set_names)) +
    geom_point(size=dot_size, alpha=.25) +
    theme_void() # just plots empty dots
  p <- p +
    geom_line(data=grid_data[grid_data$observed,], # adds the lines
                     aes(x=intersect_id, y=set_names, group=intersect_id),
                     size=1.5,
                     inherit.aes = F)
  p <- p +
    geom_point(data=grid_data[grid_data$observed,], # Add `filled in points`
                      aes(x=intersect_id, y=set_names),
                      size=dot_size,
                      inherit.aes = F) +
    theme(plot.margin=margin(0, 0, 0, 0, "line"),
          axis.text.y = element_text(size=rel(10.5),
                                     face='bold'))
  p <- p +
    geom_vline(xintercept = v_breaks) +
    geom_hline(yintercept = h_breaks)
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
upset_wrapper <- function(bm){
  # p1 <- grid_dot_plot(ol$grid_data)
  # p2 <- intersect_bar_plot(ol$intersect_data)
  # p3 <- set_totals_bar_plot(ol$set_totals)
  plt_list <- list(
    intersect_bar_plot(bm),
    set_totals_bar_plot(bm),
    grid_dot_plot(bm)
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
  plt_list <- cowplot::align_plots(plotlist = plt_list, align = 'hv')
  # l_mat <- matrix(c(NA, 1, 2, 3), ncol = 2, byrow = TRUE)
  # l_mat <- matrix(c(1, NA, 2, 3), ncol = 2, byrow = TRUE)
  l_mat <- matrix(c(1, NA, 3, 2), ncol = 2, byrow = TRUE)
  out_plt <-  arrangeGrob(grobs=plt_list, layout_matrix = l_mat)
  return(out_plt)

}

upset_canvas_wrapper <- function(bm){

  x_map <- data.frame(x=seq(1, nlevels(bm$intersect_id)),
                      intersect_id=levels(bm$intersect_id))
  # Kludge
  x_map$intersect_set_size <-
    bm[match(x_map$intersect_id, bm$intersect_id),]$intersect_set_size

  y_map <- data.frame(y=seq(1, nlevels(bm$set_names)),
                      set_names=levels(bm$set_names))
  y_map$

  pmain <- grid_dot_plot(bm)
  x_marg_plot <- axis_canvas(pmain, axis = 'x', data=x_map, mapping = aes(x=x)) +
    geom_bar(aes(y=intersect_set_size), stat='identity')
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


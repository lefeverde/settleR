#' Creates a settleR plot starting from a named list of sets
#'
#' @inheritParams sets_to_matrix
#' @inheritParams calc_set_overlaps
#' @inheritParams grid_dot_plot
#' @inheritParams make_upset_plots
#' @inheritParams merge_upset_list
#'
#' @return
#' @export
#'
#' @examples
settleR_plot <- function(setlist,
                         nintersects = 15,
                         set_levels=NULL,
                         col_map=NULL,
                         grid_size=.75){
  upset_list <- sets_to_matrix(setlist) %>%
    calc_set_overlaps(.,
                      nintersects=nintersects,
                      set_levels=set_levels)
  upset_plt <- make_upset_plots(upset_list, col_map) %>%
    merge_upset_list
  return(upset_plt)
}

#' Creates ggplot bars of set size
#'
#' bottom left plot which shows the size of the sets
#'
#' @param set_totals data.frame with y, set_names, total_set_size as columns
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
               hjust=1,
               min.segment.length=.001) +

    theme_void() +
    theme(aspect.ratio = .5,
          plot.margin = margin(0,0,0,0))
  # This adds a title without messing up the
  # alignment via cowplot
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
#' @param intersect_data data.frame with x, intersect_id, intersect_set_size, and intersect_degree as columns
#'
#' @return
#' @export
#'
#' @importFrom grid grobTree textGrob gpar
#'
#' @examples
intersect_bar_plot <- function(intersect_data){

  p <- ggplot(data=intersect_data, aes(x=intersect_id, y=intersect_set_size)) +
    geom_bar(stat='identity', width = .5) +
    ggrepel::geom_label_repel(aes(y=intersect_set_size,
                                  label=intersect_set_size),
                              vjust=1,
                              min.segment.length=.001) +

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
#' @param grid_data data.frame with intersect_id, set_names, and observed as columns
#' @param dot_size size of dot
#' @param col_map optional named vector specifying text colours of y-axis labels
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

    # scale_y_discrete(label=function(x){
    #   stringr::str_trunc(x, width = 20, side = 'center')
    # })
  if(!is.null(col_map)){
    text_colours <- col_map[match( levels(grid_data$set_names), names(col_map))]
    p <- p + theme(axis.text.y = element_text(colour=text_colours))
  }
  # adds a numeric index to x axis
  p <- p + scale_x_discrete(labels=(seq(1, nrow(grid_data)))) +
    theme(aspect.ratio = .5,
          axis.text.y = element_text(size=dot_size*3,
                                     face='bold'),
          axis.text.x = element_text(size=dot_size*3,
                                     face='bold')
  )
  return(p)
}

#' Creates list of upset plots
#'
#' This is a wrapper which just wraps the creation
#' of the 3 plots for required for an upset plot.
#' I've broken the plot creation into simple
#' functions to make further customizations
#' easy without re-writing everything
#'
#' @param upset_list list of data.frames from \link[settleR]{calc_set_overlaps}
#' @param col_map optional named vector specifying text colours of y-axis labels in \link[settleR]{grid_dot_plot}
#'
#' @return
#' @export
#'
#' @examples
make_upset_plots <- function(upset_list, col_map=NULL){

  if(!all(names(upset_list) %in% c("grid_data",      "intersect_data","set_totals"))){
    stop('upset_list needs to be a named list with data.frames named: grid_data, intersect_data, and set_totals')
  }

  pmain <- grid_dot_plot(upset_list$grid_data,
                         col_map = col_map)

  x_marg_plot <- intersect_bar_plot(upset_list$intersect_data)
  y_marg_plot <- set_totals_bar_plot(upset_list$set_totals)

  plt_list <- list(pmain,x_marg_plot,y_marg_plot) %>%
    setNames(., c('pmain','x_marg_plot','y_marg_plot') )

  return(plt_list)
}


#' Merges list of ggplot upset into gtable object
#'
#' @param upset_list list of ggplots created from \link[settleR]{make_upset_plots} or similiarly structured
#' @param grid_size sets grid size
#'
#' @return
#' @export
#'
#' @examples
merge_upset_list <- function(upset_list, grid_size=.75){

  plt_w_marg <-
    cowplot::insert_xaxis_grob(upset_list$pmain,
                               upset_list$x_marg_plot,
                               position = 'top',
                               height = grid::unit(grid_size,
                                                   "null")) %>%

    cowplot::insert_yaxis_grob(.,
                               upset_list$y_marg_plot,
                               position = 'right',
                               width = grid::unit(grid_size,
                                                  "null"))
  return(plt_w_marg)
}




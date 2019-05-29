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
settleR_plot <- function(setList,
                         nIntersects=15,
                         setLevels=NULL,
                         colMap=NULL,
                         margSize=5){
  upset_list <- sets_to_matrix(setList) %>%
    calc_set_overlaps(.,
                      nIntersects=nIntersects,
                      setLevels=setLevels)
  upset_plt <- make_upset_plots(upset_list, colMap) %>%
    merge_upset_list(.,)
  return(upset_plt)
}

#' wrapper to save the upset plot created by settleR_plot
#'
#' Essentially, this is just a wrapper around ggsave. The difference is that
#' plot size is automatically calculated.
#' @param upset_plt an upset_plt gtabl object
#' @param fn file name
#'
#' @importFrom magrittr multiply_by
#' @importFrom gtable gtable_height gtable_width
#' @importFrom grid convertHeight
#' @return
#' @export
#'
#' @examples
settleR_save <- function(upset_plt, fn){
  hght <- gtable_height(upset_plt) %>%
    convertHeight(., 'cm', valueOnly = TRUE) %>%
    multiply_by(1.125) %>%
    round(., 1)
  wdth <- gtable_width(upset_plt) %>%
    convertHeight(., 'cm', valueOnly = TRUE) %>%
    multiply_by(1.125) %>%
    round(., 1)
  ggsave(fn, upset_plt, width = wdth, height = hght, units = 'cm')

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
                                           fontsize=15,
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

    theme_void()
  p <- p + theme(plot.margin = margin(0,0,0,0),
               aspect.ratio = .5)

  axis_title <-  grobTree(textGrob("Size of overlap",
                                   x=0.25,
                                   y=1.05,
                                   hjust=0,
                                   gp=gpar(col="black",
                                           fontsize=18,
                                           fontface="bold")))
  p <- p + annotation_custom(axis_title)
  return(p)

}

#' Plotting showing the grids
#'
#' @param grid_data data.frame with intersect_id, set_names, and observed as columns
#' @param dot_size size of dot
#' @param colMap optional named vector specifying text colours of y-axis labels
#'
#' @return
#' @export
#'
#' @examples
grid_dot_plot <- function(grid_data, dot_size=6.25, colMap=NULL){

  v_breaks <- seq(1,length(unique(grid_data$intersect_id)) -1, by = 1)+ .5
  h_breaks <- seq(1,length(unique(grid_data$set_names)) - 1, by = 1)+ .5
  p <-  ggplot(data=grid_data, aes(x=intersect_id, y=set_names)) +
    geom_point(size=dot_size, alpha=.25) +
    theme_empty()
  # just plots empty dots

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
  if(!is.null(colMap)){
    text_colours <- colMap[match( levels(grid_data$set_names), names(colMap))]
    p <- p + theme(axis.text.y = element_text(colour=text_colours))
  }
  # adds a numeric index to x axis
  p <- p + scale_x_discrete(labels=(seq(1, nrow(grid_data)))) +
    theme(aspect.ratio = .5,
          axis.text.y = element_text(size=dot_size*2,
                                     face='bold'),
          axis.text.x = element_text(size=dot_size*2,
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
#' @param colMap optional named vector specifying text colours of y-axis labels in \link[settleR]{grid_dot_plot}
#' @importFrom stats setNames
#' @return
#' @export
#'
#' @examples
make_upset_plots <- function(upset_list, colMap=NULL){

  if(!all(names(upset_list) %in% c("grid_data","intersect_data","set_totals"))){
    stop('upset_list needs to be a named list with data.frames named: grid_data, intersect_data, and set_totals')
  }

  pmain <- grid_dot_plot(upset_list$grid_data,
                         colMap = colMap)

  x_marg_plot <- intersect_bar_plot(upset_list$intersect_data)
  y_marg_plot <- set_totals_bar_plot(upset_list$set_totals)

  plt_list <- list(pmain,x_marg_plot,y_marg_plot) %>%
    setNames(., c('pmain','x_marg_plot','y_marg_plot') )

  return(plt_list)
}


#' Merges list of ggplot upset into gtable object
#'
#' @param plt_list list of ggplots created from \link[settleR]{make_upset_plots} or similiarly structured
#' @param margSize sets height or width of plots in margin size in cm
#'
#' @return
#' @export
#'
#' @examples
merge_upset_list <- function(plt_list, margSize=5){
  if(!all(names(plt_list) %in% c("pmain","x_marg_plot","y_marg_plot"))){
    stop('upset_list needs to be a named list with ggplot2 objects named: pmain, x_marg_plot, and y_marg_plot')
  }
  # Gets dims of grid matrix
  grid_dims <- plt_list$pmain$data %>%
    filter(., observed) %>%
    summarise(n_distinct(intersect_id),
              n_distinct(set_names)) %>%
    setNames(., c('x', 'y'))

  # converts to grob and hardcodes the size
  pmain <- plt_list$pmain %>%
    ggplotGrob(.) %>%
    set_panel_size(.,
                   width = .75*grid_dims$x,
                   height = .75*grid_dims$y)

  plt_w_marg <-
    cowplot::insert_xaxis_grob(pmain,
                               plt_list$x_marg_plot,
                               position = 'top',
                               height = grid::unit(margSize,
                                                   'cm')) %>%

    cowplot::insert_yaxis_grob(.,
                               plt_list$y_marg_plot,
                               position = 'right',
                               width = grid::unit(margSize,
                                                  'cm'))
  return(plt_w_marg)
}




#' Fixes size of ggplot grob object
#'
#' This sets the size of ggplot grob object to fixed
#' sizes. I've adapted this from \url{https://stackoverflow.com/a/30571289}.
#' The height and width parameters are in cm. May change in future.
#'
#' @param g a gtable object returned from \code{\link[ggplot2]{ggplotGrob}}
#' @param width an int
#' @param height also an int
#'
#' @return gtable object
#' @export
#' @importFrom grid unit
#' @examples
set_panel_size <- function(g, width=4, height=4){
  width <-  unit(width, "cm")
  height <- unit(height, "cm")
  panels <- grep("panel", g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

  g$widths[panel_index_w] <-  rep(width,  nw)
  g$heights[panel_index_h] <- rep(height, nh)


  return(g)
}




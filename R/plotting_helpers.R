##### File for SettleR plotting functions ####
# This file contains all of the functions for
# creating the SettleR plots. These are currently
# mostly set to be internal, but documented.


#### Making the plots ####
#' Creates ggplot bars of set size
#'
#' bottom left plot which shows the size of the sets
#'
#' @param set_totals data.frame with y, set_names, total_set_size as columns
#'
#' @return setPlotY
#' @keywords internal
#'
#' @examples
set_totals_bar_plot <- function(set_totals){
  p <- ggplot(data=set_totals, aes(x=set_names, y=total_set_size)) +
    geom_bar(stat='identity', width = .5) +
    coord_flip() +
    geom_label(aes(y=total_set_size,
                   label=total_set_size),
               hjust=-.1) +
    theme_void() +
    theme(aspect.ratio = .5,
          plot.margin = margin(0,10,0,0))
  p <- p + scale_y_continuous(expand = expansion(mult = c(0, .1)))

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
#' @return intersectPlotX
#' @keywords internal
#'
#' @importFrom grid grobTree textGrob gpar
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggtext geom_richtext
#'
#' @examples
intersect_bar_plot <- function(intersect_data){

    p <- ggplot(data=intersect_data, aes(x=intersect_id, y=intersect_set_size)) +
      geom_bar(stat='identity', width = .75) +
      geom_richtext(aes(y=(intersect_set_size), label=intersect_set_size), angle = 45, vjust=-.05, hjust=-.05) +
      # geom_label_repel(aes(y=intersect_set_size,
      #                      label=intersect_set_size),
      #                  # vjust=1,
      #                  min.segment.length=0) +
      theme_void()

    p <- p + theme(plot.margin = margin(10,0,0,0), aspect.ratio = .5)
    p <- p + scale_y_continuous(expand = expansion(mult = c(0, .1)))
    axis_title <-  grobTree(textGrob("Size of overlap",
                                     x=0.35,
                                     y=1,
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
#' @return gridPlot
#' @keywords internal
#'
#' @examples
grid_dot_plot <- function(grid_data, dot_size=4.25, colMap=NULL){
  v_max <- max(1, length(unique(grid_data$intersect_id)) -1)
  v_breaks <- seq(1, v_max, by = 1)+ .5
  h_max <- max(1, length(unique(grid_data$set_names)) - 1)
  h_breaks <- seq(1, h_max, by = 1)+ .5
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
#'
#' @return list of 3 upset plots
#'
#' @keywords internal
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
    setNames(., c("gridPlot","intersectPlotX","setPlotY"))

  return(plt_list)
}


#' Merges list of ggplot upset into gtable object
#'
#' @param plt_list list of ggplots created from \link[settleR]{make_upset_plots} or similiarly structured
#' @param margSize sets height or width of plots in margin size in cm
#'
#' @return  a merged settleR plot
#' @keywords internal
#'
#' @examples
merge_upset_list <- function(plt_list, margSize=5){

  # Gets dims of grid matrix
  grid_dims <- plt_list$gridPlot$data %>%
    filter(., observed) %>%
    summarise(n_distinct(intersect_id),
              n_distinct(set_names)) %>%
    setNames(., c('x', 'y'))

  # converts to grob and hardcodes the size
  pmain <- plt_list$gridPlot %>%
    ggplotGrob(.) %>%
    set_panel_size(.,
                   width = .75*grid_dims$x,
                   height = .75*grid_dims$y)

  plt_w_marg <-
    cowplot::insert_xaxis_grob(pmain,
                               plt_list$intersectPlotX,
                               position = 'top',
                               height = grid::unit(margSize,
                                                   'cm'), clip="off") %>%

    cowplot::insert_yaxis_grob(.,
                               plt_list$setPlotY,
                               position = 'right',
                               width = grid::unit(margSize,
                                                  'cm'), clip="off")
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
#' @keywords internal
#' @importFrom grid unit
#' @examples
#'
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



#### ggplot2 themes ####
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


#### Misc plotting functions ####

#' Returns x and y coords of a bounding box for a particular intersect
#'
#'
#'
#' @param grid_data_plot ggplot object, for example a plot created by \link[settleR]{grid_dot_plot} or similiar plots
#' @param intersects_to_box Vector of intercepts which specify intercepts to draw a box around
#' @param box_pad padding surrounding the box
#'
#' @return
#' @export
#'
#' @examples
box_intercepts_dims <- function(grid_data_plot,
                                intersects_to_box,
                                box_pad=.5125){

  #TODO make this accept integer idx instead of
  # intersect str

  if(all(class(grid_data_plot) !=  c('gg', 'ggplot'))){
    stop('grid_data_plot needs to be a ggplot2 object')
  }
  plt_build <- ggplot_build(grid_data_plot)
  orig_inter_order <- levels(plt_build$plot$data$intersect_id)

  coord_pos <-  plt_build[['data']][[2]] %>%
    group_by(., group) %>%
    summarise(
      xmin=min(x) - box_pad,
      xmax=max(x) + box_pad,
      ymin=min(y) - box_pad,
      ymax=max(y) + box_pad
    )
  coord_pos <- cbind(orig_inter_order,
                     coord_pos)
  names(coord_pos)[1] <- 'intersect_id'
  coord_pos <-
    coord_pos[coord_pos$intersect_id %in% intersects_to_box,]
  coord_pos$intersect_id <- as.character(coord_pos$intersect_id)
  return(coord_pos)

}


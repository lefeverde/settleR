




#' Creates the 3 plots for a settleR plot
#'
#' TODO incorperate this function into the SettleR validity checking
#'
#' @param settleRObject \link[settleR]{SettleR} object created by the constructor function.
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
make_settleR_plots <- function(settleRObject){

  #### Grid data ####
  gridData <-
    gridData(settleRObject) %>%
    filter(., intersect_id %in% intersectLevels(settleRObject)) %>%
    droplevels
  gridData$intersect_id <-
    factor(gridData$intersect_id,
           levels=intersectLevels(settleRObject))
  gridData <- gridData %>%
    arrange(., intersect_id)
  gridData$set_names <-
    factor(gridData$set_names,
           levels=setLevels(settleRObject))

  #### intersectData ####
  intersectData <-
    intersectData(settleRObject) %>%
    filter(., intersect_id %in% intersectLevels(settleRObject)  ) %>%
    droplevels
  intersectData$intersect_id <-
    factor(intersectData$intersect_id,
           levels=intersectLevels(settleRObject))
  intersectData <- intersectData %>%
    arrange(., intersect_id)
  # Re-indexing so that x axis is sequential
  intersectData$x <- seq(1, nrow(intersectData))
  #### setTotals ####
  setTotals <- setTotals(settleRObject)
  setTotals$set_names <-
    factor(setTotals$set_names,
           levels = setLevels(settleRObject))

  #### Plots ####
  # Kind of kludgey where I put the plots in
  # an S3 list. However, I currently think this
  # is the easiest to use with the getters/setters,
  # since I need to create them first and then
  # merge them to create the SettleR plot.
  gridPlot <-
    grid_dot_plot(gridData,
                  colMap = colMap(settleRObject),
                  use_cowplot=settleRObject@use_cowplot)
  intersectPlotX <- intersect_bar_plot(intersectData,
                                       use_cowplot=settleRObject@use_cowplot)
  setPlotY <- set_totals_bar_plot(setTotals,
                                  use_cowplot=settleRObject@use_cowplot)

  plt_list <- list(gridPlot, intersectPlotX, setPlotY) %>%
    setNames(., c('gridPlot','intersectPlotX','setPlotY') )

  # Merge via cowplot if set otherwise use patchwork
  if(settleRObject@use_cowplot){
    plt_list[['settleRPlot']] <- cowplot_merge_upset_list(plt_list)
  } else {
    plt_list[['settleRPlot']] <- merge_upset_list(plt_list)
  }

  plotList(settleRObject) <- plt_list
  return(settleRObject)
}



#' wrapper to save the upset plot created by settleR_plot
#'
#' Essentially, this is just a wrapper around ggsave. The difference is that
#' plot size is automatically calculated.
#' @param settleRObject \link[settleR]{SettleR} object created by the constructor function.
#' @param fn file name
#'
#' @importFrom magrittr multiply_by
#' @importFrom gtable gtable_height gtable_width
#' @importFrom grid convertHeight
#' @return saved settleR plot
#' @export
#'
#' @examples
settleR_save <- function(settleRObject, fn){

  plts <- plotList(settleRObject)
  upset_plt <- plts$settleRPlot

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

#' Plots the settleR plot on the device
#'
#' @param settleRObject \link[settleR]{SettleR} object created by the constructor function.
#'
#' @importFrom cowplot ggdraw
#'
#' @return plot drawn on the device
#' @export
#'
#' @examples
settleR_plot <- function(settleRObject){
  plts <- plotList(settleRObject)
  upset_plt <- plts$settleRPlot
  ggdraw(upset_plt)

}

#' Returns vector of intersect levels
#'
#'
#' Intersect levels are reordered such that the
#' exclusive  singleton sets are the first n
#' number of intersects and the remaining are
#' ordered from largest to smallest like normal.
#'
#' @param settleRObject \link[settleR]{SettleR} object created by the constructor function.
#'
#'
#' @return
#' @export
#'
#' @examples
reorder_by_singletons <- function(settleRObject){
  # TODO make this return an updated settleRObject

  original_intersects <- intersectLevels(settleRObject)

  all_gd <- gridData(settleRObject) %>%
    filter(., observed)

  singleton_intersects <- all_gd %>%
    group_by(., intersect_id) %>%
    summarise(., deg=n()) %>%
    filter(., deg==1)
  singleton_intersects$set_names <-
    all_gd$set_names[match(singleton_intersects$intersect_id,all_gd$intersect_id)]
  singleton_intersects <- singleton_intersects %>%
    arrange(., set_names) %>%
    pull(1) %>%
    as.character(.)

  original_intersects <-
    original_intersects[!original_intersects %in% singleton_intersects]
  intersect_lvls <- c(singleton_intersects, original_intersects)
  intersectLevels(settleRObject) <- intersect_lvls
  return(settleRObject)

}

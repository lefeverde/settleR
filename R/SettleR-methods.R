#' Creates the 3 plots for a settleR plot
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
  gridPlot <-
    grid_dot_plot(gridData,
                  colMap = colMap(settleRObject))
  intersectPlotX <- intersect_bar_plot(intersectData)
  setPlotY <- set_totals_bar_plot(setTotals)

  plt_list <- list(gridPlot, intersectPlotX, setPlotY) %>%
    setNames(., c('gridPlot','intersectPlotX','setPlotY') )
  plt_list[['settleRPlot']] <- merge_upset_list(plt_list)
  plotList(settleRObject) <- plt_list
  return(settleRObject)
}


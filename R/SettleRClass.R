### Constructor ####
#' Constructor for SettleR class
#'
#'
#'
#' @param setList List of sets
#' @param intersectLevels Optional vector of intersects which specifies ordering of intersects. If null, intersects are sorted by size.
#' @param setLevels Optional vector of the set names (y-axis on \link[settleR]{grid_dot_plot}). If null, sets are ordered by size.
#' @param nIntersects Integer number of intersects to show (default: 15)
#' @param colMap Optional named vector specifying text colours of y-axis labels
#'
#' @return
#' @export
#'
#' @examples
SettleR <- function(setList,
                    intersectLevels=NULL,
                    setLevels=NULL,
                    nIntersects=15,
                    colMap=NULL){

  binaryMat <- sets_to_matrix(setList)
  countDf <- create_count_df(binaryMat)

  # Data for X-top margin plot
  intersectData <-
    data.frame(intersect_set_size=countDf$freq,
               intersect_degree=rowSums(countDf[,seq(1, ncol(countDf) -1 )])) %>%
    rownames_to_column(., 'intersect_id')

  if(is.null(intersectLevels)){
    intersectLevels <-
      dplyr::arrange(intersectData, desc(intersect_set_size)) %>%
      dplyr::slice(., seq(1, nIntersects)) %>%
      pull(1)
  }

  intersectData <-
    dplyr::arrange(intersectData, intersect_id) %>%
    cbind(x=seq(1, nrow(.)), .)

  # Data for Y-right margin plot
  setTotals <- data.frame(colSums(binaryMat)) %>%
    rownames_to_column(.) %>%
    setNames(., c('set_names', 'total_set_size'))
  if(is.null(setLevels)){
    setLevels <-
      dplyr::arrange(setTotals, total_set_size) %>%
      pull(1)
  }
  setTotals$set_names <- factor(setTotals$set_names,
                                 levels=setLevels)
  setTotals <-
    dplyr::arrange(setTotals, set_names) %>%
    cbind(y=seq(1, nrow(.)), .)

  gridData <- countDf[,seq(1, ncol(countDf) -1)] %>%
    as.matrix(.) %>%
    reshape2::melt(.) %>%
    setNames(., c('intersect_id', 'set_names', 'observed'))
  # observed is basically a variable stating whether
  # the row combinations exists in the original data
  gridData$observed <- as.logical(gridData$observed)
  gridData$set_names <- factor(gridData$set_names,
                                levels=setLevels)

  if(is.null(colMap)){
    colMap <- rep('black', nrow(setTotals)) %>%
      setNames(., setTotals$set_names)
  }


  settleRObj <- new('SettleR',
                    setList=setList,
                    binaryMat=binaryMat,
                    countDf=countDf,
                    gridData=gridData,
                    intersectData=intersectData,
                    setTotals=setTotals,
                    setLevels=setLevels,
                    colMap=colMap,
                    intersectLevels=intersectLevels)
  # Kind of a kludge.
  # Basically, I create the class with empty plotList
  # and then
  settleRObj <- make_settleR_plots(settleRObj)
  return(settleRObj)

}


#### SettleR roxygen block ####
#' @title Setters and Getters
#'
#' @description Place to put all documentation for Setters and Getters
#' @name accessors
#' @inheritParams SettleR-class
#'
NULL

# TODO create nest of raw data, and settleRdata
# So it's consistant with plotList
### Class ####

#' SettleR class
#'
#' This is the class which stores all the data for creating a
#' settleR plot. It should be created through using the
#' \link[settleR]{SettleR} function.
#'
#'
#' @slot setList list of sets
#' @slot binaryMat binary matrix
#' @slot countDf data.frame.
#' @slot gridData data.frame.
#' @slot intersectData data.frame.
#' @slot setTotals data.frame.
#' @slot plotList list.
#' @slot setLevels character.
#' @slot colMap character.
#' @slot intersectLevels character.
#' @slot nIntersects numeric.
#'
#' @name SettleR-class
#' @rdname SettleR-class
#'
#' @return
#'
#'
#' @examples
#'
setClass('SettleR',
         slots=list(
           setList='list',
           binaryMat='matrix',
           countDf='data.frame',
           gridData='data.frame',
           intersectData='data.frame',
           setTotals='data.frame',
           plotList='list',
           setLevels='character',
           colMap='character',
           intersectLevels='character',
           nIntersects='numeric'
         )
)
### show ###
setMethod(f = 'show',
          signature = 'SettleR',
          definition = function(object){
            out_str <- 'SettleR class'
            cat(out_str)
      })

### gridData ####
#' @rdname accessors
#' @export
setGeneric('gridData', function(object){standardGeneric('gridData')})
setMethod(
  f = 'gridData',
  signature = 'SettleR',
  definition = function(object){
    return(object@gridData)
  })

#' @export
#' @rdname accessors
setGeneric('gridData<-', function(object,value){standardGeneric('gridData<-')})
setReplaceMethod(
  f = 'gridData',
  signature = 'SettleR',
  definition = function(object, value){
    object@gridData <- value
    return(object)
  })
### intersectData ####
#' @export
#' @rdname accessors
setGeneric('intersectData', function(object){standardGeneric('intersectData')})
setMethod(
  f = 'intersectData',
  signature = 'SettleR',
  definition = function(object){
    return(object@intersectData)
  })
#' @export
#' @rdname accessors
setGeneric('intersectData<-', function(object,value){standardGeneric('intersectData<-')})
setReplaceMethod(
  f = 'intersectData',
  signature = 'SettleR',
  definition = function(object, value){
    object@intersectData <- value
    return(object)
  })
### setTotals ####
#' @export
#' @rdname accessors
setGeneric('setTotals', function(object){standardGeneric('setTotals')})
setMethod(
  f = 'setTotals',
  signature = 'SettleR',
  definition = function(object){
    return(object@setTotals)
  })
#' @export
#' @rdname accessors
setGeneric('setTotals<-', function(object,value){standardGeneric('setTotals<-')})
setReplaceMethod(
  f = 'setTotals',
  signature = 'SettleR',
  definition = function(object, value){
    object@setTotals <- value
    return(object)
  })
### plotList ####
#' @export
#' @rdname accessors
setGeneric('plotList', function(object){standardGeneric('plotList')})
setMethod(
  f = 'plotList',
  signature = 'SettleR',
  definition = function(object){
    return(object@plotList)
  })
#' @export
#' @rdname accessors
setGeneric('plotList<-', function(object,value){standardGeneric('plotList<-')})
setReplaceMethod(
  f = 'plotList',
  signature = 'SettleR',
  definition = function(object, value){
    object@plotList <- value
    return(object)
  })
### setLevels ####
#' @export
#' @rdname accessors
setGeneric('setLevels', function(object){standardGeneric('setLevels')})
setMethod(
  f = 'setLevels',
  signature = 'SettleR',
  definition = function(object){
    return(object@setLevels)
  })
#' @export
#' @rdname accessors
setGeneric('setLevels<-', function(object,value){standardGeneric('setLevels<-')})
setReplaceMethod(
  f = 'setLevels',
  signature = 'SettleR',
  definition = function(object, value){
    object@setLevels <- value
    object <- make_settleR_plots(object)
    return(object)
  })
### intersectLevels ####
#' @export
#' @rdname accessors
setGeneric('intersectLevels', function(object){standardGeneric('intersectLevels')})
setMethod(
  f = 'intersectLevels',
  signature = 'SettleR',
  definition = function(object){
    return(object@intersectLevels)
  })
#' @export
#' @rdname accessors
setGeneric('intersectLevels<-', function(object,value){standardGeneric('intersectLevels<-')})
setReplaceMethod(
  f = 'intersectLevels',
  signature = 'SettleR',
  definition = function(object, value){
    object@intersectLevels <- value
    object <- make_settleR_plots(object)
    return(object)
  })
### colMap ####
#' @export
#' @rdname accessors
setGeneric('colMap', function(object){standardGeneric('colMap')})
setMethod(
  f = 'colMap',
  signature = 'SettleR',
  definition = function(object){
    return(object@colMap)
  })
#' @export
#' @rdname accessors
setGeneric('colMap<-', function(object,value){standardGeneric('colMap<-')})
setReplaceMethod(
  f = 'colMap',
  signature = 'SettleR',
  definition = function(object, value){
    object@colMap <- value
    object <- make_settleR_plots(object)
    return(object)
  })

### gridPlot ####
#' @export
#' @rdname accessors
setGeneric('gridPlot', function(object){standardGeneric('gridPlot')})
setMethod(
  f = 'gridPlot',
  signature = 'SettleR',
  definition = function(object){
    return(plotList(object)$gridPlot)
  })
#' @export
#' @rdname accessors
setGeneric('gridPlot<-', function(object,value){standardGeneric('gridPlot<-')})
setReplaceMethod(
  f = 'gridPlot',
  signature = 'SettleR',
  definition = function(object, value){
    plotList(object)$gridPlot <- value
    plotList(object) <- merge_upset_list(plotList(object))
    return(object)
  })

### intersectPlotX ####
#' @export
#' @rdname accessors
setGeneric('intersectPlotX', function(object){standardGeneric('intersectPlotX')})
setMethod(
  f = 'intersectPlotX',
  signature = 'SettleR',
  definition = function(object){
    return(plotList(object)$intersectPlotX)
  })
#' @export
#' @rdname accessors
setGeneric('intersectPlotX<-', function(object,value){standardGeneric('intersectPlotX<-')})
setReplaceMethod(
  f = 'intersectPlotX',
  signature = 'SettleR',
  definition = function(object, value){
    plotList(object)$intersectPlotX <- value
    plotList(object) <- merge_upset_list(plotList(object))
    return(object)
  })

### setPlotY ####
#' @export
#' @rdname accessors
setGeneric('setPlotY', function(object){standardGeneric('setPlotY')})
setMethod(
  f = 'setPlotY',
  signature = 'SettleR',
  definition = function(object){
    return(plotList(object)$setPlotY)
  })
#' @export
#' @rdname accessors
setGeneric('setPlotY<-', function(object,value){standardGeneric('setPlotY<-')})
setReplaceMethod(
  f = 'setPlotY',
  signature = 'SettleR',
  definition = function(object, value){
    plotList(object)$setPlotY <- value
    plotList(object) <- merge_upset_list(plotList(object))
    return(object)
  })

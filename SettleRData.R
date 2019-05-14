# Wrapper ####
SettleRData <- function(setList,
                        setLevels=NULL,
                        colMap=NULL,
                        intersectLevels=NULL,
                        nintersects=15){


  plotList <- sets_to_matrix(setlist) %>%
    calc_set_overlaps(.,
                      intersect_levels=intersect_levels,
                      set_levels=set_levels,
                      nintersects=nintersects )
  setDat <- new('SettleRData',
                setList=set_list,
                intersectLevels=intersect_levels,
                setLevels=set_levels,
                nintersects=nintersects)
}


# Class -----
setClass('SettleRData',
         slots=list(
           setList='list',
           plotList='list',
           setLevels='character',
           colMap='character',
           intersectLevels='character',
           nintersects='numeric'
         )
)

# Methods =============
## Slot Accessors ####
### setList ####
setGeneric('setList', function(object){standardGeneric('setList')})
setMethod(
  f = 'setList',
  signature = 'SettleRData',
  definition = function(object){
    return(object@setList)
  })
setGeneric('setList<-', function(object,value){standardGeneric('setList<-')})
setReplaceMethod(
  f = 'setList',
  signature = 'SettleRData',
  definition = function(object, value){
    object@setList <- value
    return(object)
  })
### setLevels ####
setGeneric('setLevels', function(object){standardGeneric('setLevels')})
setMethod(
  f = 'setLevels',
  signature = 'SettleRData',
  definition = function(object){
    return(object@setLevels)
  })
setGeneric('setLevels<-', function(object,value){standardGeneric('setLevels<-')})
setReplaceMethod(
  f = 'setLevels',
  signature = 'SettleRData',
  definition = function(object, value){
    object@setLevels <- value
    return(object)
  })
### colMap ####
setGeneric('colMap', function(object){standardGeneric('colMap')})
setMethod(
  f = 'colMap',
  signature = 'SettleRData',
  definition = function(object){
    return(object@colMap)
  })
setGeneric('colMap<-', function(object,value){standardGeneric('colMap<-')})
setReplaceMethod(
  f = 'colMap',
  signature = 'SettleRData',
  definition = function(object, value){
    object@colMap <- value
    return(object)
  })
### intersectLevels ####
setGeneric('intersectLevels', function(object){standardGeneric('intersectLevels')})
setMethod(
  f = 'intersectLevels',
  signature = 'SettleRData',
  definition = function(object){
    return(object@intersectLevels)
  })
setGeneric('intersectLevels<-', function(object,value){standardGeneric('intersectLevels<-')})
setReplaceMethod(
  f = 'intersectLevels',
  signature = 'SettleRData',
  definition = function(object, value){
    object@intersectLevels <- value
    return(object)
  })
### nintersects ####
setGeneric('nintersects', function(object){standardGeneric('nintersects')})
setMethod(
  f = 'nintersects',
  signature = 'SettleRData',
  definition = function(object){
    return(object@nintersects)
  })
setGeneric('nintersects<-', function(object,value){standardGeneric('nintersects<-')})
setReplaceMethod(
  f = 'nintersects',
  signature = 'SettleRData',
  definition = function(object, value){
    object@nintersects <- value
    return(object)
  })

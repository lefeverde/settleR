# Constructor
## SettleR because names are hard
SettleR <- function(setList,
                    setLevels=NULL,
                    colMap=NULL,
                    intersectLevels=NULL){
  binaryMat <- sets_to_matrix(setList)

  plotData <- binaryMat %>%
    calc_set_overlaps(.,
                      intersectLevels=intersectLevels,
                      setLevels=setLevels,
                      nintersects=nintersects
    )
  ## Kludge to get set, and intersect levels
  ## Don't want to modify function so the code below
  ## gets levels without regardless of whether they
  ## passed as arg or NULL and calculated on the fly
  intersectLevels <- levels(plotData$grid_data$intersect_id)
  setLevels <-  levels(pd$set_totals$set_names)


  settleRObj <- new('SettleR',
                    setList=setList,
                    binaryMat=binaryMat,
                    plotData=plotData,
                    plotList=NULL,
                    setLevels=setLevels,
                    colMap=colMap,
                    intersectLevels=intersectLevels,
                    nintersects='numeric')

}

# Class -----
setClass('SettleR',
         slots=list(
           setList='list',
           binaryMat='matrix',
           plotData='list',
           plotList='list',
           setLevels='character',
           colMap='character',
           intersectLevels='character',
           nintersects='numeric'
         )
)

## Validity function #####
check_SettleR <- function(object){
  errors <- character()

}


# Constructor
## SettleR because names are hard
SettleR <- function(setList,
                    setLevels=NULL,
                    colMap=NULL,
                    intersectLevels=NULL,
                    nintersects=NULL){
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
  setLevels <-  levels(plotData$set_totals$set_names)
  if(is.null(colMap)){
    colMap <- data.frame()
  }

  settleRObj <- new('SettleR',
                    setList=setList,
                    binaryMat=binaryMat,
                    # plotData=plotData,
                    grid_data=grid_data,
                    intersect_data=intersect_data,
                    set_totals=set_totals,
                    # plotList=NULL,
                    setLevels=setLevels,
                    colMap=colMap,
                    intersectLevels=intersectLevels,
                    nintersects=nintersects)

}

# Class -----
setClass('SettleR',
         slots=list(
           setList='list',
           binaryMat='matrix',
           cnt_df='data.frame',
           grid_data='data.frame',
           intersect_data='data.frame',
           set_totals='data.frame',
           # plotData='list',
           # plotList='list',
           setLevels='character',
           colMap='data.frame',
           intersectLevels='character',
           nintersects='numeric'
         )
)

## Validity function #####
check_SettleR <- function(object){
  errors <- character()

}


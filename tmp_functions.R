SettleR <- function(setList, intersectLevels=NULL, setLevels=NULL, nintersects=15, colMap=NULL){

  binary_mat <- sets_to_matrix(setList)
  cnt_df <- create_count_df(binary_mat)

  # Data for X-top margin plot
  intersect_data <-
    data.frame(intersect_set_size=cnt_df$freq,
               intersect_degree=rowSums(cnt_df[,seq(1, ncol(cnt_df) -1 )])) %>%
    rownames_to_column(., 'intersect_id')

  if(is.null(intersectLevels)){
    intersectLevels <-
      dplyr::arrange(intersect_data, desc(intersect_set_size)) %>%
      dplyr::slice(., seq(1, nintersects)) %>%
      pull(1)
  }

  # intersect_data$intersect_id <-
  #   factor(intersect_data$intersect_id,
  #          levels=intersect_levels)

  intersect_data <-
    dplyr::arrange(intersect_data, intersect_id) %>%
    cbind(x=seq(1, nrow(.)), .)


  # Data for Y-right margin plot
  set_totals <- data.frame(colSums(binary_mat)) %>%
    rownames_to_column(.) %>%
    setNames(., c('set_names', 'total_set_size'))
  if(is.null(set_levels)){
    set_levels <-
      dplyr::arrange(set_totals, total_set_size) %>%
      pull(1)
  }
  set_totals$set_names <- factor(set_totals$set_names,
                                 levels=set_levels)
  set_totals <-
    dplyr::arrange(set_totals, set_names) %>%
    cbind(y=seq(1, nrow(.)), .)

  grid_data <- cnt_df[,seq(1, ncol(cnt_df) -1)] %>%
    as.matrix(.) %>%
    reshape2::melt(.) %>%
    setNames(., c('intersect_id', 'set_names', 'observed'))
  # observed is basically a variable stating whether
  # the row combinations exists in the original data
  grid_data$observed <- as.logical(grid_data$observed)
  grid_data$set_names <- factor(grid_data$set_names,
                                levels=set_levels)
  # grid_data$intersect_id <- factor(grid_data$intersect_id,
  #                                  levels=intersect_levels)

  if(is.null(colMap)){
    colMap <- data.frame()
  }

  settleRObj <- new('SettleR',
                    setList=setList,
                    binaryMat=binaryMat,
                    cnt_df=cnt_df,
                    # plotData=plotData,
                    grid_data=grid_data,
                    intersect_data=intersect_data,
                    set_totals=set_totals,
                    # plotList=NULL,
                    setLevels=setLevels,
                    colMap=colMap,
                    intersectLevels=intersectLevels)
  return(settleRObj)

}

setClass('SettleR',
         slots=list(
           setList='list',
           binaryMat='matrix',
           cnt_df='data.frame',
           # setData='list',
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


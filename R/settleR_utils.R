#' Converts list of sets into a binary matrix
#'
#' This creates a matrix simliar to that in the UpSetR
#' package but I've removed a lot of the unneeded code.
#' I've also added a few helpful errors here.
#'
#' @param setlist list of sets
#'
#' @return binary matrix
#' @export
#'
#' @examples
sets_to_matrix <- function(setlist){
  ## Errors
  if(class(setlist) != 'list'){
    e_message <- paste0('setlist needs to be a list of sets, not ',
                        class(setlist))
    stop(e_message)
  }

  if(! all(sapply(setlist, is.vector, simplify = TRUE))){
    stop('all items in setlist need to be vectors')
  }
  ## Code, adapted from UpSetR
  elements <- unlist(setlist) %>%
    unique(.)
  binary_mat <- lapply(setlist, function(x){
    x <- as.vector(match(elements, x))
    x[is.na(x)] <- 0
    return(x)
  }) %>% do.call(cbind, .)
  binary_mat <-  ifelse(binary_mat >0,
                        as.integer(1),
                        as.integer(0)
                      )
  row.names(binary_mat) <- elements
  return(binary_mat)


}

#' Creates df of upset data
#'
#' This formats the resulting data to long(ish)
#' formatted data. The reason why I've tried
#' put everything in 1 data.frame is so that
#' I can set the factors once in a single object.
#' It's currently adding more work to the uspset
#' wrapper since I have to essentially split this
#' into seperate objects, but I think it's worth
#' it so that I don't have to always think about the factors.
#'
#'
#'
#' @param binary_mat
#' @param intersect_levels
#' @param set_levels
#'
#' @return
#' @export
#'
#' @examples
calc_set_overlaps <- function(binary_mat, intersect_levels=NULL, set_levels=NULL,nintersects=100){

  cnt_df <- plyr::count(binary_mat) %>%
    arrange(., desc(freq)) %>%
    dplyr::slice(., 1:nintersects)
  row.names(cnt_df) <- paste0('intersect_', seq(1,nrow(cnt_df)))

  colnames(cnt_df) <-  gsub('x\\.','', colnames(cnt_df))

  # Data for X-top margin plot
  intersect_data <- data.frame(intersect_set_size=cnt_df$freq,
                               intersect_degree=rowSums(cnt_df[,seq(1, ncol(cnt_df) -1 )])) %>%
    rownames_to_column(., 'intersect_id')
  if(is.null(intersect_levels)){
    intersect_levels <-
      dplyr::arrange(intersect_data, desc(intersect_set_size)) %>%
      pull(1)
  }

  intersect_data$intersect_id <- factor(intersect_data$intersect_id,
                                        levels=intersect_levels)
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
    reshape2::melt() %>%
    setNames(., c('intersect_id', 'set_names', 'observed'))
  # observed is basically a variable stating whether
  # the row combinations exists in the original data
  grid_data$observed <- as.logical(grid_data$observed)
  grid_data$set_names <- factor(grid_data$set_names,
                                levels=set_levels)
  grid_data$intersect_id <- factor(grid_data$intersect_id,
                                   levels=intersect_levels)

  ol <- list(grid_data, intersect_data, set_totals) %>%
    setNames(., c('grid_data', 'intersect_data', 'set_totals'))
  return(ol)

  # Merging DFs into longish df
  # out_df <- merge(grid_data, intersect_data,
  #                 all = T,
  #                 by = 'intersect_id')
  # out_df <-  merge(set_totals, out_df, by='set_names')
  # return(out_df)


}

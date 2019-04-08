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
#'
#' @return
#' @export
#'
#' @examples
calc_set_overlaps <- function(binary_mat){

  cnt_df <- plyr::count(binary_mat)
  row.names(cnt_df) <- paste0('intersect_', seq(1,nrow(cnt_df)))

  colnames(cnt_df) <-  gsub('x\\.','', colnames(cnt_df))

  intersect_data <- data.frame(intersect_set_size=cnt_df$freq,
                               intersect_degree=rowSums(cnt_df[,seq(1, ncol(cnt_df) -1 )])) %>%
    rownames_to_column(., 'intersect_id')

  set_totals <- data.frame(colSums(binary_mat)) %>%
    rownames_to_column(.) %>%
    setNames(., c('set_names', 'total_set_size'))

  grid_data <- cnt_df[,seq(1, ncol(cnt_df) -1)] %>%
    as.matrix(.) %>%
    reshape2::melt() %>%
    setNames(., c('intersect_id', 'set_names', 'observed'))
  # observed is basically a variable stating whether
  # the row combinations exists in the original data
  grid_data$observed <- as.logical(grid_data$observed)

  # ol <- list(grid_data, intersect_data, set_totals) %>%
  #   setNames(., c('grid_data', 'intersect_data', 'set_totals'))
  # return(ol)

  # Merging DFs into longish df
  out_df <- merge(grid_data, intersect_data,
                  all = T,
                  by = 'intersect_id')
  out_df <-  merge(set_totals, out_df, by='set_names')
  return(out_df)


}

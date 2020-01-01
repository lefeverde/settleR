#### File for set opertation functions ####
# This file contains the miscelanous fucntions used for
# SettleR as well others with less intergration.
# TODO revisit the organization


#' Converts list of sets into a binary matrix
#'
#' This creates a matrix simliar to that in the UpSetR
#' package but I've removed a lot of the unneeded code.
#' I've also added a few helpful errors here.
#'
#' @param setList list of sets
#'
#' @return binary matrix
#' @export
#'
#' @examples
sets_to_matrix <- function(setList){
  ## Errors
  if(class(setList) != 'list'){
    e_message <- paste0('setList needs to be a list of sets, not ',
                        class(setList))
    stop(e_message)
  }

  if(! all(sapply(setList, is.vector, simplify = TRUE))){
    stop('all items in setList need to be vectors')
  }
  ## Code, adapted from UpSetR
  elements <- unlist(setList) %>%
    unique(.)
  binary_mat <- lapply(setList, function(x){
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

#' Creates a data.frame with the counts per set
#'
#' Takes a binary matrix, for example the output from \link[settleR]{sets_to_matrix}
#' or similiar and returns a data.frame with the counts (or frequency) of the
#' set for each of the groups.
#'
#' @param binary_mat binary matrix (see \link[settleR]{sets_to_matrix})
#'
#' @return a data.frame
#' @export
#'
#' @examples
create_count_df <- function(binary_mat){
  cnms <- colnames(binary_mat)
  cnt_df <- plyr::count(binary_mat) %>%
    arrange(., desc(freq))
  cnt_df <- cnt_df[seq_len(nrow(cnt_df)),]
  row.names(cnt_df) <- paste0('intersect_', seq(1,nrow(cnt_df)))
  colnames(cnt_df)[1:(ncol(cnt_df) - 1)] <- cnms

  return(cnt_df)
}


#### Functions for testing #####

#' loads data for testing
#'
#' @return
#' @keywords internal
#'
#' @examples
get_example_data <- function(){
  rds_to_load <-
    c('ex_col_map.rds',
      'ex_gene_setlist.rds',
      'ex_set_levels.rds',
      'expected_gridData.rds',
      'expected_intersectData.rds',
      'expected_setTotals.rds') %>%
    lapply(., function(x){
      system.file('extdata',
                  x,
                  package = 'settleR',
                  mustWork = TRUE)
    })
}





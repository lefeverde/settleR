library(settleR)
context('Testing functions from settleR_utils.R')

set1 <- list(upreg=paste0('gene', seq(1, 10)))

create_biset_data <- function(n_genes=100, pos_frac=.25, neg_frac=NULL, seed=42){
  set.seed(seed)
  genes <- paste0('gene', seq(1, n_genes))
  if(is.null(neg_frac)){
    neg_frac <- pos_frac
  }
  pos_overlap <-
    sample(genes, pos_frac*n_genes) %>%
    split(., 1:2)
  neg_overlap <-
    genes[!genes %in% pos_overlap] %>%
    sample(., neg_frac*n_genes) %>%
    split(., 1:2)

  non_overlap_genes <-
    genes[!genes %in% c(unlist(pos_overlap), unlist(neg_overlap))]


  set1_non_overlap <-
    non_overlap_genes[1:ceiling(length(non_overlap_genes)/2)] %>%
    split(., 1:2)
  set2_non_overlap <-
    non_overlap_genes[!non_overlap_genes %in% set1_non_overlap] %>%
    split(., 1:2)

  s1_upreg <-
    c(set1_non_overlap[[1]],
      pos_overlap[[1]],
      neg_overlap[[1]])
  s1_downreg <-
    c(set1_non_overlap[[2]],
      pos_overlap[[2]],
      neg_overlap[[2]])

  s2_upreg <-
    c(set2_non_overlap[[1]],
      pos_overlap[[1]],
      neg_overlap[[2]])
  s2_downreg <-
    c(set2_non_overlap[[2]],
      pos_overlap[[2]],
      neg_overlap[[1]])

  biset1 <-
    list(s1_upreg, s1_downreg) %>%
    setNames(., c('upreg', 'downreg'))
  biset2 <-
    list(s2_upreg, s2_downreg) %>%
    setNames(., c('upreg', 'downreg'))

  ol <- list(biset1, biset2)
  return(ol)


}



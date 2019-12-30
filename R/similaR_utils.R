calc_biset_overlap <- function(biset1, biset2){
  max_len <-
    max(length(unlist(biset1)),length(unlist(biset2)))
  pos_overlap <-
    c(intersect(biset1[[1]], biset2[[1]]),
      intersect(biset1[[2]], biset2[[2]]))


  neg_overlap <-
    c(intersect(biset1[[1]], biset2[[2]]),
      intersect(biset1[[2]], biset2[[1]]))

  out_df <- tibble(pos_overlap=length(pos_overlap)/max_len,
                   neg_overlap=length(neg_overlap)/max_len)
  return(out_df)
}

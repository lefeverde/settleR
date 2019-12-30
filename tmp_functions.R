#TODO Accessor fucntions for plots
#TODO Validity checking for levels
#TODO Check validity of ggplots
check_validity <- function(settleRObject){
  plt_list <- plotList(settleRObject)

}

get_exlcusive_items <- function(setList, coefs_to_keep){
  overlap_set <- setList[coefs_to_keep] %>%
    reduce(.,  .f = intersect)

  items_to_remove <-
    setList[!names(setList) %in% coefs_to_keep] %>%
    unlist(.) %>%
    unique(.)

  overlap_set <-
    overlap_set[!overlap_set %in% items_to_remove]
  return(overlap_set)
}

get_reordered_intersect_lvls <- function(SettleR_Object){

  original_intersects <- SettleR_Object@intersectLevels
    # SettleR_Object@intersectData %>%
    # filter(intersect_id %in% SettleR_Object@intersectLevels) %>%
    # pull(2) %>%
    # as.character(.)

  all_gd <- SettleR_Object@gridData %>%
    filter(., observed) %>%
    filter(intersect_id %in% original_intersects)

  singleton_intersects <- all_gd %>%
    group_by(., intersect_id) %>%
    summarise(., deg=n()) %>%
    filter(., deg==1)
  singleton_intersects$set_names <-
    all_gd$set_names[match(singleton_intersects$intersect_id,all_gd$intersect_id)]
  singleton_intersects <- singleton_intersects %>%
    arrange(., set_names) %>%
    pull(1) %>%
    as.character(.)

  original_intersects <-
    original_intersects[!original_intersects %in% singleton_intersects]
  intersect_lvls <- c(singleton_intersects, original_intersects)
  return(intersect_lvls)

}

get_redbox_dims <- function(SettleR_Object,box_pad=.5){


  plt_build <- SettleR_Object@plotList$gridPlot %>%
    ggplot_build(.)


  coord_pos <-  plt_build[['data']][[2]] %>%
    group_by(., group) %>%
    summarise(
      xmin=min(x) - box_pad,
      xmax=max(x) + box_pad,
      ymin=min(y) - box_pad,
      ymax=max(y) + box_pad
    )
  return(coord_pos)
}



get_coefs_as_list <- function(SettleR_Object){
  grid_data <-
    SettleR_Object@gridData %>%
    filter(intersect_id %in% SettleR_Object@intersectLevels) %>%
    droplevels
  coefs_list <-
    grid_data %>%
    filter(., observed) %>%
    split(., .$intersect_id) %>%
    lapply(., function(x){
      x$set_names %>%
        droplevels(.) %>%
        as.character(.)
    })
  return(coefs_list)
}

library(settleR)
context('Testing functions from settleR_utils.R')

get_example_data <- function(){
  rds_to_load <-
    c('ex_gene_setList.rds',
      'ex_col_map.rds',
      'ex_set_levels.rds') %>%
    lapply(., function(x){
      system.file('extdata',
                  x,
                  package = 'settleR',
                  mustWork = TRUE)
    })
}

setList <- list(
  S1=LETTERS[1:5],
  S2=LETTERS[3:5],
  S3=LETTERS[1:10]
)

setList2 <- list(
  S1=LETTERS[1:5],
  S2=LETTERS[3:5],
  S3=LETTERS[1:10],
  S4=LETTERS[3:10]
)


test_that('sets_to_matrix gives errors on incorrect input', {
  expect_error(sets_to_matrix(data.frame()),
  "setList needs to be a list of sets, not data.frame")
  expect_error(sets_to_matrix(list(data.frame())),
               "all items in setList need to be vectors")

})
test_that('calc_set_overlaps correctly creates the list of data.frames', {
  intersect_lvls <- paste0('intersect_', seq(1,3))
  set_lvls <- c("S2","S1","S3")

  e_grid_data <- data.frame(
    intersect_id=factor(intersect_lvls,
                        levels = intersect_lvls),
    set_names=factor(rep(c('S1', 'S2', 'S3'), each=3),
                     levels = set_lvls),
    observed=c(FALSE,
               TRUE,
               TRUE,
               FALSE,
               TRUE,
               FALSE,
               TRUE,
               TRUE,
               TRUE)
  )

  e_intersect_data <- data.frame(
    x=seq(1,3),
    intersect_id=factor(intersect_lvls,
                        levels = intersect_lvls),
    intersect_set_size=c(5, 3, 2),
    intersect_degree=c(1, 3, 2)
  )
  e_set_totals <- data.frame(
    y=seq(1,3),
    set_names=factor(set_lvls,
                     set_lvls),
    total_set_size=c(3, 5, 10)
  )
  res_list <- sets_to_matrix(setList) %>%
    calc_set_overlaps(.)
  expect_equal(res_list$grid_data, e_grid_data)
  expect_equal(res_list$intersect_data, e_intersect_data)
  expect_equal(res_list$set_totals, e_set_totals)


})

test_that('get_reordered_intersect_lvls produces correctly ordered intersects',{
  rds_to_load <- get_example_data()

  gene_setList <- readRDS(rds_to_load[[1]])
  col_map <- readRDS(rds_to_load[[2]])
  set_levels <- readRDS(rds_to_load[[3]])


  e_lvls <- c("intersect_13",
              "intersect_2",
              "intersect_11",
              "intersect_14",
              "intersect_9",
              "intersect_10",
              "intersect_3",
              "intersect_1",
              "intersect_4",
              "intersect_5",
              "intersect_6",
              "intersect_7",
              "intersect_8",
              "intersect_12",
              "intersect_15"
  )
  o_lvls <- get_reordered_intersect_lvls(gene_setList, set_levels)
  expect_equal(e_lvls, o_lvls)



})


test_that('box_intercepts_dims returns bounding box over the specified intersect_levels',{
  rds_to_load <- get_example_data()

  gene_setList <- readRDS(rds_to_load[[1]])
  col_map <- readRDS(rds_to_load[[2]])
  set_levels <- readRDS(rds_to_load[[3]])

  intersect_levels <- get_reordered_intersect_lvls(gene_setList, set_levels)
  upset_list <- sets_to_matrix(gene_setList) %>%
    calc_set_overlaps(.,
                      intersectLevels = intersect_levels,
                      setLevels = set_levels)
  ul <- make_upset_plots(upset_list, col_map)

  box_dims <- box_intercepts_dims(ul$gridPlot, intersects_to_box =  'intersect_14')
  e_box_dims <-
    matrix(data = c('intersect_14', 4, 3.625, 4.375, 4.625, 5.375), nrow = 1, ncol = 6) %>%
    data.frame(., stringsAsFactors = FALSE) %>%
    setNames(., c("intersect_id","group","xmin","xmax","ymin","ymax"))

  row.names(e_box_dims) <- 4
  e_box_dims[,2:ncol(e_box_dims)] <-  lapply(e_box_dims[,2:ncol(e_box_dims)], as.numeric)
  e_box_dims$group <- as.integer(e_box_dims$group)
  expect_true(all_equal(box_dims, e_box_dims))

})

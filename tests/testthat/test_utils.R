library(settleR)
context('Testing functions from settleR_utils.R')



setlist <- list(
  S1=LETTERS[1:5],
  S2=LETTERS[3:5],
  S3=LETTERS[1:10]
)

setlist2 <- list(
  S1=LETTERS[1:5],
  S2=LETTERS[3:5],
  S3=LETTERS[1:10],
  S4=LETTERS[3:10]
)
test_that('sets_to_matrix gives errors on incorrect input', {
  expect_error(sets_to_matrix(data.frame()),
  "setlist needs to be a list of sets, not data.frame")
  expect_error(sets_to_matrix(list(data.frame())),
               "all items in setlist need to be vectors")

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
  res_list <- sets_to_matrix(setlist) %>%
    calc_set_overlaps(.)
  expect_equal(res_list$grid_data, e_grid_data)
  expect_equal(res_list$intersect_data, e_intersect_data)
  expect_equal(res_list$set_totals, e_set_totals)


})

test_that('get_reordered_intersect_lvls produces correctly ordered intersects',{
  rds_to_load <-
    c('ex_gene_setlist.rds',
      'ex_col_map.rds',
      'ex_set_levels.rds') %>%
    lapply(., function(x){
      system.file('extdata',
                  x,
                  package = 'settleR',
                  mustWork = TRUE)
    })

  gene_setlist <- readRDS(rds_to_load[[1]])
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
  o_lvls <- get_reordered_intersect_lvls(gene_setlist, set_levels)
  expect_equal(e_lvls, o_lvls)



})



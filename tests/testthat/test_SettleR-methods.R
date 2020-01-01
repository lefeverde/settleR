library(settleR)
context('Testing functions from SettleR-methods')

test_data <-
  system.file('extdata','test_data.RData',package = 'settleR',mustWork = TRUE)
load(test_data)

test_that('reorder_by_singletons produces correctly ordered intersects',{
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
  tmp_setobj <-
    reorder_by_singletons(settler_object)
  expect_equal(e_lvls, intersectLevels(tmp_setobj))

})
# TODO test make_settleR_plots
# TODO test settleR_save

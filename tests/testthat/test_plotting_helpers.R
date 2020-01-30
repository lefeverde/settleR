test_data <-
  system.file('extdata','test_data.RData',package = 'settleR',mustWork = TRUE)
load(test_data)

test_that('box_intercepts_dims returns bounding box over the specified intersect_levels',{

  ul <- plotList(settler_object)

  box_dims <- box_intercepts_dims(ul$gridPlot, intersects_to_box =  'intersect_14')
  row.names(box_dims) <- NULL
  e_box_dims <-
    matrix(data = c("intersect_14", 14, 13.4875, 14.5125, 4.4875, 5.5125), nrow = 1, ncol = 6) %>%
    data.frame(., stringsAsFactors = FALSE) %>%
    setNames(., c("intersect_id","group","xmin","xmax","ymin","ymax"))

  row.names(e_box_dims) <- NULL
  e_box_dims[,2:ncol(e_box_dims)] <-  lapply(e_box_dims[,2:ncol(e_box_dims)], as.numeric)
  e_box_dims$group <- as.integer(e_box_dims$group)
  expect_true(all_equal(box_dims, e_box_dims))

})

library(settleR)
context('Basic testing of SettleR class')
test_data <-
  system.file('extdata','test_data.RData',package = 'settleR',mustWork = TRUE)
load(test_data)

test_that('SettleR Constructor creates correct type of object',{
  #TODO write test for checking that object matches
  # some pre-defined settleR object
  tmp_so <- SettleR(gene_setList,
                            setLevels = set_levels,
                            colMap = col_map)
  expect_equal(class(tmp_so)[1], 'SettleR')
})

test_that('Accessor functions work access',{

  expect_equal(gridData(settler_object), expected_gridData)
  expect_equal(intersectData(settler_object), expected_intersectData)
  expect_equal(setTotals(settler_object), expected_setTotals)

})


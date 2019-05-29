library(settleR)
context('Basic testing of SettleR class')
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




rds_to_load <- get_example_data()

col_map <- readRDS(rds_to_load[[1]])
gene_setList <- readRDS(rds_to_load[[2]])
set_levels <- readRDS(rds_to_load[[3]])
expected_gridData <- readRDS(rds_to_load[[4]])
expected_intersectData <- readRDS(rds_to_load[[5]])
expected_setTotals <- readRDS(rds_to_load[[6]])


test_that('SettleR Constructor creates correct type of object',{
  #TODO write test for checking that object matches
  # some pre-defined settleR object
  settler_object <- SettleR(gene_setList,
                            setLevels = set_levels,
                            colMap = col_map)
  expect_equal(class(settler_object)[1], 'SettleR')
})

test_that('Accessor functions work access',{
  settler_object <- SettleR(gene_setList,
                            setLevels = set_levels,
                            colMap = col_map)
  expect_equal(gridData(settler_object), expected_gridData)
  expect_equal(intersectData(settler_object), expected_intersectData)
  expect_equal(setTotals(settler_object), expected_setTotals)

})






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

context('names_are_unique_and_non_empty')

test_that('it correctly checks if names are unique and non-empty for a length 0 object', {
  expect_true(names_are_unique_and_non_empty(c()))
  expect_true(names_are_unique_and_non_empty(list()))
})

test_that('it correctly checks if names are unique and non-empty for a non-length 0 object', {
  expect_true(names_are_unique_and_non_empty(c(a = 1)))
  expect_true(names_are_unique_and_non_empty(list(b = 2)))
})

test_that('it correctly asserts no names were supplied', {
  expect_false(names_are_unique_and_non_empty(c(1)))
  expect_false(names_are_unique_and_non_empty(list(2)))
})




#' stopifnot(names_are_unique_and_non_empty(c()))
#' stopifnot(names_are_unique_and_non_empty(list(a = 1, b = 2))
#' stopifnot(!names_are_unique_and_non_empty(c("")))
#' stopifnot(!names_are_unique_and_non_empty(list(a = 1, a = 2))

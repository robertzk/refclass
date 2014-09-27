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
  expect_error(names_are_unique_and_non_empty(list(2), error = TRUE), "names supplied")
})

test_that('it correctly asserts some names are empty', {
  expect_error(names_are_unique_and_non_empty(list(2, b = 1), error = TRUE),
                "names must be nonempty")
})

test_that('it correctly asserts some names are duplicated', {
  expect_false(names_are_unique_and_non_empty(c(a = 1, a = 2)))
  expect_error(names_are_unique_and_non_empty(list(b = 2, b = 1), error = TRUE),
               "names must be distinct")
})

test_that('it correctly uses the what parameter', {
  expect_error(names_are_unique_and_non_empty(list(1), 'field', error = TRUE),
               'field names')
})


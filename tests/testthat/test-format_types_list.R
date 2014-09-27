context('format_types_list')

#' stopifnot(identical(format_types_list(c(a = 'character', b = 'data.frame')),
#'   list(a = 'character', b = 'data.frame')))
#' stopifnot(identical(format_types_list(c('a', 'b')), list(a = 'ANY', b = 'ANY')))
#' stopifnot(identical(format_types_list(list(a = 'character')), list(a = 'character')))
#' # The following will error because of duplicate names:

test_that('it correctly formats a simple character example', {
  expect_identical(format_types_list(c(a = 'character', b = 'data.frame')),
    list(a = 'character', b = 'data.frame'))
})

test_that('it correctly formats an unnamed character example', {
  expect_identical(format_types_list(c('a', 'b')), list(a = 'ANY', b = 'ANY'))
})

test_that('it correctly formats a list example', {
  expect_identical(format_types_list(list(a = 'character')), list(a = 'character'))
})

test_that('it errors when a duplicate name is given', {
  expect_error(format_types_list(list(a = 'character', a = 'character')), 'distinct')
})

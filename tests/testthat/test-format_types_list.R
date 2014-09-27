context('format_types_list')

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

test_that('it errors when a non-list non-character argument is provided', {
  expect_error(format_types_list(5), 'must be a list or')
})

test_that('the what parameter works as expected', {
  expect_error(format_types_list(5, 'boops'), paste(dQuote('boops'), 'must be a list or'))
})

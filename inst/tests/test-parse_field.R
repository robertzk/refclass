context('parse_field')

test_that('it errors when a non-1 length field value gets passed', {
  expect_error(parse_field('foo', c('a','b')), "single class name is needed")
})

test_that('it errors when a non-existent class gets passed', {
  expect_error(parse_field('foo','bar', environment()),  "Class .* for field .* is not defined")
})

test_that('it creates an unitializedField prototype for ANY field classes', {
  expect_equal(as.character(class(parse_field('foo', 'ANY')[[2]])), 'uninitializedField') 
})

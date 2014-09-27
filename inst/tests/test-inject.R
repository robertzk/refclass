context('inject')
library(testthatsomemore)

test_that('it correctly injects example values into the local scope', {
  inject(list(a = 1, b = 2))
  assert(a); assert(b)
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that('it errors when we do not provide a list', {
  expect_error(inject(5), 'must be a list')
})

test_that('it errors when an unnamed list is given', {
  expect_error(inject(list(5)), 'must be a named list')
})

test_that('it errors when a list with duplicate or empty names is given', {
  expect_error(inject(list(a = 1, 2)))
  expect_error(inject(list(a = 1, a = 2)))
})

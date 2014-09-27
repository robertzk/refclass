context('get_superclasses_information')

test_that('it returns the trivial stuff on an empty list', {
  expect_identical(get_superclasses_information(list(), environment()),
                   list(superClasses = character(0), isRefSuperClass = logical(0)))
})

test_that('it gives an error when the contains argument is not convertible to classes', {
  expect_error(get_superclasses_information(5), 'should be the names of superclasses')
})

test_that('it errors when super class names are missing definitions', {
  expect_error(get_superclasses_information('blah', environment()), 'is not a defined class')
})

test_that('it can get the superclass information for a simple example', {
  setClass('test__a')
  methods::setRefClass('test__b')
  expect_identical(get_superclasses_information(c('test__a', 'test__b'), environment()),
                   list(superClasses = c('test__a', 'test__b'), isRefSuperClass = c(FALSE, TRUE)))
})



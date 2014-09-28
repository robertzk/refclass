context('get_superclasses_information')

declassify <-
  function(x) { x[[1]] <- sapply(x[[1]], function(y) as.character(y@className)); x }

test_that('it returns the trivial stuff on an empty list', {
  expect_identical(declassify(get_superclasses_information(list(), environment())),
                   list(superClassDefs = c(envRefClass = 'envRefClass'),
                        superClasses = c(envRefClass = 'envRefClass'), isRefSuperClass = TRUE))
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
  expect_identical(
    declassify(get_superclasses_information(c('test__a', 'test__b'), environment())),
     list(superClassDefs = c('test__a', 'test__b'),
          superClasses = c('test__a', 'test__b'),
          isRefSuperClass = c(FALSE, TRUE))
  )

  removeClass('test__a')
  removeClass('test__b')
})



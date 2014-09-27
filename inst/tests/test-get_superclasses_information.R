context('get_superclasses_information')

test_that('it returns the trivial stuff on an empty list', {
  expect_identical(get_superclasses_information(list(), environment()),
                   list(superClasses = character(0), isRefSuperClass = logical(0)))
})


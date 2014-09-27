context('names_are_unique_and_non_empty')

test_that('it correctly checks if names are unique and non-empty for a length 0 object', {

})

#' stopifnot(names_are_unique_and_non_empty(c()))
#' stopifnot(names_are_unique_and_non_empty(list(a = 1, b = 2))
#' stopifnot(!names_are_unique_and_non_empty(c("")))
#' stopifnot(!names_are_unique_and_non_empty(list(a = 1, a = 2))

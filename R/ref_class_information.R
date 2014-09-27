#' Fetch all necessary information to construct a reference class generator function.
#'
#' @rdname setRefClass
#' @param refMethods list. A named list of reference class methods.
ref_class_information <- function(Class, contains, fields, refMethods, where) {
  inject(get_superclasses_information(contains, where))

}

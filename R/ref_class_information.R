#' Fetch all necessary information to construct a reference class generator function.
#'
#' @rdname setRefClass
#' @param refMethods list. A named list of reference class methods.
ref_class_information <- function(Class, contains, fields, refMethods, where) {
  superclasses_information <- get_superclasses_information(contains, where)  
  for (i in superclasses_information) assign(i, superclasses_information[[i]])
}

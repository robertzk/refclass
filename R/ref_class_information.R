#' Fetch all necessary information to construct a reference class generator function.
#'
#' @rdname setRefClass
#' @param refMethods list. A named list of reference class methods.
ref_class_information <- function(Class, contains, fields, refMethods, where) {
  inject(get_superclasses_information(contains, where))

}

#' Get superclass information.
#'
#' Provide a list containing \code{superClasses} (the actual class names)
#' and \code{isRefSuperClass}, a vector of logicals indicating
#' whether each superclass is a reference class.
#'
#' @param contains character. A character vector of super class names.
#' @param where environment. The environment in which to look for superClasses.
#'  # TODO: (RK) Provide ability to look for super classes in multiple environments?
#' @return a list with keys \code{superClasses} and \code{isRefSuperClass} 
#'  indicating a character vector of super class names and whether
#'  each is a defined reference class or not.
get_superclasses_information <- function(contains, where) {
}


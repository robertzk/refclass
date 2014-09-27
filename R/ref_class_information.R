#' Fetch all necessary information to construct a reference class generator function.
#'
#' @rdname setRefClass
#' @param refMethods list. A named list of reference class methods.
ref_class_information <- function(Class, contains, fields, refMethods, where) {
  inject(get_superclasses_information(contains, where))

}

#' Get superclass information.
#'
#' Provide a list containing \code{superClasses}, (the actual class names)
#' \code{isRefSuperClass}, a vector of logicals indicating
#' whether each superclass is a reference class,
#' and \code{superClassDefs}, containing the actual superclass
#' definitions.
#'
#' @param contains character. A character vector of super class names.
#' @param where environment. The environment in which to look for superClasses.
#'  # TODO: (RK) Provide ability to look for super classes in multiple environments?
#' @return a list with keys \code{superClasses}, \code{isRefSuperClass} 
#'  indicating a character vector of super class names, whether
#'  each is a defined reference class or not, and the super class definitions,
#'  respectively.
get_superclasses_information <- function(contains, where) {
  superclass_definitions <- lapply(contains, function(what) {
    if (is(what, 'classRepresentation')) what
    else if (is.character(what)) getClass(what, where = where)
    else stop(gettextf("the 'contains' argument should be the names of superclasses: got an element of class %s",
                       dQuote(class(what)[1])), domain = NA, call. = FALSE)
  })

  missing_definitions <- vapply(superclass_definitions, is.null, logical(1))
  if (any(missing_definitions))
    stop(gettextf("no definition found for inherited class: %s",
                  paste0('"',contains[missingDefs], '"', collapse = ", ")),
         domain = NA, call. = FALSE)

  list(superClassDefs = superclass_definitions,
       superClasses = vapply(superclass_definitions, function(def) def@className, character(1)),
       isRefSuperClass = vapply(superclass_definitions,
                                function(def) is(def, 'refClassRepresentation'),
                                logical(1)))
}


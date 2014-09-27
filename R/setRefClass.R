#' Create a new reference class.
#'
#' Reference class objects are the closest approximation of the concepts of
#' object-oriented programming available in R. While S3 and S4 classes serve
#' the useful purpose of providing dispatching mechanisms on generic functions,
#' reference classes allow for mutability of data on the underlying object
#' regardless of calling convention (or nasty syntax like replace methods).
#'
#' # TODO: (RK) More description here.
#'
#' @param Class character. The name of the newly created reference class.
#' @param fields character. The field for the reference class. Fields are
#'   equivalent to "members" in other languages, and can represent any data.
#'   In this particular case, the convention is something like
#'   \code{list(id = "numeric", data = "data.frame")} to specify two fields,
#'   \code{id} and \code{data} with required types \code{numeric} and 
#'   \code{data.frame}. The default is \code{character()}.
#' @param contains character. The parent reference classes of this reference
#'   class. The default is \code{character()}. # TODO: (RK) Document this behavior better.
#' @param methods list. The methods for instances of this reference class.
#'   In particular, the names of this list represent the names of the methods,
#'   and the values must be functions. For example,
#'    \code{list(foo = function(x) print(x)} would define a method \code{foo}
#'   that we could call with \code{reference_class_object$foo(...)}.
#'   The default is to default is to define no methods (\code{list()}).
#' @param where environment. Where to store the definition of the reference
#'   class. The default is \code{topenv(parent.frame())}.
#' @param ... additional arguments to the \code{setClass} function call
#'   that gets called under the hood. (Note that a reference class generator
#'   is an extension of a classGeneratorFunction in the S3 sense.)
#' @return the fully formed reference class object generator. For example,
#'   if we call \code{x <- setRefClass(...)}, then we can call \code{x$new(...)}
#'   to invoke the constructor for the object \code{x}. The class of the
#'   reference class object generator is \code{"refObjectGenerator"}.
#' # TODO: (RK) More documentation and examples.
setRefClass <- function(Class, fields = character(), contains = character(),
                        methods = list(), where = topenv(parent.frame()), ...) {
  fields <- format_types_list(fields, 'field')
  method_names <- names(methods)

  class_info <- ref_class_information(Class, contains, fields, methods, where)

  # Make codetools happy.
  requires <- c("superClasses", "refSuperClasses", "fieldClasses", "fieldPrototypes", "refMethods")
  for (i in requires) assign(i, class_info[[i]])

  # Get the classGeneratorFunction from setClass
  class_function <- setClass(Class, contains = superClasses, where = where, ...)
  
  class_definition <- new("refClassRepresentation",
    getClassDef(Class, where = where), fieldClasses = fieldClasses,
    refMethods = to_env(refMethods), fieldPrototypes = to_env(fieldPrototypes),
    refSuperClasses = refSuperClasses)

  assignClassDef(Class, classDef, where)

  generator <- new("refGeneratorSlot")
  env <- as.environment(generator)
  env$def <- class_definition; env$className <- Class

  declare_variables(class_definition, where)
  invisible(new("refObjectGenerator", class_function, generator = generator))
}

# TODO: (RK) Implement.
ref_class_information <- function(...) { }
declare_variables <- function(...) { }


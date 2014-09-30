#' Create a default binding (prototype) for a given field in a given environment.
#'
#' Before fields are initialized in reference classes, they should be initialized
#' to some value. If this is not done in the constructor, it is the responsibility
#' of this function to give a default value.
#'
#' @param field_name character. The name of the field.
#' @param field_class character. The class of the field.
#' @param read_only logical. Whether or not this field should be read-only.
#'    The default is \code{FALSE}.
#' @param where environment. The environment in which to create the field binding.
#' @return a list consisting of a \code{defaultBindingFunction} and a
#'    \code{uninitializedField} if the \code{field_class} refers to a virtual
#'    class or \code{new(field_class)} otherwise.
make_default_binding <- function(field_name, field_class, read_only = FALSE, where) {
  meta_name <- paste0(".->", field_name)
}

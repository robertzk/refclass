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

  f <- eval(substitute(function(value) {
    if (missing(value)) dummy_field_name
    else refclass:::set_dummy_field(.self, dummy_field, dummy_class, this_field, is_read_only, value)
  }, list(dummy_field = meta_name, this_field = field_name,
          dummy_class = field_class, dummy_field_name = as.name(meta_name),
          is_read_only = isTRUE(read_only))
  ))

  environment(f) <- where
  f <- new("defaultBindingFunction", f, field = field_name, className = field_class)
  init <-
    if (isVirtualClass(field_class))
      new("uninitializedField", field = field_name, className = field_class)
    else new(field_class)

  setNames(list(f, init), c(field_name, meta_name))
}

#' Make an active binding for a field.
#'
#' #TODO: (RK) Explain activeBindingFunction more
#' 
#' @param field activeBindingFunction or character.
#' @return \code{activeBindingFunction}
make_active_binding <- function(field) {
  if (is(field, "activeBindingFunction")) field
  else new("activeBindingFunction", field)
}


#' Provide a list of class methods.
#'
#' @param class_methods list. A named list of functions corresponding to
#'   methods inherited from the inheritance chain.
#' @param class_name character. The name of the class these methods will
#'   belong to.
#' @param instance_methods list. Additional methods provided by the
#'   \code{methods} parameter to \code{setRefClass}.
#' @param field_names character. The names of the fields in the class.
#'   Used to verify fields are not getting incorrectly assigned with
#'   local (\code{<-}) instead of global (\code{<<-}) assignment.
#' @param return_all logical. Whether or not to return all methods.
#'   # TODO: (RK) What does this mean?
#' @return a named list.
generate_class_methods <- function(class_methods, class_name,
  instance_methods, field_names, return_all) {
  new_methods  <- names(instance_methods)
  prev_methods <- names(class_methods) 
  all_methods <- unique(c(new_methods, prev_methods))
  return_methods <- if (return_all) class_methods else instance_methods

  check <- TRUE
  for (method in new_methods) {
    prev_method <- class_methods[[method]] # Either NULL or a superclass method
    if (is.null(prev_method)) {
      super_class_method <-
        if (identical(method, "initialize")) "initField"
        else ""
    } else if (identical(prev_method@refClassName, class_name)) {
      super_class_method <- prev_method@superClassMethod
    } else {
      super_class_method <- super_class_method_name(prev_method)
      return_methods[[super_class_method]] <- prev_method
    }

    method_definition <- make_class_method(instance_methods[[method]],
      method, class_name, super_class_method, all_methods)
    check <- check && check_fields_in_method(method_definition, field_names,
      super_class_method, all_methods)
    return_methods[[method]] <- method_definition
  }

  if (is.na(check) && methods_package_is_loaded())
    message(gettextf(
      paste("Code for methods in class %s was not checked for suspicious field",
            "assignments (recommended package %s not available?)"),
      dQuote(class_name), sQuote("codetools")), domain = NA)

  return_methods
}

superClassMethodName <- function(def)
    paste(def@name, def@refClassName, sep = "#")


#' Fetch all necessary information to construct a reference class generator function.
#'
#' @rdname setRefClass
#' @param refMethods list. A named list of reference class methods.
ref_class_information <- function(Class, contains, fields, refMethods, where) {
  inject(get_superclasses_information(contains, where))

  # Get full inheritance chain.
  refSuperClasses <- get_all_ref_classes(superClassDefs[isRefSuperClass])
  
  # Parse the fields to, e.g., determine if fields of type ANY need to be
  # initialized as uninitializedField instances.
  parsed_fields <- parse_fields(fields, where)

  fieldNames <- lapply(parsed_fields, `[[`, 3)
  fieldClasses <- lapply(parsed_fields, `[[`, 1)
  fieldPrototypes <- lapply(parsed_fields, `[[`, 2)

  field_information <- 
    process_field_information(fieldNames, fieldClasses, fieldPrototypes,
                              superClassDefs[isRefSuperClass])
  class_methods <- field_information$classMethods
  field_information$classMethods <- field_information$fieldNames <- NULL

  class_methods <- inject_standard_class_methods(class_methods, Class,
    refMethods, names(field_information$fieldClasses), TRUE)

  c(list(superClasses = superClasses, refSuperClasses = refSuperClasses),
    field_information, list(refMethods = class_methods))
}


#' Determine class and prototypes for fields from super class hierarchy.
#'
#' If \code{superClassDefs} is a list of parent classes, their definitions will
#' be traversed in reverse order (so that nearer classes respect the inheritance
#' chain) and the according field and prototypes extracted.
#'
#' @param fieldClasses character. The names for each field.
#' @param fieldClasses character. The classes for each field.
#' @param fieldClasses list or function. The prototype for each field
#'    (i.e. default binding).
#' @param superClassDefs list. A list of super class definitions (things of
#'   class \code{refClassRepresentation}, obtained using \code{methods::getClass}.
process_field_information <- function(fieldNames, fieldClasses, fieldPrototypes, superClassDefs) {
  field_classes <- field_prototypes <- class_methods <- list()

  # Starting backwards, so nearer superclasses override closer super classes,
  # assign the above to this class.
  for (klass in rev(superClassDefs)) {
    field_classnames <- klass@fieldClasses
    field_prototypes <- as.list(klass@fieldPrototypes, all.names = TRUE)
    class_methods_list <- as.list(klass@refMethods, all.names = TRUE)
    insert_fields(field_classes) <- field_classnames
    field_prototypes[names(field_prototypes)] <- field_prototypes
    class_methods[names(class_methods_list)] <- class_methods_list
  }
  insert_fields(field_classes) <- fieldClasses
  field_prototypes[fieldNames] <- fieldPrototypes
  
  list(fieldClasses = field_classes, fieldPrototypes = field_prototypes,
       fieldNames = names(field_prototypes), classMethods = class_methods)
}

#' Parse out field class names and prototypes.
#'
#' @param fields list. A named list of \code{character}s or \code{function}s.
#'    If \code{character}, this represents the class name of the field
#'    (for example, \code{"data.frame"}). If a function, an active binding
#'    that will be used for the field prototype. For example, if we have
#'    a field \code{radius} and a field \code{diameter}, we could set the
#'    default value of \code{radius} (its prototype) to 
#'    \code{function() 2 * radius}, which wil be evaluated within the context
#'    of the reference class environment.
#' @param where environment. Where to look for class definitions.
#' @return a list with three keys, \code{names}, \code{classes} and \code{prototypes} of
#'    class names and prototypes, respectively. Prototypes are just the
#'    default values of the respective fields.
parse_fields <- function(fields, where) {
  lapply(seq_along(fields),
    function(i) parse_field(names(fields)[[i]], fields[[i]], where))
}

#' Parse out field class name and prototype.
#'
#' A field definition (\code{field_value}) can be either a character or
#' a binding function (see the \code{fields} parameter).
#'
#' @rdname parse_fields
#' @param field_name character. The name of the field. (This is primarily 
#'   used for error messages when the \code{field_value} does not validate
#'   all conditions.)
#' @param field_value character or function. See \code{fields}.
#' @return a list with two values, class name and prototype value.
parse_field <- function(field_name, field_value, where) {
  klass <- prototype <- NULL
  if (is.character(field_value)) {
    if (length(field_value) != 1)
      simple_error(paste("A single class name is needed for field %s,",
        "got a character vector of length %d"), sQuote(field_name), length(field_value))
    if (is.null(getClassDef(field_value, where = where)))
      simple_error("Class %s for field %s is not defined", dQuote(field_value), sQuote(field_name))
    klass <- field_value
    prototype <-
      if (klass == "ANY")
        new("uninitializedField", field = field_name, className = "ANY")
      else
        # Making a binding is better than setting a value due to lazy evaluation.
        # With a binding, the variable wont be instantiated until it is requested
        # by some method. # TODO: (RK) This is false... this is something different.
        make_default_binding(field_name, field_value, TRUE, where) 
  } else if (is.function(field_value)) {
    klass <- "activeBindingFunction"
    prototype <- make_active_binding(field_value)
  } else
    simple_error(paste("Field %s was supplied as an object of class %s;",
                       "must be a class name or a binding function"),
                 sQuote(field_name), dQuote(class(field_value)[1]))

  list(klass, prototype)
}

# TODO: (RK) Implement.
`insert_fields<-` <- function(fields, value) {  }
inject_standard_class_methods <- function(class_methods, class_name,
  instance_methods, field_names, once) { } 

#' Get superclass information.
#'
#' Provide a list containing \code{superClasses}, (the actual class names)
#' \code{isRefSuperClass}, a vector of logicals indicating
#' whether each superclass is a reference class,
#' and \code{superClassDefs}, containing the actual superclass
#' definitions, obtained using \code{methods::getClass}.
#'
#' Note that \code{envRefClass} will always be one of the returned
#' superclasses.
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

  isRefSuperClass <- vapply(superclass_definitions,
    function(def) is(def, 'refClassRepresentation'), logical(1))

  if (!any(isRefSuperClass)) {
    superclass_definitions$envRefClass <- getClass("envRefClass", where = .package())
    isRefSuperClass <- c(isRefSuperClass, TRUE)
  }

  list(superClassDefs = superclass_definitions,
       superClasses = vapply(superclass_definitions, function(def) def@className, character(1)),
       isRefSuperClass = isRefSuperClass)
}

#' Get all reference superclass names from the inheritance chain.
#' 
#' Given a list of reference class definitions, fetch all class and superclass
#' definitions from the inheritance chain.
#'
#' @param superClassDefs list. The list of respective class definitions.
#' @return a full character vector of superclasses in the inheritance chain,
#'   obtained by grabbing the slot \code{refSuperClasses} on the class definitions.
get_all_ref_classes <- function(superClassDefs) {
  unique(as.character(c(
    vapply(superClassDefs, function(def) def@className, character(1)),
    c(recursive = TRUE, lapply(superClassDefs, function(def) def@refSuperClasses))
  )))
}


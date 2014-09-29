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
  parsed_fields <- parse_fields(fields)

  # TODO: (RK) fieldClasses and fieldPrototypes need to come from parsed_fields

  field_information <- 
    process_field_information(fieldClasses, fieldPrototypes, superClassDefs[isRefSuperClass])
  class_methods <- field_information$classMethods
  field_information$classMethods <- NULL

  class_methods <- inject_standard_class_methods(class_methods, Class,
    refMethods, names(field_information$fieldClasses), TRUE)

  c(list(superClasses = superClasses, refSuperClasses = refSuperClasses),
    field_information, list(refMethods = class_methods))
}

process_field_information <- function(fieldClasses, fieldPrototypes, superClassDefs) {
  field_classes <- field_prototypes <- class_methods <- list()
  # Starting backwards, so nearer superclasses override closer super classes,
  # assign the above to this class.
  for (klass in rev(superClassDefs)) {
    field_classnames <- klass@fieldClasses
    field_prototypes <- as.list(klass@fieldPrototypes, all.names = TRUE)
    class_methods_list <- as.list(klass@refMethods, all.names = TRUE)
    insertFields(field_classes) <- field_classnames
    field_prototypes[names(field_prototypes)] <- field_prototypes
    class_methods[names(class_methods_list)] <- class_methods_list
  }
  insertFields(field_classes) <- fieldClasses
  field_prototypes[names(fieldPrototypes)] <- fieldPrototypes
  
  list(fieldClasses = field_classes, fieldPrototypes = field_prototypes,
       refMethods = class_methods)
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
#' @param field_name character. The name of the field.
#' @param field_value character or function. See \code{fields}.
parse_fields <- function(fields) {
  lapply(seq_along(fields),
    function(i) parse_field(names(fields)[[i]], fields[[i]]))
}

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


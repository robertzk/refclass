#' Verify that the names of the given object are non-empty and distinct.
#'
#' Since R supports duplicate list names or empty list names, this
#' function is handy for any verification of hash-like names: that is,
#' none must be empty, and they must all be distinct.
#'
#' @param obj any R object that has \code{names}.
#' @param what character. A description to use when \code{error = TRUE}.
#'   For example, if \code{what = "field"} and no names were supplied,
#'   the error \code{"No field names supplied"} will trigger. The default
#'   is \code{""}.
#' @param error logical. Whether or not to error if the names of the \code{obj}
#'   are not unique and non-empty. The default is \code{FALSE}, in which case
#'   a \code{logical} of length will be returned.
#' @param character.only logical. Whether or not to call \code{names} on
#'   \code{obj} or just treat it as a character vector of names. THe 
#'   default is \code{FALSE}.
#' @return If \code{error = TRUE}, an error will be thrown if all the names
#'   of the \code{obj} are not non-empty and unique. Otherwise, \code{TRUE}
#'   or \code{FALSE} will be returned according as the names supplied are
#'   unique and non-empty.
#' @export
#' @examples
#' stopifnot(names_are_unique_and_non_empty(c()))
#' stopifnot(names_are_unique_and_non_empty(list(a = 1, b = 2)))
#' stopifnot(!names_are_unique_and_non_empty(c("")))
#' stopifnot(!names_are_unique_and_non_empty(list(a = 1, a = 2)))
names_are_unique_and_non_empty <- function(obj, what = "", error = FALSE, character.only = FALSE) {
  n <- length(obj)
  propNames <- if (isTRUE(character.only)) obj else names(obj)
  if (!n) return (TRUE)
  error_message <-
    if (is.null(propNames))
      gettextf("No %s names supplied", what)
    else if (!all(nzchar(propNames)))
      gettextf("All %s names must be nonempty in:\n(%s)", what,
               paste(sQuote(propNames), collapse = ", "))
    else if (any(duplicated(propNames))) 
      gettextf("All %s names must be distinct in:\n(%s)", what,
               paste(sQuote(propNames), collapse = ", "))
  if (!is.null(error_message)) {
    if (!identical(error, FALSE))
      stop(error_message, call. = FALSE, domain = NA)
    else FALSE
  } else TRUE
}

#' Convert a list to an environment.
#'
#' If the list if of length 0, be careful to not use \code{as.environment}
#' as it will break.
#'
#' @param x list. The list to convert to an environment.
#' @return an environment with keys coming from the names of \code{x}
#'   and values coming from the respective values of \code{x}.
to_env <- function(x) if (length(x)) as.environment(x) else new.env(FALSE)

#' Inject a list into the current environment.
#'
#' For example, if we call this function with \code{list(a = 1, b = 2)},
#' it will create local variables \code{a} and \code{b} with
#' values \code{1} and \code{2}, respectively.
#'
#' @param values list. A named list of values.
#' @param where environment. Where should we inject this list? The
#'   default is \code{parent.frame()}, the calling environment.
#' @export
#' @return \code{NULL} -- the values will be injected into \code{where}.
#' @examples
#' inject(list(a = 1, b = 2))
#' stopifnot(exists('a') && exists('b'))
inject <- function(values, where = parent.frame()) {
  if (!is.list(values))
    stop(gettextf("values must be a list; instead I got %s", dQuote(class(values)[1])))

  value_names <- names(values)
  if (length(value_names) != length(values)) stop("values must be a named list")

  names_are_unique_and_non_empty(values, "value", error = TRUE)

  for (i in value_names) assign(i, values[[i]], envir = where)
  invisible(NULL)
}

#' Call base::stop with interpolated arguments.
#'
#' @param message character. The message to call \code{base::stop} with.
#' @param ... any instances of \code{"\%s"} in \code{message} will be replaced
#'   with the respective additional arguments.
#' @param call. logical. Whether or not to show a stack trace. The default is
#'   \code{TRUE}.
#' @examples
#' stopifnot(identical('hello world', tryCatch(error = function(e) e$message,
#'   simple_error("hello %s", "world"))))
simple_error <- function(message, ..., call. = TRUE) {
  stop(gettextf(message, ...), domain = NA, call. = call.)
}

# A self-reference to this package
.package <- function() as.environment('package:refclass')


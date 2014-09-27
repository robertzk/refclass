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


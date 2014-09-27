#' Verify that the names of the given object are non-empty and distinct.
#'
#' Since R supports duplicate list names or empty list names, this
#' function is handy for any verification of hash-like names: that is,
#' none must be empty, and they must all be distinct.
#'
#'
#' @param obj any R object that has \code{names}.
#' @param what character. A description to use when \code{error = TRUE}.
#'   For example, if \code{what = "field"} and no names were supplied,
#'   the error \code{"No field names supplied"} will trigger. The default
#'   is \code{""}.
#' @param error logical. Whether or not to error if the names of the \code{obj}
#'   are not unique and non-empty. The default is \code{FALSE}, in which case
#'   a \code{logical} of length will be returned.
#' @return If \code{error = TRUE}, an error will be thrown if all the names
#'   of the \code{obj} are not non-empty and unique. Otherwise, \code{TRUE}
#'   or \code{FALSE} will be returned according as the names supplied are
#'   unique and non-empty.
#' @examples
#' stopifnot(names_are_unique_and_non_empty(c()))
#' stopifnot(names_are_unique_and_non_empty(list(a = 1, b = 2))
#' stopifnot(!names_are_unique_and_non_empty(c("")))
#' stopifnot(!names_are_unique_and_non_empty(list(a = 1, a = 2))
names_are_unique_and_non_empty <- function(obj, what = "", error = FALSE) {
  n <- length(obj)
  propNames <- names(obj)
  if (!n) return (TRUE)
  error_message <-
    if (is.null(propNames))
      gettextf("No %s names supplied", what)
    else if (!all(nzchar(propNames)))
      gettextf("All %s names must be nonempty in:\n(%s)", what)
    else if (any(duplicated(propNames))) 
      gettextf("All %s names must be distinct in:\n(%s)", what)
  if (!is.null(error_message)) {
    if (!identical(error, FALSE))
      stop(error_message, call. = FALSE, domain = NA)
    else FALSE
  } else TRUE
}


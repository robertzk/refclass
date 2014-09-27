#' Format a list or character vector of types or error if invalid.
#'
#' A reference class must specify its fields, for example,
#'   \code{c(data = "data.frame", ntrees = "integer")}. These fields
#' can be provided in several formats. The goal of this function is to
#' ensure that format is correct, make small modifications if necessary,
#' and error if there are problems.
#'
#' The following are valid formats:
#'
#' \itemize{
#'   \item{"character"}{"A named character vector with non-empty and distinct
#'     names.}
#'   \item{"list"}{"A named list with non-empty and distinct names, and
#'      character values of length 1."}
#'   \item{"unnamed character"}{"A totally unnamed character vector with
#'      non-empty and distinct values. These will be turned into the names
#'      of a character vector with values \code{'ANY'}.
#'   }
#' }
#' 
#' If the provided \code{props} does not satisfy any of these, an error
#' will be invoked.
#'
#' # TODO: (RK) Figure out what inferring means...
#'
#' @param props character or list. See full description of this function.
#' @param what character. The name of the thing we are creating a
#'   types list for. The default is \code{"fields"}. This will be used
#'   to give more descriptive error messages.
#' @return a list that is guaranteed to have distinct non-zero length
#'   names and values that are of type character of length 1.
#' @examples
#' stopifnot(identical(format_types_list(c(a = 'character', b = 'data.frame')),
#'   list(a = 'character', b = 'data.frame')))
#' stopifnot(identical(format_types_list(c('a', 'b')), list(a = 'ANY', b = 'ANY')))
#' stopifnot(identical(format_types_list(list(a = 'character')), list(a = 'character')))
#' # The following will error because of duplicate names:
#' format_types_list(list(a = 'character', b = 'character'))
format_types_list <- function(props, what = "fields") {

  NULL
}


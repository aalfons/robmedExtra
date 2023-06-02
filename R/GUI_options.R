# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

# internal function to initialize and set default values for options
initialize_options <- function(...) {

  # assign default values to options (they live in this environment)
  values <- list(...)

  # accessor function to retrieve current options (or a selection)
  get <- function(which, drop = TRUE) {
    if (missing(which)) values
    else if (length(which) == 1L && drop) values[[which]]
    else values[which]
  }

  # accessor function to get (selected) current options
  set <- function(...) {
    # combine supplied options into list and extract names
    new_values <- list(...)
    option_names <- names(new_values)
    # check if a list is supplied as a single unnamed argument
    if (length(new_values) == 1L && is.null(option_names) &&
        is.list(new_values[[1L]])) {
      new_values <- new_values[[1L]]
      option_names <- names(new_values)
    }
    # check if there is anything to do
    if (length(new_values) == 0L || is.null(option_names)) {
      warning("no names or values of options supplied; no options were set",
              call. = FALSE)
    } else {
      # check if supplied options are meaningful
      keep <- option_names %in% names(values)
      new_values <- new_values[keep]
      option_names <- option_names[keep]
      if (length(new_values) == 0L) {
        warning("supplied options do not exist; no options were set",
                call. = FALSE)
      } else if (!all(keep)) {
        warning("some supplied options do not exist; those were not set",
                call. = FALSE)
      } else values[option_names] <<- new_values
    }
    # don't return anything
    invisible()
  }

  # return list of accessor functions
  list(get = get, set = set)
}


#' Options for the graphical user interface for (robust) mediation analysis
#'
#' Retrieve or set global options for the graphical user interface for (robust)
#' mediation analysis via accessor functions.
#'
#' The following options are available:
#' \describe{
#'   \item{\code{}}{}
#'   \item{\code{}}{}
#'   \item{\code{}}{}
#' }
#'
#' @format A list with the following two components:
#' \describe{
#'   \item{\code{get(which, drop = TRUE)}}{an accessor function to retrieve
#'   current options, which are usually returned as a named list.  Argument
#'   \code{which} allows to select which options to retrieve.  If a single
#'   option is selected, argument \code{drop} indicates whether only its value
#'   should be returned (\code{TRUE}) or a list of length one (\code{FALSE}).}
#'   \item{\code{set(...)}}{an accessor function to set certain options using
#'   \code{name = value} pairs.}
#' }
#'
#' @author Andreas Alfons
#'
#' @examples
#' # retrieve the list of options
#' GUI_options$get()
#'
#' # retrieve a single option
#' GUI_options$get("level")
#'
#' # set options
#' GUI_options$set(level = 0.99)
#'
#' @keywords utilities
#'
#' @noRd

GUI_options <- initialize_options()


## we can use the same machinery to pass arguments to the shiny app
GUI_args <- initialize_options(df_name = "")

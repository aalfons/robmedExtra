# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


#' Export a flextable of results from (robust) mediation analysis to 'Microsoft
#' Word'
#'
#' Export a \code{\link[flextable]{flextable}} object to \proglang{Microsoft
#' Word}. This function is intended for tables of results from (robust)
#' mediation analysis.  It is essentially a wrapper for
#' \code{\link[flextable]{save_as_docx}()} with a more convenient argument
#' for page size, and it uses additional information from subclass
#' \code{"\link[=to_flextable]{mediation_flextable}"} to determine the page
#' orientation.
#'
#' @param object  an object of class \code{"\link[flextable]{flextable}"}, or
#' an object containing results from (robust) mediation analysis that can be
#' converted to the subclass \code{"mediation_flextable"} via
#' \code{\link{to_flextable}()}.
#' @param file  a character string specifying the path an file name for the
#' \proglang{Microsoft Word} file to be generated.
#' @param size  a character string specifying the page size.  Possible values
#' are \code{"A4"} for A4 format (297 x 210 mm) or \code{"letter"} for US
#' letter format (11 x 8.5 inches).
#' @param \dots  for the \code{"flextable"} method, additional arguments to
#' be passed down to \code{\link[officer]{prop_section}()} for defining page
#' composition such as page size, page orientation, and margins.  For the
#' default method, additional arguments to be passed down to
#' \code{\link{to_flextable}()} for converting results from (robust)
#' mediation analysis.
#'
#' @return
#' The function is called for its side effect of creating a \proglang{Microsoft
#' Word} file.  It returns the path to the file invisibly.
#'
#' @author Andreas Alfons
#'
#' @seealso
#' \code{\link{export_pptx}()}
#'
#' \code{\link[flextable]{flextable}()}, \code{\link{to_flextable}()}
#'
#' @examples
#' data("BSG2014")
#'
#' # seed to be used for the random number generator
#' seed <- 20211117
#'
#' # perform mediation analysis via robust bootstrap test ROBMED
#' set.seed(seed)
#' robust_boot <- test_mediation(BSG2014,
#'                               x = "ValueDiversity",
#'                               y = "TeamCommitment",
#'                               m = "TaskConflict",
#'                               robust = TRUE)
#'
#' # construct flextable of results
#' ft <- to_flextable(robust_boot)
#'
#' # export to Microsoft Word
#' file_name <- tempfile(fileext = ".docx")
#' export_docx(ft, file = file_name)
#'
#' @export

export_docx <- function(object, file, ...) UseMethod("export_docx")

#' @name export_docx
#' @importFrom flextable save_as_docx
#' @export
export_docx.flextable <- function(object, file, size = c("A4", "letter"), ...) {
  # initializations
  size <- match.arg(size)
  # define page size (in inches)
  if (size == "A4") {
    width <- 210 / 25.4
    height <- 297 / 25.4
  } else {
    width <- 8.5
    height <- 11
  }
  # define page orientation: if we have a mediation flextable in landscape
  # mode, make sure that the document is in landscape mode as well
  if (inherits(object, "mediation_flextable")) {
    orientation <- object$orientation
    if (is.null(orientation)) orientation <- "portrait"
  } else orientation <- "portrait"
  # create object specifying document properties
  properties <- properties_section(width = width, height = height,
                                   orientation = orientation, ...)
  # save flextable to Microsoft Word file
  save_as_docx(object, path = file, pr_section = properties)
}

#' @name export_docx
#' @export
export_docx.default <- function(object, file, ...) {
  ft <- to_flextable(object, ...)
  export_docx(ft, file = file)
}


## internal wrapper function for officer::prop_section()
#' @importFrom officer page_size prop_section
properties_section <- function(width, height, orientation,
                               page_size = NULL, ...) {
  # if argument 'page_size' is not specified, use other arguments
  # (allows user to override the default size and orientation)
  if (is.null(page_size)) {
    page_size <- officer::page_size(width = width, height = height,
                                    orient = orientation)
  }
  # call officer::prop_section()
  officer::prop_section(page_size = page_size, ...)
}

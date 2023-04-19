# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************

#' @export
export_docx <- function(object, ...) UseMethod("export_docx")

## @importFrom flextable body_add_flextable
## @importFrom officer body_end_section_landscape read_docx
#' @importFrom flextable save_as_docx
#' @export
export_docx.flextable <- function(object, file, size = c("A4", "letter"), ...) {
  # initializations
  size <- match.arg(size)
  have_landscape <- inherits(object, "mediation_flextable") &&
    !is.null(object$orientation) && object$orientation == "landscape"
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
    if (is.null(orientation)) orientation <- "landscape"
  } else orientation <- "landscape"
  # create object specifying document properties
  properties <- properties_section(width = width, height = height,
                                   orientation = orientation, ...)
  # save flextable to Microsoft Word file
  save_as_docx(object, path = file, pr_section = properties)
}

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

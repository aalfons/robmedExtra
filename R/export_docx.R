# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************

#' @export
export_docx <- function(object, ...) UseMethod("export_docx")

#' @importFrom flextable body_add_flextable
#' @importFrom officer read_docx
#' @export
## TODO: play with font size and cell margins so that certain example tables
##       fit nicely
export_docx.flextable <- function(object, file, ...) {
  # create .docx file
  docx <- officer::read_docx()
  # add flextables of results from mediation analysis
  docx <- flextable::body_add_flextable(docx, object)
  # if we have a mediation flextable in landscape mode, make sure that the
  # Word document is in landscape mode as well
  if (inherits(object, "mediation_flextable") &&
      !is.null(object$orientation) &&
      object$orientation == "landscape") {
    docx <- body_end_section_landscape(docx)
    # FIXME: we get a blank page in portrait mode at the end of the document
    #        that should be removed
  }
  # write to file
  print(docx, target = file)
  # return file invisibly
  invisible(docx)
}

#' @export
export_docx.default <- function(object, file, ...) {
  ft <- to_flextable(object, ...)
  export_docx(ft, file = file)
}

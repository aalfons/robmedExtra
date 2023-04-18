# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************

# TODO: look at function save_as_docx() from package 'flextable'
#       This function allows to supply an object that defines page layout
#       (e.g., orientation, width, height). If this function is a generic
#       function, it may be therefore be better to write a method for subclass
#       "mediation_flextable".

#' @export
export_docx <- function(object, ...) UseMethod("export_docx")

#' @importFrom flextable body_add_flextable
#' @importFrom officer body_end_section_landscape read_docx
#' @export
## TODO: allow to set font, font size, and cell margins
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
    docx <- officer::body_end_section_landscape(docx)
    # Note: We get a blank page in portrait mode at the end of the document
    #       that should be removed. Package 'officer' doesn't seem to provide
    #       a way to avoid or remove that blank page.
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

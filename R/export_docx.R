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
export_docx.flextable <- function(object, file, ...) {
  # create .docx file
  docx <- officer::read_docx()
  # add flextables of results from mediation analysis
  docx <- flextable::body_add_flextable(docx, object)
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

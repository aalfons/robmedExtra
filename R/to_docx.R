# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************

#' @export
to_docx <- function(object, ...) UseMethod("to_docx")

#' @importFrom officer read_docx
#' @export
to_docx.flextable <- function(object, file, ...) {
  # create .docx file
  docx <- officer::read_docx()
  # add flextables of results from mediation analysis
  # FIXME: make sure the columns of the three tables are aligned
  docx <- body_add_flextable(docx, object)
  # write to file
  print(docx, target = file)
  # return file invisibly
  invisible(docx)
}

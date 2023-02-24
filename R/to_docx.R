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
to_docx.mediation_flextables <- function(object, file, ...) {
  # create .docx file
  docx <- officer::read_docx()
  # add flextables of results from mediation analysis
  # FIXME: make sure the columns of the three tables are aligned
  docx <- flextable::body_add_flextable(docx, object$total)
  docx <- flextable::body_add_flextable(docx, object$direct)
  docx <- flextable::body_add_flextable(docx, object$indirect)
  # write to file
  print(docx, target = file)
  # return file invisibly
  invisible(docx)
}

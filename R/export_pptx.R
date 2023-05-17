# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


#' Export a flextable of results from (robust) mediation analysis to Powerpoint
#'
#' Export a \code{\link[flextable]{flextable}} object to Microsoft Powerpoint.
#' This function is intended for tables of results from (robust) mediation
#' analysis.  It is essentially a wrapper for
#' \code{\link[flextable]{save_as_pptx}()}.
#'
#' @param object  an object of class \code{"\link[flextable]{flextable}"}, or
#' an object containing results from (robust) mediation analysis that can be
#' converted to the subclass \code{"mediation_flextable"} via
#' \code{\link{to_flextable}()}.
#' @param file  a character string specifying the path an file name for the
#' Microsoft Powerpoint file to be generated.
#' @param \dots  for the \code{"flextable"} method, additional arguments to
#' be passed down to \code{\link[officer]{prop_section}()} for defining page
#' composition such as page size, page orientation, and margins.  For the
#' default method, additional arguments to be passed down to
#' \code{\link{to_flextable}()} for converting results from (robust)
#' mediation analysis.
#'
#' @return
#' The function is called for its side effect of creating a Microsoft
#' Powerpoint file.  It returns the path to the file invisibly.
#'
#' @author Andreas Alfons
#'
#' @seealso
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
#' \dontrun{
#' # export to Microsoft Powerpoint
#' file_name <- tempfile(fileext = ".pptx")
#' export_pptx(ft, file = file_name)}
#'
#' @export

export_pptx <- function(object, file, ...) UseMethod("export_pptx")

#' @name export_pptx
#' @importFrom flextable save_as_pptx
#' @export
export_pptx.flextable <- function(object, file, ...) {
  # make sure that we have a mediation flextable in landscape mode
  if (inherits(object, "mediation_flextable")) {
    orientation <- object$orientation
    if (!is.null(orientation) && orientation != "landscape") {
      stop("only tables in landscape format can be exported to ",
           "Microsoft Powerpoint")
    }
  }
  # save flextable to Microsoft Powerpoint file
  save_as_pptx(object, path = file)
}

#' @name export_pptx
#' @export
export_pptx.default <- function(object, file, ...) {
  ft <- to_flextable(object, ...)
  export_pptx(ft, file = file)
}

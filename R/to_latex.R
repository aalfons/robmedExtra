# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# Convert results from mediation analysis to a LaTeX table -----

#' @export
to_latex <- function(object, ...) UseMethod("to_latex")

#' @export
to_latex.test_mediation <- function(object, type = c("boot", "data"), ...) {
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # call method for summary
  to_latex(summary, ...)
}

#' @export
to_latex.summary_test_mediation <- function(object, p_value = FALSE,
                                            digits = 3L,
                                            align = c("lrrrr", "c"),
                                            ...) {
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, digits = digits)
  # add alignment specification and set class
  tables$align <- align
  class(tables) <- "mediation_latex_tables"
  # return object for LaTeX tables
  tables
}

#' @export
print.mediation_latex_tables <- function(x, ...) {
  ## initializations
  direct <- x$direct
  indirect <- x$indirect
  width <- ncol(direct) - ncol(indirect) + 1L
  ## initialize LaTeX table
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{", x$align[1L], "}\n", sep = "")
  cat("\\hline\\noalign{\\smallskip}\n")
  ## write table for total effects
  total <- to_effect_table(x$total, which = "latex")
  # write table header
  cat(paste(names(total), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, total), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  ## write table for direct effects
  direct <- to_effect_table(direct, which = "latex")
  # write table header
  cat(paste(names(direct), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, direct), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  ## write table for indirect effects
  indirect <- to_indirect_table(indirect, which = "latex", width = width,
                                align = x$align[2L])
  # write table header
  cat(paste(names(indirect), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, indirect), "\\\\ \n")
  cat(lines, sep = "")
  ## finalize LaTeX table
  cat("\\noalign{\\smallskip}\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  ## add note
  cat("\\emph{Note}.", format_latex_note(x$note), "\n")
}


# Internal functions -----

# wrapper function for pasting columns of LaTeX table with column separator '&'
paste_amp <- function(..., sep = " & ") paste(..., sep = sep)

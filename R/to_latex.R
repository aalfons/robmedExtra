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

## Formatting numbers is not done via formatC().  Arguments are passed down
## via '...' (such as 'digits' for the number of digits), but some of the
## defaults are different.  In addition, argument 'big.mark' is ignored for
## the numbers in the table and only used for the sample size and number of
## bootstrap samples in the table note.
## The first element in argument 'align' is used as alignment specification in
## the \begin{tabular} statement, while the second element is used for the
## alignment specification in \multicolumn{} statements in case of bootstrapped
## confidence intervals in case of the indirect effect.
#' @export
to_latex.summary_test_mediation <- function(object, p_value = FALSE,
                                            align = c("lrrrr", "c"),
                                            ...) {
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, ...)
  # add alignment specification and set class
  tables$align <- align
  class(tables) <- "mediation_latex_tables"
  # return object for LaTeX tables
  tables
}

#' @export
to_latex.list <- function(object, type = c("boot", "data"), p_value = FALSE,
                          orientation = c("portrait", "landscape"),
                          align = NULL, ...) {
  # initializations
  orientation <- match.arg(orientation)
  if (is.null(align)) {
    if (orientation == "portrait") align <- c("lrrrr", "c")
    else align <- c("lrrrrlrrrr", "c")
  }
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, type = type, p_value = p_value, ...)
  # add orientation and alignment specification, and set class
  tables$orientation <- orientation
  tables$align <- align
  class(tables) <- "mediation_latex_tables"
  # return object for LaTeX tables
  tables
}

#' @export
print.mediation_latex_tables <- function(x, ...) {
  ## initializations
  # direct <- x$direct
  # indirect <- x$indirect
  ## initialize LaTeX table
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{", x$align[1L], "}\n", sep = "")
  cat("\\hline\\noalign{\\smallskip}\n")
  # ## write table for total effects
  # total <- format_table_latex(x$total)
  # # write table header
  # cat(paste(names(total), collapse = " & "), "\\\\ \n")
  # cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # # write table body
  # lines <- paste(do.call(paste_amp, total), "\\\\ \n")
  # cat(lines, sep = "")
  # cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # ## write table for direct effects
  # direct <- format_table_latex(direct)
  # # write table header
  # cat(paste(names(direct), collapse = " & "), "\\\\ \n")
  # cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # # write table body
  # lines <- paste(do.call(paste_amp, direct), "\\\\ \n")
  # cat(lines, sep = "")
  # cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # ## write table for indirect effects
  # j_ci <- grep("Confidence Interval", names(indirect), fixed = TRUE)
  # width <- ncol(direct) - ncol(indirect) + 1L
  # indirect <- format_table_latex(indirect, multicolumn = j_ci, width = width,
  #                               align = x$align[2L])
  # # write table header
  # cat(paste(names(indirect), collapse = " & "), "\\\\ \n")
  # cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # # write table body
  # lines <- paste(do.call(paste_amp, indirect), "\\\\ \n")
  # cat(lines, sep = "")
  ## print table contents
  if (is.null(x$orientation)) {
    print_latex_table(x$total, x$direct, x$indirect, align = x$align[2L], ...)
  } else if (x$orientation == "portrait") {
    first <- x$methods[1L]
    for (method in x$methods) {
      if (method != first) {
        cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
      }
      print_latex_table(x$total[[method]], x$direct[[method]],
                        x$indirect[[method]], label = method,
                        align = x$align[2L], ...)
    }
  } else {
    stop("not implemented yet")
  }
  ## finalize LaTeX table
  cat("\\noalign{\\smallskip}\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  ## add note
  note <- get_table_note(x = x$x, m = x$m, y = x$y, covariates = x$covariates,
                         n = x$n, R = x$R, type = "latex")
  cat(note, "\n")
}


print_latex_table <- function(total, direct, indirect, label = NULL,
                              align = "c", ...) {
  ## if submitted, write label for method
  if (!is.null(label)) {
    p <- ncol(total)
    cat(" & \\multicolumn{", p-1L, "}{", align, "}{", label, "} \\\\ \n",
        sep = "")
    cat("\\noalign{\\smallskip}\\cline{2-", p, "}\\noalign{\\smallskip}\n")
  }
  ## write table for total effects
  total <- format_table_latex(total)
  # write table header
  cat(paste(names(total), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, total), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  ## write table for direct effects
  direct <- format_table_latex(direct)
  # write table header
  cat(paste(names(direct), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, direct), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  ## write table for indirect effects
  j_ci <- grep("Confidence Interval", names(indirect), fixed = TRUE)
  width <- ncol(direct) - ncol(indirect) + 1L
  indirect <- format_table_latex(indirect, multicolumn = j_ci, width = width,
                                 align = align)
  # write table header
  cat(paste(names(indirect), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, indirect), "\\\\ \n")
  cat(lines, sep = "")
  # return NULL invisibly
  invisible()
}

# Internal functions -----

# format a table for LaTeX
format_table_latex <- function(object, multicolumn = NULL,
                               width = 1L, align = "c") {
  # format the table header for LaTeX
  names(object) <- format_header_latex(object)
  # format the table body for LaTeX
  object[, 1L] <- format_labels_latex(object[, 1L])
  object[, -1L] <- lapply(object[, -1L], format_values_latex)
  # if applicable, ensure that specified column uses \multicolumn{} statement
  # (condition uses length() as grep() returns because empty integer vector if
  # there is no match)
  if (length(multicolumn) == 1L) {
    multicolumn_statement <- "\\multicolumn{%d}{%s}{%s}"
    names(object)[multicolumn] <- sprintf(multicolumn_statement, width, align,
                                          names(object)[multicolumn])
    object[[multicolumn]] <- sprintf(multicolumn_statement, width, align,
                                     object[[multicolumn]])
  }
  # return formatted object
  object
}

# format the table header for LaTeX
format_header_latex <- function(object) {
  # extract column names
  cn <- names(object)
  # format for LaTeX
  cn <- gsub("^(([ptz]) )", "$\\2$ ", cn, fixed = FALSE)
  cn <- gsub("%", "\\%", cn, fixed = TRUE)
  # return formatted column names
  cn
}

# format the label column of a table for LaTeX
format_labels_latex <- function(labels) {
  # first call format_values_latex(), which puts cells in math mode
  labels <- format_values_latex(labels)
  # format arrows and ellipses nicely
  labels <- gsub("->", " \\rightarrow ", labels, fixed = TRUE)
  labels <- gsub("...", " \\ldots ", labels, fixed = TRUE)
  # ensure that indices are formatted as subscripts
  labels <- gsub("([0-9]+)", "_{\\1}", labels, fixed = FALSE)
  # ensure that a space interrupts math mode so that the space is maintained
  labels <- gsub(" (", "$ $(", labels, fixed = TRUE)
  # ensure that label for total indirect effect is in text mode
  labels <- gsub("$(total)$", "(total)", labels, fixed = TRUE)
  # return labels
  labels
}

# format a column of a table for LaTeX
format_values_latex <- function(column) {
  paste0("$", column, "$")
}

# wrapper function for pasting columns of LaTeX table with column separator '&'
paste_amp <- function(..., sep = " & ") paste(..., sep = sep)

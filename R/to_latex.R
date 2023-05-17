# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# Convert results from mediation analysis to a LaTeX table -----


#' LaTeX table of results from (robust) mediation analysis
#'
#' Generate \proglang{LaTeX} code for a tabular summary of results from
#' (robust) mediation analysis in the form of a \code{tabular} environment.
#' This \code{tabular} environment can easily be integrated into a dynamic
#' \proglang{LaTex} document with tools such as package \pkg{knitr}, which
#' eliminates the risk of mistakes in reporting that stem from
#' copying-and-pasting results.  Note that \code{to_latex()} itself does not
#' actually produce any \proglang{LaTeX} code, it is the \code{print()} method
#' of the resulting object that prints the code for the \code{tabular}
#' environment.
#'
#' @param object  an object inheriting from class
#' \code{"\link[robmed]{test_mediation}"} or
#' \code{"\link[robmed:summary.test_mediation]{summary_test_mediation}"}
#' containing results from (robust) mediation analysis, or a list of such
#' objects (typically obtained via different procedures for mediation
#' analysis).  In case of a named list, the supplied names are used as labels
#' in the resulting \proglang{LaTeX} table, otherwise default labels are
#' constructed based on how the mediation model was fitted and which type of
#' test was used (e.g., \code{"ROBMED"} or \code{"OLS Bootstrap"}).
#' @param \dots  additional arguments to be passed down, eventually to
#' \code{\link[base]{formatC}()} for formatting numbers.  In particular,
#' argument \code{digits} can be used to customize the number of digits after
#' the decimal point (defaults to 3).  Also note that argument \code{big.mark}
#' is ignored for the numbers in the table; it is only used for formatting the
#' sample size and (if applicable) the number of bootstrap samples in the table
#' note.
#'
#' @return
#' An object of class \code{"mediation_latex_tables"} with the following
#' components:
#' \item{labels}{a character string giving the labels to be used in the
#' \proglang{LaTeX} table (only returned if a list of objects is supplied).}
#' \item{total}{a data frame containing a tabular summary of the total effects,
#' or a list of such data frames.}
#' \item{direct}{a data frame containing a tabular summary of the direct
#' effects, or a list of such data frames.}
#' \item{indirect}{a data frame containing a tabular summary of the indirect
#' effects, or a list of such data frames.}
#' \item{x, m, y, covariates}{character vectors specifying the respective
#' variables used.}
#' \item{n}{a character string containing the (formatted) sample size.}
#' \item{R}{a character string containing the (formatted) number of bootstrap
#' samples (only returned if applicable).}
#' \item{orientation}{a character string specifying how to arrange the results
#' from different objects in the \proglang{LaTeX} table (only returned if a
#' list of objects is supplied).}
#' \item{align}{a character vector of length two. The first element gives the
#' alignment specification to be used in the \code{\\begin\{tabular\}\{\}}
#' statement, while the second element gives the alignment specification to be
#' used in \code{\\multicolumn\{\}\{\}\{\}} statements for any bootstrapped
#' confidence intervals of the indirect effect.}
#'
#' @note
#' The \code{print()} method for class \code{"mediation_latex_tables"} prints
#' the \proglang{LaTeX} code for the \code{tabular} environment.  It ignores
#' any additional arguments, and it returns the supplied object invisibly.
#'
#' @author Andreas Alfons
#'
#' @references
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022a) A Robust Bootstrap Test
#' for Mediation Analysis.  \emph{Organizational Research Methods},
#' \bold{25}(3), 591--617.  doi:10.1177/1094428121999096.
#'
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022b) Robust Mediation Analysis:
#' The \R Package \pkg{robmed}.  \emph{Journal of Statistical Software},
#' \bold{103}(13), 1--45.  doi:10.18637/jss.v103.i13.
#'
#' @seealso
#' \code{\link{test_mediation}()},
#' \code{\link[robmed:summary.test_mediation]{summary}()}
#'
#' \code{\link{to_flextable}()}
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
#' # construct LaTeX table of results
#' to_latex(robust_boot)
#'
#' # perform mediation analysis via the OLS bootstrap
#' set.seed(seed)
#' ols_boot <- test_mediation(BSG2014,
#'                            x = "ValueDiversity",
#'                            y = "TeamCommitment",
#'                            m = "TaskConflict",
#'                            robust = FALSE)
#'
#' # construct LaTeX table of results from both procedures
#' boot_list <- list(ols_boot, robust_boot)
#' to_latex(boot_list, orientation = "landscape")
#'
#' # customize labels for procedures and number of digits
#' boot_list_named <- list("Non-robust" = ols_boot,
#'                         "Robust" = robust_boot)
#' to_latex(boot_list_named, orientation = "landscape",
#'          digits = 4)
#'
#' @export

to_latex <- function(object, ...) UseMethod("to_latex")


#' @name to_latex
#'
#' @param type  a character string specifying which estimates and significance
#' tests to report if mediation analysis was done via a bootstrap procedure.
#' If \code{"boot"} (the default), the means of the bootstrap replicates are
#' reported as point estimates for all effects, and significance tests for the
#' total and direct effects use the normal approximation of the bootstrap
#' distribution (i.e., the tests assume a normal distribution of the
#' corresponding effect with the standard deviation computed from the bootstrap
#' replicates).  If \code{"data"}, the point estimates on the original data are
#' reported for all effects, and the significance tests for the total and
#' direct effects are based on statistical theory (e.g., t-tests if the
#' coefficients are estimated via regression).  Note that for bootstrap
#' procedures, significance of the indirect effect is always reported via a
#' percentile-based confidence interval due to the asymmetry of its
#' distribution.
#'
#' @export

to_latex.test_mediation <- function(object, type = c("boot", "data"), ...) {
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # call method for summary
  to_latex(summary, ...)
}


#' @name to_latex
#'
#' @param p_value  a logical indicating whether to include p-values for the
#' indirect effects if mediation analysis was done via a bootstrap procedure
#' (defaults to \code{FALSE}).  If \code{TRUE}, the p-values are obtained via
#' \code{\link[robmed]{p_value}()} and may take some time to compute.
#' @param align  a character vector of length two.  The first element is used
#' as the alignment specification in the \code{\\begin\{tabular\}\{\}}
#' statement, while the second element is used for the alignment specification
#' in \code{\\multicolumn\{\}\{\}\{\}} statements in case of bootstrapped
#' confidence intervals of the indirect effect.  For the former, note that
#' if \code{object} is a list of objects and \code{orientation} is
#' \code{"landscape"}, two empty columns are inserted in the \code{LaTeX}
#' table.  It is not recommended to set this argument unless you know what
#' you are doing.
#'
#' @export

to_latex.summary_test_mediation <- function(object, p_value = FALSE,
                                            align = c("lrrrr", "c"),
                                            ...) {
  # TODO: add checks for argument 'align'
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, ...)
  # add alignment specification and set class
  tables$align <- align
  class(tables) <- "mediation_latex_tables"
  # return object for LaTeX tables
  tables
}


#' @name to_latex
#'
#' @param orientation  a character string specifying how to arrange the results
#' from different objects (list elements) in the \proglang{LaTeX} table.  If
#' \code{"portrait"}, results from different objects are arranged underneath
#' one another, which is intended for documents in portrait mode.  If
#' \code{"landscape"}, results from two objects are arranged next to each
#' other with the results from remaining objects underneath (in groups of
#' two), which is intended for documents in landscape mode (or \proglang{LaTeX}
#' table environments such as \code{sidewaystable}).
#'
#' @export

to_latex.list <- function(object, type = c("boot", "data"), p_value = FALSE,
                          orientation = c("portrait", "landscape"),
                          align = NULL, ...) {
  # check argument for orientation
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, type = type, p_value = p_value, ...)
  # check argument for alignment
  # TODO: add checks for argument 'align'
  if (is.null(align)) {
    n_methods <- length(tables$labels)
    if (n_methods == 1L || orientation == "portrait") align <- c("lrrrr", "c")
    else align <- c("llrrrrlrrrr", "c")
  }
  # add alignment specification and set class
  tables$orientation <- orientation
  tables$align <- align
  class(tables) <- "mediation_latex_tables"
  # return object for LaTeX tables
  tables
}


# print() method that generates the LaTeX code -----

#' @export
print.mediation_latex_tables <- function(x, ...) {
  ## initialize LaTeX table
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{", x$align[1L], "}\n", sep = "")
  cat("\\hline\\noalign{\\smallskip}\n")
  ## print table contents
  n_methods <- length(x$labels)
  if (n_methods == 0L) {
    print_latex_table(x$total, x$direct, x$indirect, align = x$align[2L], ...)
  } else if (n_methods == 1L) {
    print_latex_table(x$total[[1L]], x$direct[[1L]], x$indirect[[1L]],
                      label = x$labels[1L], align = x$align[2L], ...)
  } else if (x$orientation == "portrait") {
    # add each table under the previous one
    for (i in seq_len(n_methods)) {
      # except for first table, add horizontal separation line
      if (i > 1L) cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
      # print current table
      print_latex_table(x$total[[i]], x$direct[[i]], x$indirect[[i]],
                        label = x$labels[i], align = x$align[2L], ...)
    }
  } else {
    # put two tables next to each other and add the next ones underneath
    have_even <- n_methods %% 2L == 0L
    i_max <- ceiling(n_methods / 2)
    # loop over rows of tables
    for (i in seq_len(i_max)) {
      # except for first row of tables, add horizontal separation line
      if (i > 1L) cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
      # indices of left and right tables in current row
      i_right <- 2L * i
      i_left <- i_right - 1L
      # print tables
      if (i < i_max || have_even) {
        # print left and right tables of current row
        print_latex_tables(x$total[[i_left]], x$direct[[i_left]],
                           x$indirect[[i_left]], x$labels[i_left],
                           x$total[[i_right]], x$direct[[i_right]],
                           x$indirect[[i_right]], x$labels[i_right],
                           align = x$align[2L], ...)
      } else {
        # last row and uneven number of tables: only one table left to print
        print_latex_tables(x$total[[i_left]], x$direct[[i_left]],
                           x$indirect[[i_left]], x$labels[i_left], ...)
      }
    }
  }
  ## finalize LaTeX table
  cat("\\noalign{\\smallskip}\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  ## add note
  note <- get_table_note(x = x$x, m = x$m, y = x$y, covariates = x$covariates,
                         n = x$n, R = x$R, type = "latex")
  cat(note, "\n")
  ## return object invisibly
  invisible(x)
}

# print lines for a latex table of a single method
print_latex_table <- function(total, direct, indirect, label = NULL,
                              align = "c", ...) {
  ## if supplied, write label for method
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
  ## return NULL invisibly
  invisible()
}

# print lines for latex tables of two methods side by side
print_latex_tables <- function(total_left, direct_left, indirect_left,
                               label_left, total_right = NULL,
                               direct_right = NULL, indirect_right = NULL,
                               label_right = NULL, align = "c", ...) {
  ## initializations
  have_right <- !is.null(total_right) && !is.null(direct_right) &&
    !is.null(indirect_right) && !is.null(label_right)
  ## write labels for methods
  p <- ncol(total_left)
  cat(" & & \\multicolumn{", p-1L, "}{", align, "}{", label_left,
      "} & & \\multicolumn{", p-1L, "}{", align, "}{", label_right,
      "} \\\\ \n", sep = "")
  cline_left <- sprintf("\\cline{3-%d}", p+1L)
  cline_right <- if (have_right) sprintf("\\cline{%d-%d}", p+3L, 2L*p+1L)
  cat("\\noalign{\\smallskip}", cline_left, cline_right,
      "\\noalign{\\smallskip}\n", sep = "")
  ## write table for total effects
  n_total <- nrow(total_left)
  empty <- get_empty_df(n_total, 1L)
  if (have_right) total_right <- total_right[, -1L, drop = FALSE]
  else total_right <- get_empty_df(n_total, p-1L)
  total <- cbind(total_left[, 1L, drop = FALSE], empty,
                 total_left[, -1L, drop = FALSE], empty,
                 total_right, fix.empty.names = FALSE)
  total <- format_table_latex(total)
  # write table header
  cat(paste(names(total), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, total), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  ## write table for direct effects
  n_direct <- nrow(direct_left)
  empty <- get_empty_df(n_direct, 1L)
  if (have_right) direct_right <- direct_right[, -1L, drop = FALSE]
  else direct_right <- get_empty_df(n_direct, p-1L)
  direct <- cbind(direct_left[, 1L, drop = FALSE], empty,
                  direct_left[, -1L, drop = FALSE], empty,
                  direct_right, fix.empty.names = FALSE)
  direct <- format_table_latex(direct)
  # write table header
  cat(paste(names(direct), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, direct), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  ## write table for indirect effects
  n_indirect <- nrow(indirect_left)
  empty <- get_empty_df(n_indirect, 1L)
  # format left table
  indirect_left <- cbind(indirect_left[, 1L, drop = FALSE], empty,
                         indirect_left[, -1L, drop = FALSE],
                         fix.empty.names = FALSE)
  j_ci_left <- grep("Confidence Interval", names(indirect_left), fixed = TRUE)
  width_left <- ncol(direct_left) - ncol(indirect_left) + 2L
  indirect_left <- format_table_latex(indirect_left, multicolumn = j_ci_left,
                                      width = width_left, align = align)
  # format right table
  if (have_right) {
    indirect_right <- indirect_right[, -1L, drop = FALSE]
    j_ci_right <- grep("Confidence Interval", names(indirect_right),
                       fixed = TRUE)
    width_right <- ncol(direct_right) - ncol(indirect_right) + 1L
    indirect_right <- format_table_latex(indirect_right, labels = FALSE,
                                         multicolumn = j_ci_right,
                                         width = width_right, align = align)
  } else indirect_right <- get_empty_df(n_indirect, p-1L)
  # put formatted left and right tables together
  indirect <- cbind(indirect_left, empty, indirect_right,
                    fix.empty.names = FALSE)
  # write table header
  cat(paste(names(indirect), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body
  lines <- paste(do.call(paste_amp, indirect), "\\\\ \n")
  cat(lines, sep = "")
  ## return NULL invisibly
  invisible()
}


# Internal functions -----

# format a table for LaTeX
format_table_latex <- function(object, labels = TRUE, multicolumn = NULL,
                               width = 1L, align = "c") {
  # format the table header for LaTeX
  names(object) <- format_header_latex(object)
  # format the table body for LaTeX
  if (labels) {
    object[, 1L] <- format_labels_latex(object[, 1L])
    object[, -1L] <- lapply(object[, -1L], format_values_latex)
  } else object <- lapply(object, format_values_latex)
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
  # # replace column names that consist only of spaces by empty string
  # cn <- gsub("^ +$", "", cn, fixed = FALSE)
  # format column names for LaTeX
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
  # return formatted labels
  labels
}

# format a column of a table for LaTeX
format_values_latex <- function(column) {
  # replace cells that consist only of spaces by empty string
  column <- gsub("^ +$", "", column, fixed = FALSE)
  # put nonempty strings in math mode
  column <- gsub("^(.+)$", "$\\1$", column, fixed = FALSE)
  # return formatted column
  column
}

# wrapper function for pasting columns of LaTeX table with column separator '&'
paste_amp <- function(..., sep = " & ") paste(..., sep = sep)

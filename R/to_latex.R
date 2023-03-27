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

# TODO: add checks for argument 'align'

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
  # check argument for orientation
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, type = type, p_value = p_value, ...)
  # check argument for alignment
  if (is.null(align)) {
    n_methods <- length(tables$methods)
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
  n_methods <- length(x$methods)
  if (n_methods == 0L) {
    print_latex_table(x$total, x$direct, x$indirect, align = x$align[2L], ...)
  } else if (n_methods == 1L) {
    print_latex_table(x$total[[1L]], x$direct[[1L]], x$indirect[[1L]],
                      label = x$methods[1L], align = x$align[2L], ...)
  } else if (x$orientation == "portrait") {
    # add each table under the previous one
    for (i in seq_len(n_methods)) {
      # except for first table, add horizontal separation line
      if (i > 1L) cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
      # print current table
      print_latex_table(x$total[[i]], x$direct[[i]], x$indirect[[i]],
                        label = x$methods[i], align = x$align[2L], ...)
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
                           x$indirect[[i_left]], x$methods[i_left],
                           x$total[[i_right]], x$direct[[i_right]],
                           x$indirect[[i_right]], x$methods[i_right],
                           align = x$align[2L], ...)
      } else {
        # last row and uneven number of tables: only one table left to print
        print_latex_tables(x$total[[i_left]], x$direct[[i_left]],
                           x$indirect[[i_left]], x$methods[i_left], ...)
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

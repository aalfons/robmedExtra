# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Convert results from mediation analysis to a flextable -----


#' Tabular summary of results from (robust) mediation analysis
#'
#' Convert results from (robust) mediation analysis to a tabular summary in
#' the form of a \code{\link[flextable]{flextable}}.  This \code{flextable}
#' can easily be exported to \proglang{Microsoft Word} or \proglang{Microsoft
#' Powerpoint}, or integrated into a dynamic \proglang{Markdown} document.
#' This eliminates the risk of mistakes in reporting that stem from
#' copying-and-pasting results.
#'
#' @param object  an object inheriting from class
#' \code{"\link[robmed]{test_mediation}"} or
#' \code{"\link[robmed:summary.test_mediation]{summary_test_mediation}"}
#' containing results from (robust) mediation analysis, or a list of such
#' objects (typically obtained via different procedures for mediation
#' analysis).  In case of a named list, the supplied names are used as labels
#' in the resulting \code{flextable}, otherwise default labels are constructed
#' based on how the mediation model was fitted and which type of test was used
#' (e.g., \code{"ROBMED"} or \code{"OLS Bootstrap"}).
#' @param \dots  additional arguments to be passed down, eventually to
#' \code{\link[base]{formatC}()} for formatting numbers.  In particular,
#' argument \code{digits} can be used to customize the number of digits after
#' the decimal point (defaults to 3).  Also note that argument \code{big.mark}
#' is ignored for the numbers in the table; it is only used for formatting the
#' sample size and (if applicable) the number of bootstrap samples in the
#' table note.
#'
#' @return  An object of class \code{"mediation_flextable"}, which inherits
#' from class \code{"\link[flextable]{flextable}"}.
#'
#' @note
#' Numbers are not formatted via \code{flextable} defaults (see
#' \code{\link[flextable]{set_flextable_defaults}()}), but instead via
#' \code{\link[base]{formatC}()}. This is done to ensure consistency in
#' number formatting between functions \code{to_flextable()} and
#' \code{\link{to_latex}()}.
#'
#' @author Andreas Alfons, based on code by Vincent Drenth
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
#' \code{\link{export_docx}()}, \code{\link{export_pptx}()}
#'
#' \code{\link{to_latex}()}
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
#' to_flextable(robust_boot)
#'
#' # perform mediation analysis via the OLS bootstrap
#' set.seed(seed)
#' ols_boot <- test_mediation(BSG2014,
#'                            x = "ValueDiversity",
#'                            y = "TeamCommitment",
#'                            m = "TaskConflict",
#'                            robust = FALSE)
#'
#' # construct flextable of results from both procedures
#' boot_list <- list(ols_boot, robust_boot)
#' to_flextable(boot_list)
#'
#' # customize labels for procedures and number of digits
#' boot_list_named <- list("Non-robust" = ols_boot,
#'                         "Robust" = robust_boot)
#' to_flextable(boot_list_named, digits = 4)
#'
#' @export

to_flextable <- function(object, ...) UseMethod("to_flextable")


#' @name to_flextable
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

to_flextable.test_mediation <- function(object, type = c("boot", "data"), ...) {
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # call method for summary
  to_flextable(summary, ...)
}


#' @name to_flextable
#'
#' @param p_value  a logical indicating whether to include p-values for the
#' indirect effects if mediation analysis was done via a bootstrap procedure
#' (defaults to \code{FALSE}).  If \code{TRUE}, the p-values are obtained via
#' \code{\link[robmed]{p_value}()} and may take some time to compute.
#'
#' @export

to_flextable.summary_test_mediation <- function(object, p_value = FALSE, ...) {
  # call workhorse function to format tables for effects
  tables <- get_mediation_tables(object, p_value = p_value, ...)
  # put data frames of effects together
  df <- prepare_table(tables$total, tables$direct, tables$indirect,
                      p_value = p_value)
  # create flextable
  info <- tables[c("x", "m", "y", "covariates", "n", "R")]
  mediation_flextable(df, info = info)
}


#' @name to_flextable
#'
#' @param orientation  a character string specifying how to arrange the results
#' from different objects (list elements) in the \code{flextable}.  If
#' \code{"landscape"} (the default), results from two objects are arranged next
#' to each other with the results from remaining objects underneath (in groups
#' of two), which is intended for documents in landscape mode.  If
#' \code{"portrait"}, results from different objects are arranged underneath
#' one another, which is intended for documents in portrait mode.
#'
#' @export

to_flextable.list <- function(object, type = c("boot", "data"), p_value = FALSE,
                              orientation = c("landscape", "portrait"), ...) {

  # check arguments
  type <- match.arg(type)
  orientation <- match.arg(orientation)
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, type = type, p_value = p_value, ...)
  # further initializations
  methods <- tables$labels
  n_methods <- length(methods)

  # construct flextable
  if (n_methods == 1L) {

    # put data frames of effects together
    df <- prepare_table(tables$total[[1L]], tables$direct[[1L]],
                        tables$indirect[[1L]], label = methods[1L],
                        p_value = p_value)

  } else if (orientation == "portrait") {

    # add each table under the previous one
    start <- 0L
    df_list <- vector("list", length = n_methods)
    # prepare tables
    for (i in seq_len(n_methods)) {
      df_list[[i]] <- prepare_table(tables$total[[i]], tables$direct[[i]],
                                    tables$indirect[[i]], label = methods[i],
                                    p_value = p_value, start = start)
      start <- start + nrow(df_list[[i]])
    }
    # put tables for methods together
    df <- do.call(rbind, df_list)
    # update attributes
    attr(df, "label_rows") <- sapply(df_list, attr, "label_rows")
    attr(df, "total_header_rows") <- sapply(df_list, attr, "total_header_rows")
    attr(df, "direct_header_rows") <- sapply(df_list, attr, "direct_header_rows")
    attr(df, "indirect_header_rows") <- sapply(df_list, attr, "indirect_header_rows")
    attr(df, "merged_cells") <- do.call(c, lapply(df_list, attr, "merged_cells"))

  } else {

    # put two tables next to each other and add the next ones underneath
    have_even <- n_methods %% 2L == 0L
    n_rows <- ceiling(n_methods / 2)
    start <- 0L
    df_list <- vector("list", length = n_rows)
    # prepare tables
    for (i in seq_len(n_rows)) {
      # relevant indices
      i_right <- 2L * i
      i_left <- i_right - 1L
      # prepare current row of tables
      if (i < n_rows || have_even) {
        # prepare left and right tables of current row
        df_list[[i]] <- prepare_tables(tables$total[[i_left]],
                                       tables$direct[[i_left]],
                                       tables$indirect[[i_left]],
                                       methods[i_left],
                                       total_right = tables$total[[i_right]],
                                       direct_right = tables$direct[[i_right]],
                                       indirect_right = tables$indirect[[i_right]],
                                       label_right = methods[i_right],
                                       p_value = p_value, start = start)
        start <- start + nrow(df_list[[i]])
      } else {
        # last row and uneven number of tables: only one table left to prepare
        df_list[[i]] <- prepare_tables(tables$total[[i_left]],
                                       tables$direct[[i_left]],
                                       tables$indirect[[i_left]],
                                       methods[i_left],
                                       p_value = p_value,
                                       start = start)
      }
    }
    # put tables for methods together
    df <- do.call(rbind, df_list)
    p <- ncol(df) %/% 2
    # extract some relevant information
    attr(df, "label_rows") <- sapply(df_list, attr, "label_rows")
    attr(df, "total_header_rows") <- sapply(df_list, attr, "total_header_rows")
    attr(df, "direct_header_rows") <- sapply(df_list, attr, "direct_header_rows")
    attr(df, "indirect_header_rows") <- sapply(df_list, attr, "indirect_header_rows")
    attr(df, "merged_cells") <- do.call(c, lapply(df_list, attr, "merged_cells"))

  }

  # create flextable
  info <- tables[c("x", "m", "y", "covariates", "n", "R")]
  mediation_flextable(df, info = info, orientation = orientation)

}


# Theme for formatting a flextable of results from mediation analysis -----

#' Theme for formatting a flextable of results from (robust) mediation analysis
#'
#' Apply a theme to a \code{\link[flextable]{flextable}} intended to format
#' results from (robust) mediation analysis.  The theme uses additional
#' information from subclass \code{"\link[=to_flextable]{mediation_flextable}"},
#' and it formats the table according to APA style.
#'
#' Theme functions for \code{\link[flextable]{flextable}}s are not like
#' \pkg{ggplot2} themes, as they are applied to the existing table immediately.
#' For example, if you add a row in the footer after setting the theme, the new
#' row is not formatted with the theme.  The theme is applied only to existing
#' elements when the function is called.
#'
#' That is, if you modify the table returned by \code{\link{to_flextable}()},
#' it may be necessary to apply the theme function again after all elements of
#' the table have been added (e.g., additional header or footer rows).
#'
#' @param x  an object of class \code{"\link[flextable]{flextable}"}.  It is
#' recommend to use this theme only for objects of subclass
#' \code{"mediation_flextable"}, as returned by \code{\link{to_flextable}()}.
#' @param \dots  additional arguments are currently ignored.
#'
#' @return
#' An object inheriting from class \code{"\link[flextable]{flextable}"}.
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
#' # add additional row to the footer
#' footer_line <- paste("Additional explanations on the conducted",
#'                      "analysis.")
#' ft <- flextable::add_footer_lines(ft, values = footer_line)
#' ft <- theme_mediation(ft)
#' ft
#'
#' @importFrom flextable align border_remove fix_border_issues
#' get_flextable_defaults hline hline_bottom hline_top ncol_keys
#' nrow_part valign
#' @importFrom officer fp_border
#' @export

theme_mediation <- function(x, ...) {
  # initializations
  if (!inherits(x, "flextable")) {
    stop("function 'theme_mediation()' supports only \"flextable\" objects")
  }
  have_mediation <- inherits(x, "mediation_flextable")
  # define border properties
  defaults <- flextable::get_flextable_defaults()
  std_border <- officer::fp_border(color = defaults$border.color,
                                   style = "solid", width = 1L)
  # get relevant table dimensions
  n <- flextable::nrow_part(x, part <- "body")
  p <- flextable::ncol_keys(x)
  # set borders
  x <- flextable::border_remove(x)
  if (n > 0L) {
    x <- flextable::hline_top(x, border = std_border, part = "body")
    x <- flextable::hline_bottom(x, border = std_border, part = "body")
    # set borders for additional header rows
    if (have_mediation) {
      if (length(x$label_rows) == 0L) offset <- 1L
      else {
        # find which merged cells involve only one row
        merged_rows <- lapply(x$merged_cells, "[[", "i")
        keep <- sapply(merged_rows, length) == 1L
        # for each label row, obtain the corresponding merged columns
        label_cols <- lapply(x$label_rows, function(i, rows, cells) {
          col_list <- lapply(cells[rows == i], function(cell) {
            seq(from = cell$j1, to = cell$j2)
          })
          do.call(c, col_list)
        }, rows = unlist(merged_rows[keep]), cells = x$merged_cells[keep])
        # add partial lines for method labels
        for (i in seq_along(x$label_rows)) {
          x <- flextable::hline(x, i = x$label_rows[i], j = label_cols[[i]],
                                border = std_border, part = "body")
        }
        # offset for full line indicating new method
        offset <- 2L
      }
      # add lines for headers for total, direct, or indirect effects
      i_lines <- c(x$total_header_rows[-1L] - offset, x$total_header_rows,
                   x$direct_header_rows - 1L, x$direct_header_rows,
                   x$indirect_header_rows - 1L, x$indirect_header_rows)
      x <- flextable::hline(x, i = i_lines, border = std_border, part = "body")
    }
  }
  # set horizontal alignment
  if (p > 0L) x <- flextable::align(x, j = 1L, align = "left", part = "body")
  if (p > 1L) {
    x <- flextable::align(x, j = seq(from = 2L, to = p), align = "right",
                          part = "body")
  }
  x <- flextable::align(x, align = "justify", part = "footer")
  # set horizontal alignment of merged cells
  if (have_mediation) {
    # loop over merged cells
    for (merged_cell in x$merged_cells) {
      x <- flextable::align(x, i = merged_cell$i, j = merged_cell$j1,
                            align = "center", part = "body")
    }
  }
  # set vertical alignment
  x <- flextable::valign(x, valign = "center", part = "all")
  # return flextable
  flextable::fix_border_issues(x, part = "all")
}


# Internal functions -----

## format headers in a flextable using italics for certain symbols
## object ... a flextable object
## values ... character string giving the unformatted values of the header
## i ........ integer giving the row of the flextable to format
#' @importFrom flextable compose as_i as_paragraph
format_header_flextable <- function(object, values, i) {
  # find columns to be formatted
  to_format <- c(grep("Statistic", values, fixed = TRUE),
                 grep("Value", values, fixed = TRUE))
  # split corresponding values in header by space
  value_list <- strsplit(values[to_format], split = " ", fixed = TRUE)
  # loop over columns and format the corresponding cells
  for (which in seq_along(to_format)) {
    j <- to_format[which]
    values <- value_list[[which]]
    object <- flextable::compose(
      object, i = i, j = j,
      value = flextable::as_paragraph(flextable::as_i(values[1L]),
                                      " ", values[2L]),
      part = "body"
    )
  }
  # return flextable with formatted row
  object
}

## format the label column of a flextable using subscripts for indices
#' @importFrom flextable compose as_paragraph as_sub
format_labels_flextable <- function(object, values, j = 1L) {
  # find rows to be formatted
  to_format <- grep("[0-9]+", values, fixed = FALSE)
  # extract text chunks and index chunks
  text_list <- strsplit(values[to_format], split = "[0-9]+", fixed = FALSE)
  indices_list <- strsplit(values[to_format], split = "[^0-9]+", fixed = FALSE)
  # construct list of values to be used in compose(): each element is in turn
  # a list to be supplied to as_paragraph()
  value_list <- mapply(function(index_chunks, text_chunks) {
    # construct list of chunks: index chunks are put in subscripts
    chunk_list <- mapply(function(index_chunk, text_chunk) {
      if (index_chunk == "") list(text_chunk)
      else {
        index_chunk <- flextable::as_sub(index_chunk)
        list(index_chunk, text_chunk)
      }
    }, index_chunk = index_chunks, text_chunk = text_chunks,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
    # make sure we don't have a nested list
    do.call(c, chunk_list)
  }, index_chunks = indices_list, text_chunks = text_list,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)
  # loop over rows and format the corresponding cells
  for (which in seq_along(to_format)) {
    i <- to_format[which]
    list_values <- value_list[[which]]
    object <- flextable::compose(
      object, i = i, j = j,
      value = flextable::as_paragraph(list_values = list_values),
      part = "body"
    )
  }
  # return flextable with formatted label column
  object
}

# format the label column of a table using nicer unicode symbols
format_labels_unicode <- function(labels) {
  # format arrows and ellipses nicely
  labels <- gsub("->", "\U2192", labels, fixed = TRUE)  # alternative: \U2B62
  labels <- gsub("...", "\U2026", labels, fixed = TRUE)
  # return formatted labels
  labels
}

# format a column of a table using nicer unicode symbols
format_values_unicode <- function(column) {
  # remove leading spaces and format minus signs nicely
  column <- gsub("^ +", "", column, fixed = FALSE)
  column <- gsub("-", "\U2212", column, fixed = TRUE)
  # return formatted column
  column
}

## construct a flextable of subclass "mediation_flextable"
#' @importFrom flextable add_footer_lines as_paragraph autofit flextable
#' merge_h_range set_header_labels
mediation_flextable <- function(data, info, orientation = NULL, ...) {
  # extract attributes
  label_rows <- attr(data, "label_rows")
  total_header_rows <- attr(data, "total_header_rows")
  direct_header_rows <- attr(data, "direct_header_rows")
  indirect_header_rows <- attr(data, "indirect_header_rows")
  merged_cells <- attr(data, "merged_cells")
  # format the table body with nicer unicode symbols
  data[, 1L] <- format_labels_unicode(data[, 1L])
  data[, -1L] <- lapply(data[, -1L], format_values_unicode)
  # create flextable
  ft <- flextable::flextable(data, ...)
  # delete header since it contains generic column names
  ft <- flextable::delete_part(ft, part = "header")
  # format headers
  for (i in c(total_header_rows, direct_header_rows, indirect_header_rows)) {
    header <- unlist(data[i, , drop = TRUE])
    ft <- format_header_flextable(ft, values = header, i = i)
  }
  # ensure that indices in effect paths and symbols are in subscripts
  ft <- format_labels_flextable(ft, values = data[, 1L], j = 1L)
  # merge cells for confidence intervals
  for (merged_cell in merged_cells) {
    ft <- flextable::merge_h_range(ft, i = merged_cell$i, j1 = merged_cell$j1,
                                   j2 = merged_cell$j2, part = "body")
  }
  # make sure that columns fit nicely
  ft <- flextable::autofit(ft)
  # prepare note
  note_list <- get_table_note(x = info$x, m = info$m, y = info$y,
                              covariates = info$covariates, n = info$n,
                              R = info$R, type = "flextable")
  # add paragraph to flextable
  footnote <- flextable::as_paragraph(list_values = note_list)
  ft <- flextable::add_footer_lines(ft, values = footnote)
  # add attributes as components
  if (length(label_rows) > 0L) ft$label_rows <- label_rows
  ft$total_header_rows <- total_header_rows
  ft$direct_header_rows <- direct_header_rows
  ft$indirect_header_rows <- indirect_header_rows
  if (length(merged_cells) > 0L) ft$merged_cells <- merged_cells
  if (!is.null(orientation)) ft$orientation <- orientation
  # set class and theme to return flextable
  class(ft) <- c("mediation_flextable", class(ft))
  theme_mediation(ft)
}

# prepare table for indirect effect
prepare_indirect_table <- function(indirect, p_extra = 0L, p_value = FALSE,
                                   position = NULL) {

  # initializations
  n_indirect <- nrow(indirect)
  # for tables in landscape mode, we need an additional empty column
  if (!is.null(position)) empty <- get_empty_df(n_indirect, 1L)

  # Note: The code is relatively complex with lots of if() statements.  But we
  # have to treat all cases here because we use empty column names.  Taking a
  # subset of columns with [, ] would generate non-empty column names again,
  # and it doesn't seem that this behavior can be suppressed.

  # if we need to add empty columns, we have a confidence interval from a
  # bootstrap test (potentially with a p value as well)
  if (p_extra > 0L) {

    # create a data frame with empty columns
    extra <- get_empty_df(n_indirect, p_extra)
    # add empty columns to data frame for indirect effects
    if (p_value) {
      # if we have a p value, we need to add the empty columns before
      p_indirect <- ncol(indirect)
      if (is.null(position)) {
        # no extra empty column
        indirect <- cbind(indirect[, -p_indirect, drop = FALSE], extra,
                          indirect[, p_indirect, drop = FALSE],
                          fix.empty.names = FALSE)
      } else if (position == "left") {
        # extra empty column after label column
        indirect <- cbind(indirect[, 1L, drop = FALSE], empty,
                          indirect[, -c(1L, p_indirect), drop = FALSE],
                          extra, indirect[, p_indirect, drop = FALSE],
                          fix.empty.names = FALSE)
      } else {
        # extra empty column instead of label column
        indirect <- cbind(empty, indirect[, -c(1L, p_indirect), drop = FALSE],
                          extra, indirect[, p_indirect, drop = FALSE],
                          fix.empty.names = FALSE)
      }
    } else {
      # otherwise add empty columns at the end
      if (is.null(position)) {
        # no extra empty column
        indirect <- cbind(indirect, extra, fix.empty.names = FALSE)
      } else if (position == "left") {
        # extra empty column after label column
        indirect <- cbind(indirect[, 1L, drop = FALSE], empty,
                          indirect[, -1L, drop = FALSE], extra,
                          fix.empty.names = FALSE)
      } else {
        # extra empty column instead of label column
        indirect <- cbind(empty, indirect[, -1L, drop = FALSE], extra,
                          fix.empty.names = FALSE)
      }
    }

  } else if (!is.null(position)) {

    # no extra columns and merged cells for confidence intervals
    if (position == "left") {
      # extra empty column after label column
      indirect <- cbind(indirect[, 1L, drop = FALSE], empty,
                        indirect[, -1L, drop = FALSE],
                        fix.empty.names = FALSE)
    } else {
      # extra empty column instead of label column
      indirect <- cbind(empty, indirect[, -1L, drop = FALSE],
                        fix.empty.names = FALSE)
    }
  }

  # return table for indirect effect
  indirect
}

# put data frames of effects together for a single method
prepare_table <- function(total, direct, indirect, label = NULL,
                          p_value = FALSE,  start = 0L) {
  ## initializations
  p <- ncol(total)
  col_keys <- paste("Column", seq_len(p), sep = "_")
  ## if supplied, start with header for label
  if (is.null(label)) {
    df <- NULL
    i_label <- NULL
    label_cells <- NULL
  } else {
    # obtain row index and header
    i_label <- start + 1L
    label_header <- c("", label, rep.int("", p-2L))
    # add header to data frame
    names(label_header) <- col_keys
    df <- as.data.frame(as.list(label_header), stringsAsFactors = FALSE)
    # indicate cells to be merged in header
    label_cells <- list(list(i = i_label, j1 = 2L, j2 = p))
  }
  ## add header and body for total effects
  # obtain row index and header
  i_total <- start + NROW(df) + 1L
  total_header <- names(total)
  # add header and body to data frame
  names(total_header) <- names(total) <- col_keys
  df <- rbind(df, total_header, total, stringsAsFactors = FALSE)
  ## add header and body for direct effects
  # obtain row index and header
  i_direct <- start + nrow(df) + 1L
  direct_header <- names(direct)
  # add header and body to data frame
  names(direct_header) <- names(direct) <- col_keys
  df <- rbind(df, direct_header, direct, stringsAsFactors = FALSE)
  ## add header and body for indirect effects
  # obtain row index
  i_indirect <- start + nrow(df) + 1L
  # prepare data frame (adds empty columns where necessary)
  p_extra <- p - ncol(indirect)
  indirect <- prepare_indirect_table(indirect, p_extra = p_extra,
                                     p_value = p_value)
  indirect_header <- names(indirect)
  # determine cells to be merged for confidence intervals
  if (p_extra == 0L) ci_cells <- NULL
  else {
    i_merge <- seq(from = i_indirect, length.out = nrow(indirect) + 1L)
    j_ci <- grep("Confidence Interval", indirect_header, fixed = TRUE)
    ci_cells <- list(list(i = i_merge, j1 = j_ci, j2 = j_ci + p_extra))
  }
  # add header and body to data frame
  names(indirect_header) <- names(indirect) <- col_keys
  df <- rbind(df, indirect_header, indirect, stringsAsFactors = FALSE)
  ## add attributes for relevant information
  attr(df, "label_rows") <- i_label
  attr(df, "total_header_rows") <- i_total
  attr(df, "direct_header_rows") <- i_direct
  attr(df, "indirect_header_rows") <- i_indirect
  attr(df, "merged_cells") <- c(label_cells, ci_cells)
  ## return data frame
  df
}

# put data frames of effects together for two methods side by side
prepare_tables <- function(total_left, direct_left, indirect_left, label_left,
                           total_right = NULL, direct_right = NULL,
                           indirect_right = NULL, label_right = NULL,
                           p_value = FALSE, start = 0L) {
  ## initializations
  p <- ncol(total_left)
  col_keys <- paste("Column", seq_len(2L*p+1L), sep = "_")
  have_right <- !is.null(total_right) && !is.null(direct_right) &&
    !is.null(indirect_right) && !is.null(label_right)
  ## if supplied, start with header for label
  # obtain row index and header
  i_label <- start + 1L
  if (have_right) {
    # construct header
    label_header <- c("", "", label_left, rep.int("", p-2L),
                      "", label_right, rep.int("", p-2L))
    # indicate cells to be merged for header
    label_cells <- list(list(i = i_label, j1 = 3L, j2 = p+1L),
                        list(i = i_label, j1 = p+3L, j2 = 2L*p+1L))
  } else {
    # construct header
    label_header <- c("", "", label_left, rep.int("", 2*p-2L))
    # indicate cells to be merged for header
    label_cells <- list(list(i = i_label, j1 = 3L, j2 = p+1L))
  }
  # add header to data frame
  names(label_header) <- col_keys
  df <- as.data.frame(as.list(label_header), stringsAsFactors = FALSE)
  ## add header and body for total effects
  # obtain row index
  i_total <- start + nrow(df) + 1L
  # combine left and right tables
  n_total <- nrow(total_left)
  empty <- get_empty_df(n_total, 1L)
  if (have_right) total_right <- total_right[, -1L, drop = FALSE]
  else total_right <- get_empty_df(n_total, p-1L)
  total <- cbind(total_left[, 1L, drop = FALSE], empty,
                 total_left[, -1L, drop = FALSE], empty,
                 total_right, fix.empty.names = FALSE)
  # add header and body to data frame
  total_header <- names(total)
  names(total_header) <- names(total) <- col_keys
  df <- rbind(df, total_header, total, stringsAsFactors = FALSE)
  ## add header and body for direct effects
  # obtain row index
  i_direct <- start + nrow(df) + 1L
  # combine left and right tables
  n_direct <- nrow(direct_left)
  empty <- get_empty_df(n_direct, 1L)
  if (have_right) direct_right <- direct_right[, -1L, drop = FALSE]
  else direct_right <- get_empty_df(n_direct, p-1L)
  direct <- cbind(direct_left[, 1L, drop = FALSE], empty,
                  direct_left[, -1L, drop = FALSE], empty,
                  direct_right, fix.empty.names = FALSE)
  # add header and body to data frame
  direct_header <- names(direct)
  names(direct_header) <- names(direct) <- col_keys
  df <- rbind(df, direct_header, direct, stringsAsFactors = FALSE)
  ## add header and body for indirect effects
  # obtain row index
  i_indirect <- start + nrow(df) + 1L
  # prepare left table (adds empty columns where necessary)
  n_indirect <- nrow(indirect_left)
  p_extra_left <- p - ncol(indirect_left)
  indirect_left <- prepare_indirect_table(indirect_left, p_extra = p_extra_left,
                                          p_value = p_value, position = "left")
  # prepare right table (adds empty columns where necessary)
  if (have_right) {
    p_extra_right <- p - ncol(indirect_right)
    indirect_right <- prepare_indirect_table(indirect_right,
                                             p_extra = p_extra_right,
                                             p_value = p_value,
                                             position = "right")
  } else {
    p_extra_right <- 0L
    indirect_right <- get_empty_df(n_indirect, p)
  }
  # combine left and right tables and obtain header
  indirect <- cbind(indirect_left, indirect_right, fix.empty.names = FALSE)
  indirect_header <- names(indirect)
  # determine which cells need to be merged for confidence intervals
  p_extra <- c(p_extra_left, p_extra_right)
  have_ci <- p_extra > 0L
  if (any(have_ci)) {
    i_merge <- seq(from = i_indirect, length.out = n_indirect + 1L)
    j_ci <- grep("Confidence Interval", indirect_header, fixed = TRUE)
    ci_cells <- mapply(function(j1, width) {
      list(i = i_merge, j1 = j1, j2 = j1 + width)
    }, j1 = j_ci, width = p_extra[have_ci], SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else ci_cells <- NULL
  # add header and body to data frame
  names(indirect_header) <- names(indirect) <- col_keys
  df <- rbind(df, indirect_header, indirect, stringsAsFactors = FALSE)
  ## add attributes for relevant information
  attr(df, "label_rows") <- i_label
  attr(df, "total_header_rows") <- i_total
  attr(df, "direct_header_rows") <- i_direct
  attr(df, "indirect_header_rows") <- i_indirect
  attr(df, "merged_cells") <- c(label_cells, ci_cells)
  ## return data frame
  df
}

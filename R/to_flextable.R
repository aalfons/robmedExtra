# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Convert results from mediation analysis to a flextable -----

#' @export
to_flextable <- function(object, ...) UseMethod("to_flextable")

#' @export
to_flextable.test_mediation <- function(object, type = c("boot", "data"), ...) {
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # call method for summary
  to_flextable(summary, ...)
}

## Formatting numbers is not done via flextable defaults, but instead via
## formatC() to have consistency between to_flextable() and to_latex().
## Arguments are passed down to formatC() via '...' (such as 'digits' for the
## number of digits), but some of the defaults are different.  In addition,
## argument 'big.mark' is ignored for the numbers in the table and only used
## for the sample size and number of bootstrap samples in the table note.
#' @importFrom flextable add_footer_lines flextable merge_h_range
#' @export
to_flextable.summary_test_mediation <- function(object, p_value = FALSE, ...) {
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, ...)

  # # start with data frame for total effects
  # df <- tables$total
  # # add header and body for direct effects
  # i_direct <- nrow(df) + 1L
  # direct <- tables$direct
  # direct_header <- names(direct)
  # names(direct_header) <- names(direct) <- names(df)
  # df <- rbind(df, direct_header, direct)
  # # add header and body for indirect effects
  # i_indirect <- nrow(df) + 1L
  # indirect <- tables$indirect
  # indirect_header <- names(indirect)
  # p_indirect <- ncol(indirect)
  # p_extra <- ncol(df) - p_indirect
  # if (p_extra > 0L) {
  #   n_indirect <- nrow(indirect)
  #   extra_header <- rep.int("", p_extra)
  #   extra <- replicate(p_extra, rep.int("", n_indirect), simplify = FALSE)
  #   if (p_value) {
  #     indirect_header <- c(indirect_header[-p_indirect], extra_header,
  #                          indirect_header[p_indirect])
  #     indirect <- cbind(indirect[, -p_indirect, drop = FALSE], extra,
  #                       indirect[, p_indirect, drop = FALSE])
  #   } else {
  #     indirect_header <- c(indirect_header, extra_header)
  #     indirect <- cbind(indirect, extra)
  #   }
  # }
  # names(indirect_header) <- names(indirect) <- names(df)
  # df <- rbind(df, indirect_header, indirect)

  # put data frames of effects together
  df <- prepare_flextable(tables$total, tables$direct, tables$indirect,
                          p_value = p_value)
  # extract some relevant information
  i_direct <- attr(df, "direct")
  direct_header <- unlist(df[i_direct, , drop = TRUE])
  i_indirect <- attr(df, "indirect")
  indirect_header <- unlist(df[i_indirect, , drop = TRUE])
  merge_args <- attr(df, "merge_args")
  have_ci <- !is.null(merge_args)
  # format the table body with nicer unicode symbols
  df[, 1L] <- format_labels_unicode(df[, 1L])
  df[, -1L] <- lapply(df[, -1L], format_values_unicode)
  # construct flextable
  ft <- flextable::flextable(df)
  # format headers
  ft <- format_header_flextable(ft, values = names(df), i = NULL)
  ft <- format_header_flextable(ft, values = direct_header, i = i_direct)
  ft <- format_header_flextable(ft, values = indirect_header, i = i_indirect)
  # ensure that indices in effect paths and symbols are in subscripts
  ft <- format_labels_flextable(ft, values = df[, 1L], j = 1L)
  # merge cells for confidence intervals
  # if (p_extra > 0L) {
  #   i_merge <- seq(from = i_indirect, length.out = n_indirect + 1L)
  #   j_ci_start <- grep("Confidence Interval", indirect_header, fixed = TRUE)
  #   j_ci_end <- j_ci_start + p_extra
  #   ft <- flextable::merge_h_range(ft, i = i_merge, j1 = j_ci_start,
  #                                  j2 = j_ci_end, part = "body")
  # }
  if (have_ci) {
    ft <- flextable::merge_h_range(ft, i = merge_args$i, j1 = merge_args$j1,
                                   j2 = merge_args$j2, part = "body")

  }
  # make sure that columns fit nicely
  ft <- flextable::autofit(ft)
  # add table note
  note_list <- get_table_note(x = tables$x, m = tables$m, y = tables$y,
                              covariates = tables$covariates, n = tables$n,
                              R = tables$R, type = "flextable")
  ft <- flextable::add_footer_lines(
    ft,
    values = flextable::as_paragraph(list_values = note_list)
  )
  # add information on rows where direct and indirect effects start
  ft$additional_header_rows <- c(direct = i_direct, indirect = i_indirect)
  # if applicable, add information on merged cells for confidence intervals
  if (have_ci) {
    # ft$merge_h_range <- list(i = i_merge, j1 = j_ci_start, j2 = j_ci_end)
    ft$merge_args <- merge_args
  }
  # set class and theme to return flextable
  class(ft) <- c("mediation_flextable", class(ft))
  theme_mediation(ft)
}


# put data frames of effects together for a single method
prepare_flextable <- function(total, direct, indirect, p_value = FALSE) {
  ## start with data frame for total effects
  df <- total
  ## add header and body for direct effects
  # obtain index and header
  i_direct <- nrow(df) + 1L
  direct_header <- names(direct)
  # add header and body to data frame
  names(direct_header) <- names(direct) <- names(df)
  df <- rbind(df, direct_header, direct)
  ## add header and body for indirect effects
  # obtain index and header
  i_indirect <- nrow(df) + 1L
  indirect_header <- names(indirect)
  # determine number of empty columns to be added
  p_indirect <- ncol(indirect)
  p_extra <- ncol(df) - p_indirect
  # if we need to add empty columns, we have a confidence interval from a
  # bootstrap test (potentially with a p value as well)
  if (p_extra > 0L) {
    # create a data frame with empty columns
    n_indirect <- nrow(indirect)
    extra <- get_empty_df(n_indirect, p_extra)
    # add empty columns to data frame for indirect effects
    if (p_value) {
      # if we have a p value, we need to add the empty columns before
      indirect_header <- c(indirect_header[-p_indirect], names(extra),
                           indirect_header[p_indirect])
      indirect <- cbind(indirect[, -p_indirect, drop = FALSE], extra,
                        indirect[, p_indirect, drop = FALSE])
    } else {
      # otherwise add empty columns at the end
      indirect_header <- c(indirect_header, names(extra))
      indirect <- cbind(indirect, extra)
    }
    # determine which cells need to be merged for confidence intervals
    i_merge <- seq(from = i_indirect, length.out = n_indirect + 1L)
    j_ci_start <- grep("Confidence Interval", indirect_header, fixed = TRUE)
    j_ci_end <- j_ci_start + p_extra
  }
  # add header and body to data frame
  names(indirect_header) <- names(indirect) <- names(df)
  df <- rbind(df, indirect_header, indirect)
  ## add relevant information as attributes
  attr(df, "direct") <- i_direct
  attr(df, "indirect") <- i_indirect
  if (p_extra > 0L) {
    attr(df, "merge_args") <- list(i = i_merge, j1 = j_ci_start, j2 = j_ci_end)
  }
  ## return data frame
  df
}


# Theme for formatting a flextable of results from mediation analysis -----

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
  n_header <- flextable::nrow_part(x, part <- "header")
  n_body <- flextable::nrow_part(x, part <- "body")
  p <- flextable::ncol_keys(x)
  # set borders
  x <- flextable::border_remove(x)
  if (n_header > 0L) {
    x <- flextable::hline_top(x, border = std_border, part = "header")
    x <- flextable::hline_bottom(x, border = std_border, part = "header")
  }
  if (n_body > 0L) {
    if (n_header == 0L) {
      x <- flextable::hline_top(x, border = std_border, part = "body")
    }
    x <- flextable::hline_bottom(x, border = std_border, part = "body")
    # set borders for additional header rows
    if (have_mediation) {
      i_hline <- rbind(x$additional_header_rows - 1L, x$additional_header_rows)
      x <- flextable::hline(x, i = i_hline, border = std_border, part = "body")
    }
  }
  # set horizontal alignment
  if (p > 0L) x <- flextable::align(x, j = 1L, align = "left", part = "all")
  if (p > 1L) {
    j_right <- seq(from = 2L, to = p)
    x <- flextable::align(x, j = j_right, align = "right", part = "header")
    x <- flextable::align(x, j = j_right, align = "right", part = "body")
  }
  x <- flextable::align(x, align = "justify", part = "footer")
  # set horizontal alignment of merged cells
  # if (have_mediation && !is.null(x$merge_h_range)) {
  #   x <- flextable::align(x, i = x$merge_h_range$i, j = x$merge_h_range$j1,
  #                         align = "center", part = "body")
  # }
  if (have_mediation && !is.null(merge_args <- x$merge_args)) {
    x <- flextable::align(x, i = merge_args$i, j = merge_args$j1,
                          align = "center", part = "body")
  }
  # set vertical alignment
  x <- flextable::valign(x, valign = "bottom", part = "header")
  x <- flextable::valign(x, valign = "center", part = "body")
  x <- flextable::valign(x, valign = "center", part = "footer")
  # return flextable
  flextable::fix_border_issues(x, part = "all")
}


# Internal functions -----

## format headers in a flextable using italics for certain symbols
## object ... a flextable object
## values ... character string giving the unformatted values of the header
## i ........ integer giving the row of the flextable to format
#' @importFrom flextable compose as_i as_paragraph
format_header_flextable <- function(object, values, i = NULL) {
  # initializations
  part <- if (is.null(i)) "header" else "body"
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
      part = part
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

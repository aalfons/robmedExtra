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
#' @importFrom flextable flextable
#' @export
to_flextable.summary_test_mediation <- function(object, p_value = FALSE, ...) {
  # call workhorse function to format tables for effects
  tables <- get_mediation_tables(object, p_value = p_value, ...)
  # put data frames of effects together
  df <- prepare_table(tables$total, tables$direct, tables$indirect,
                      p_value = p_value)
  # extract some relevant information
  direct_header_rows <- attr(df, "direct_header_rows")
  indirect_header_rows <- attr(df, "indirect_header_rows")
  merged_cells <- attr(df, "merged_cells")
  # construct flextable
  ft <- flextable::flextable(df)
  # format flextable nicely
  ft <- format_flextable(ft, direct_header_rows = direct_header_rows,
                         indirect_header_rows = indirect_header_rows,
                         merged_cells = merged_cells)
  # add table note
  ft <- add_note_flextable(ft, x = tables$x, m = tables$m, y = tables$y,
                           covariates = tables$covariates, n = tables$n,
                           R = tables$R)
  # add information on rows where direct and indirect effects start
  ft$direct_header_rows <- direct_header_rows
  ft$indirect_header_rows <- indirect_header_rows
  # if applicable, add information on merged cells for confidence intervals
  if (length(merged_cells) > 0L) ft$merged_cells <- merged_cells
  # set class and theme to return flextable
  class(ft) <- c("mediation_flextable", class(ft))
  theme_mediation(ft)
}


#' @export
to_flextable.list <- function(object, type = c("boot", "data"), p_value = FALSE,
                              orientation = c("portrait", "landscape"), ...) {
  # check arguments
  orientation <- match.arg(orientation)
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, type = type, p_value = p_value, ...)
  # further initializations
  methods <- tables$methods
  n_methods <- length(methods)
  # construct flextable
  if (n_methods == 1L) {
    # put data frames of effects together
    df <- prepare_table(tables$total[[1L]], tables$direct[[1L]],
                        tables$indirect[[1L]], p_value = p_value)
    # extract some relevant information
    direct_header_rows <- attr(df, "direct_header_rows")
    indirect_header_rows <- attr(df, "indirect_header_rows")
    merged_cells <- attr(df, "merged_cells")
    # construct flextable
    ft <- flextable::flextable(df)
    # format flextable nicely
    ft <- format_flextable(ft, direct_header_rows = direct_header_rows,
                           indirect_header_rows = indirect_header_rows,
                           merged_cells = merged_cells)
    # add header row for method label
    p <- ncol(df)
    ft <- flextable::add_header_row(ft, values = c("", methods),
                                    colwidths =  c(1L, p-1L),
                                    top = TRUE)
    # add merged label cells to list of merged cells
    label_cells <- list(h = 1L, j1 = 2L, j2 = p, part = "header")
    merged_cells <- c(list(label_cells), merged_cells)
    # define columns for partial lines under method labels
    label_cols <- seq(from = 2L, to = p)
  } else if (orientation == "portrait") {
    stop("not implemented yet")
  } else {
    stop("not implemented yet")
  }
  # add table note
  ft <- add_note_flextable(ft, x = tables$x, m = tables$m, y = tables$y,
                           covariates = tables$covariates, n = tables$n,
                           R = tables$R)
  # add information on rows where direct and indirect effects start
  if (n_methods > 1L) ft$total_header_rows <- total_header_rows
  ft$direct_header_rows <- direct_header_rows
  ft$indirect_header_rows <- indirect_header_rows
  # add information on columns for partial lines under method labels
  ft$label_cols <- label_cols
  # add information on merged cells for confidence intervals
  ft$merged_cells <- merged_cells
  # add information on orientation of the table
  ft$orientation <- orientation
  # set class and theme to return flextable
  class(ft) <- c("mediation_flextable", class(ft))
  theme_mediation(ft)
}


# put data frames of effects together for a single method
prepare_table <- function(total, direct, indirect, p_value = FALSE) {
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
    ci_cells <- list(i = i_merge, j1 = j_ci_start, j2 = j_ci_end, part = "body")
  }
  # add header and body to data frame
  names(indirect_header) <- names(indirect) <- names(df)
  df <- rbind(df, indirect_header, indirect)
  ## format the table body with nicer unicode symbols
  df[, 1L] <- format_labels_unicode(df[, 1L])
  df[, -1L] <- lapply(df[, -1L], format_values_unicode)
  ## add attributes for relevant information
  attr(df, "direct_header_rows") <- i_direct
  attr(df, "indirect_header_rows") <- i_indirect
  if (p_extra > 0L) attr(df, "merged_cells") <- list(ci_cells)
  ## return data frame
  df
}

## format flextable: headers, labels, and merged cells
#' @importFrom flextable autofit merge_h_range
format_flextable <- function(ft, total_header_rows = NULL, direct_header_rows,
                             indirect_header_rows, merged_cells = NULL) {
  # extract underlying data frame
  df <- ft$body$dataset
  # format headers
  ft <- format_header_flextable(ft, values = names(df), i = NULL)
  for (i in c(total_header_rows, direct_header_rows, indirect_header_rows)) {
    current_header <- unlist(df[i, , drop = TRUE])
    ft <- format_header_flextable(ft, values = current_header, i = i)
  }
  # ensure that indices in effect paths and symbols are in subscripts
  ft <- format_labels_flextable(ft, values = df[, 1L], j = 1L)
  # merge cells for confidence intervals
  for (merged_cell in merged_cells) {
    ft <- flextable::merge_h_range(ft, i = merged_cell$i,
                                   j1 = merged_cell$j1,
                                   j2 = merged_cell$j2,
                                   part = merged_cell$part)
  }
  # make sure that columns fit nicely
  flextable::autofit(ft)
}

## add note to flextable
#' @importFrom flextable add_footer_lines as_paragraph
add_note_flextable <- function(object, x, m, y, covariates = character(),
                               n, R = NULL) {
  # prepare note
  note_list <- get_table_note(x = x, m = m, y = y, covariates = covariates,
                              n = n, R = R, type = "flextable")
  # add paragraph to flextable
  flextable::add_footer_lines(
    object,
    values = flextable::as_paragraph(list_values = note_list)
  )
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
    if (have_mediation && !is.null(label_cols <- x$label_cols)) {
      x <- flextable::hline(x, i = 1L, j = label_cols, border = std_border,
                            part = "header")
    }
    x <- flextable::hline_bottom(x, border = std_border, part = "header")
  }
  if (n_body > 0L) {
    if (n_header == 0L) {
      x <- flextable::hline_top(x, border = std_border, part = "body")
    }
    x <- flextable::hline_bottom(x, border = std_border, part = "body")
    # set borders for additional header rows
    if (have_mediation) {
      # TODO: borders for total header rows (above which we have the label,
      #       which should be separated by partial lines)
      i_hline <- c(x$direct_header_rows - 1L, x$direct_header_rows,
                   x$indirect_header_rows - 1L, x$indirect_header_rows)
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
  if (have_mediation && !is.null(merged_cells <- x$merged_cells)) {
    # extract relevant information on merged cells
    merged_cells <- x$merged_cells
    i_list <- lapply(merged_cells, "[[", "i")
    j <- sapply(merged_cells, "[[", "j1")
    part <- sapply(merged_cells, "[[", "part")
    # align any merged cells in table header
    which_header <- which(part == "header")
    if (length(which_header) > 0L) {
      x <- flextable::align(x, i = do.call(c, i_list[which_header]),
                            j = j[which_header], align = "center",
                            part = "header")
    }
    # align any merged cells in table body
    which_body <- which(part == "body")
    if (length(which_body) > 0L) {
      x <- flextable::align(x, i = do.call(c, i_list[which_body]),
                            j = j[which_body], align = "center",
                            part = "body")
    }
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

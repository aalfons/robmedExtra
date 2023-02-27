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

#' @importFrom flextable add_footer_lines as_i flextable merge_h_range
#' @export
to_flextable.summary_test_mediation <- function(object, p_value = FALSE,
                                                digits = 3L, ...) {
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, digits = digits)
  # start with data frame for total effects
  df <- tables$total
  # add header and body for direct effects
  i_direct <- nrow(df) + 1L
  direct <- tables$direct
  direct_header <- names(direct)
  names(direct_header) <- names(direct) <- names(df)
  df <- rbind(df, direct_header, direct)
  # add header and body for indirect effects
  indirect <- tables$indirect
  i_indirect <- nrow(df) + 1L
  p_extra <- ncol(df) - ncol(indirect)
  if (p_extra > 0L) {
    n_indirect <- nrow(indirect)
    indirect_header <- c(names(indirect), rep.int("", p_extra))
    extra <- replicate(p_extra, rep.int("", n_indirect), simplify = FALSE)
    indirect <- cbind(indirect, extra)
  }
  names(indirect_header) <- names(indirect) <- names(df)
  df <- rbind(df, indirect_header, indirect)
  # format the table body with nicer unicode symbols
  df[, 1L] <- format_unicode_label(df[, 1L])
  df[, -1L] <- lapply(df[, -1L], format_unicode_column)
  # construct flextable
  ft <- flextable::flextable(df)
  # merge cells for confidence intervals
  if (p_extra > 0L) {
    i_merge <- seq(from = i_indirect, length.out = n_indirect + 1L)
    j_ci_start <- grep("Confidence Interval", indirect_header, fixed = TRUE)
    j_ci_end <- j_ci_start + p_extra
    ft <- flextable::merge_h_range(ft, i = i_merge, j1 = j_ci_start,
                                   j2 = j_ci_end, part = "body")
  }
  # make sure that columns fit nicely
  ft <- flextable::autofit(ft)
  # add table note
  note <- paste(tables$note, collapse = " ")
  ft <- flextable::add_footer_lines(
    ft,
    values = flextable::as_paragraph(flextable::as_i("Note."), " ", note)
  )
  # add information on rows where direct and indirect effects start
  ft$additional_header_rows <- c(direct = i_direct, indirect = i_indirect)
  # if applicable, add information on merged cells for confidence intervals
  if (p_extra > 0L) {
    ft$merge_h_range <- list(i = i_merge, j1 = j_ci_start, j2 = j_ci_end)
  }
  # set class and theme to return flextable
  class(ft) <- c("mediation_flextable", class(ft))
  theme_mediation(ft)
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
  big_border <- officer::fp_border(color = defaults$border.color,
                                   style = "solid", width = 2L)
  # get relevant table dimensions
  n_header <- flextable::nrow_part(x, part <- "header")
  n_body <- flextable::nrow_part(x, part <- "body")
  p <- flextable::ncol_keys(x)
  # set borders
  x <- flextable::border_remove(x)
  if (n_header > 0L) {
    x <- flextable::hline_top(x, border = big_border, part = "header")
    x <- flextable::hline_bottom(x, border = big_border, part = "header")
  }
  if (n_body > 0L) {
    if (n_header == 0L) {
      x <- flextable::hline_top(x, border = big_border, part = "body")
    }
    x <- flextable::hline_bottom(x, border = big_border, part = "body")
    # set borders for additional header rows
    if (have_mediation) {
      i_hline <- rbind(x$additional_header_rows - 1L, x$additional_header_rows)
      x <- flextable::hline(x, i = i_hline, border = big_border, part = "body")
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
  if (have_mediation && !is.null(x$merge_h_range)) {
    x <- flextable::align(x, i = x$merge_h_range$i, j = x$merge_h_range$j1,
                          align = "center", part = "body")
  }
  # set vertical alignment
  x <- flextable::valign(x, valign = "bottom", part = "header")
  x <- flextable::valign(x, valign = "center", part = "body")
  x <- flextable::valign(x, valign = "center", part = "footer")
  # return flextable
  fix_border_issues(x, part = "all")
}


# Internal functions -----

# format a column of a table using nicer unicode symbols
format_unicode_column <- function(column) {
  gsub("-", "\U2212", column, fixed = TRUE)
}

# format the label column of a table using nicer unicode symbols
format_unicode_label <- function(label) {
  # format arrows and ellipses nicely
  label <- gsub("->", "\U2192", label, fixed = TRUE)  # alternative: \U2B62
  label <- gsub("...", "\U2026", label, fixed = TRUE)
  # return label
  label
}

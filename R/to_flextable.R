# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************

#' @export
to_flextable <- function(object, ...) UseMethod("to_flextable")

#' @export
to_flextable.test_mediation <- function(object, type = c("boot", "data"), ...) {
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # call method for summary
  to_flextable(summary, ...)
}

#' @importFrom flextable add_footer_lines align as_i flextable hline
#' merge_h_range valign
#' @export
to_flextable.summary_test_mediation <- function(object, p_value = FALSE,
                                                digits = 3L, align = NULL,
                                                ...) {
  # initializations
  if (is.null(align)) {
    align <- list(table = c("left", "right", "right", "right", "right"),
                  ci = "center", footer = "justify")
  }
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, digits = digits)
  # start with data frame for total effects
  df <- tables$total
  # add header and body for direct effects
  direct <- tables$direct
  i_hline <- nrow(df) + 0L:1L
  direct_header <- names(direct)
  names(direct_header) <- names(direct) <- names(df)
  df <- rbind(df, direct_header, direct)
  # add header and body for indirect effects
  indirect <- tables$indirect
  i_start <- nrow(df) + 1L
  i_hline <- c(i_hline, nrow(df) + 0L:1L)
  n_indirect <- nrow(indirect)
  p_extra <- ncol(df) - ncol(indirect)
  indirect_header <- c(names(indirect), rep.int("", p_extra))
  extra <- replicate(p_extra, rep.int("", n_indirect), simplify = FALSE)
  indirect <- cbind(indirect, extra)
  names(indirect_header) <- names(indirect) <- names(df)
  df <- rbind(df, indirect_header, indirect)
  # TODO: format the table body with nicer unicode symbols
  # construct flextable
  ft <- flextable::flextable(df)
  # add horizontal lines for headers of direct and indirect effects
  # FIXME: make sure that all borders look the same
  # (probably we should write a theme function that sets the borders)
  ft <- flextable::hline(ft, i = i_hline)
  # merge cells for confidence intervals
  i_merge <- seq(from = i_start, length.out = n_indirect + 1L)
  j_ci <- grep("Confidence Interval", indirect_header, fixed = TRUE)
  ft <- flextable::merge_h_range(ft, i = i_merge, j1 = j_ci,
                                 j2 = j_ci + p_extra, part = "body")
  # make sure that columns fit nicely
  ft <- flextable::autofit(ft)
  # add table note
  note <- paste(tables$note, collapse = " ")
  ft <- flextable::add_footer_lines(
    ft,
    values = flextable::as_paragraph(flextable::as_i("Note."), " ", note)
  )
  # set column alignment
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  for (j in seq_along(align$table)) {
    ft <- flextable::align(ft, j = j, align = align$table[j], part = "all")
  }
  ft <- flextable::align(ft, i = i_merge, j = j_ci, align = align$ci,
                         part = "body")
  ft <- flextable::align(ft, align = align$footer, part = "footer")
  # return flextable
  ft
}

#' @export
print.mediation_flextables <- function(x, ...) {
  # create preview of flextables
  print(x$total, ...)
  print(x$direct, ...)
  print(x$indirect, ...)
}

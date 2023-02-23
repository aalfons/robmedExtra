# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Convert results from mediation analysis to list of flextables -----

#' @export
to_flextable <- function(object, ...) UseMethod("to_flextable")

#' @export
to_flextable.test_mediation <- function(object, type = c("boot", "data"), ...) {
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # call method for summary
  to_flextable(summary, ...)
}

#' @export
to_flextable.summary_test_mediation <- function(object, p_value = FALSE,
                                                digits = 3L, ...) {
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, p_value = p_value, digits = digits)
  # further formatting of tables
  tables$total <- to_effect_table(tables$total, which = "flextable")
  tables$direct <- to_effect_table(tables$direct, which = "flextable")
  tables$indirect <- to_effect_table(tables$indirect, which = "flextable")
  # set class and return object containing flextables
  class(tables) <- "mediation_flextables"
  tables
}

#' @export
print.mediation_flextables <- function(x, ...) {
  # create preview of flextables
  print(x$total, ...)
  print(x$direct, ...)
  print(x$indirect, ...)
}

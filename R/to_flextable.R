#' Create flextable from mediation test
#'
#' Creates a flextable from a test_mediation object
#'
#' @param test_model an object inheriting from class
#' \code{"\link{test_mediation}"} or a list of objects of that class.
#' of object
#'
#' @param digits a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#'
#' @param merged boolean that determines whether the flextables are merged to show
#' the results of two tests in one table (except possibly the last one) or
#' seperate. Only used when test_model
#' is a list.
#'
#' @param p_values boolean that indicates whether the p-values for indirect
#' effects should be included in the flextable or not.
#' Default is to include the p-values
#'
#' @return An object of class \code{"\link{flextable}"} or a list of objects of
#' this class.
#'
#' @importFrom flextable theme_booktabs align bold add_header_row autofit
#' merge_at flextable add_footer_row hline
#'
#' @author Vincent Drenth
#'
#' @examples
#' data("BSG2014")
#'
#' boot_robust <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014)
#'
#' ft <- to_flextable(boot_robust)
#'
#' boot_ols <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014,
#'                               robust = FALSE)
#'
#' flextables_list <- to_flextable(list(boot_robust, boot_ols))
#'
#'
#' @export

to_flextable <- function(test_model, ...) {
  UseMethod("to_flextable")
}


#' @export
#' @method to_flextable test_mediation

to_flextable.test_mediation <- function(test_model, digits = 4, p_values = T) {

  tables_paths <- prep_data_table(test_model = test_model, digits = digits,
                                  p_values = p_values)

  df_stacked <- tables_paths[[1]]
  ft <- flextable(df_stacked)

  start_merge <- which(df_stacked[,1] == "Indirect Effects")
  path_values <- c(tables_paths[[2]] - 1, start_merge, start_merge - 1)


  # Merge the third and fourth column for the indirect effects to create one
  # column for the confidence interval
  indirect_range <- c(start_merge:(nrow(df_stacked)))

  if (p_values) {
    merge_range <- c(3:4)
  } else {
    merge_range <- c(3:5)
  }

  for (index in indirect_range) {
    ft <- merge_at(ft, i = index, j = merge_range)
  }

  # Changing cosmetics
  ft <- flextable::theme_booktabs(ft, bold_header = TRUE)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, align = "left", j = 1, part = "all")
  ft <- flextable::bold(ft, i = start_merge, bold = T)
  ft <- flextable::add_header_row(ft, top = TRUE, values = c(get_method_robmed(test_model)),
                                  colwidths = c(5))
  ft <- hline(ft, i = path_values, border = NULL, part = "body")

  for (row in c(1:(start_merge - 1))) {
    orig_text <- df_stacked[row,1]
    text_list <- strsplit(orig_text, " ")[[1]]

    effect <- paste(text_list[1:3], collapse = " ")
    letter_number <- strsplit(gsub("[()]", "", text_list[4]), "")[[1]]
    if (letter_number[2] == "'") {
      letter = paste0(letter_number[1:2], collapse = "")
      number = paste0(letter_number[3:length(letter_number)], collapse = "")
    } else {
      letter = letter_number[1]
      number = paste0(letter_number[2:length(letter_number)], collapse = "")
    }

    new_text <- as_paragraph(as_chunk(effect), as_chunk(" ("),
                             as_chunk(letter),
                             as_sub(number),
                             as_chunk(")"))

    ft <- flextable::compose(ft, i = row, j = 1, value = new_text)
  }

  ft <- autofit(ft)

  return(ft)
}

# Helper function used for shiny
to_flextable.name <- function(test_model, digits = 4,
                              p_values = T, ...) {
  tryCatch({model <- get(x = test_model)},
           error = function(cond) {
             message(paste("No object with name", test_model))
             return(NA)
           }
  )
  return(to_flextable(model, digits = digits, p_values = p_values))
}

#' @export
#' @method to_flextable list
to_flextable.list <- function(test_model, digits = 4, merged = F,
                              p_values = TRUE, ...) {
  result <- list()
  if(merged) {
    for (index in seq(1, ceiling((length(test_model) + 1)/2), 2)) {
      test_model_left <- test_model[[index]]
      test_model_right <- test_model[[index + 1]]

      ft <- merged_flextable(test_model_left, test_model_right)
      result <- append(result, list(ft))
    }

    if (length(test_model) %% 2 == 1) {
      ft <- to_flextable(test_model[[length(test_model)]], p_values = p_values)
      result <- append(result, list(ft))
    }

  } else {
    # Seperate tables for each method
    for(test in test_model) {
      ft <- to_flextable(test, digits = digits, p_values = p_values)
      result <- append(result, list(ft))
    }
  }

  return(result)
}

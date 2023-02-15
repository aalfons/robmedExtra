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

to_flextable <- function(object, ...) UseMethod("to_flextable")


#' @export
#' @method to_flextable test_mediation

to_flextable.test_mediation <- function(object, digits = 4, p_values = TRUE,
                                        ...) {

  tables_paths <- prep_data_table(test_model = object, digits = digits,
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
  ft <- flextable::add_header_row(ft, top = TRUE, values = c(get_method_robmed(object)),
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
to_flextable.name <- function(object, digits = 4, p_values = TRUE, ...) {
  tryCatch({model <- get(x = object)},
           error = function(cond) {
             message(paste("No object with name", object))
             return(NA)
           }
  )
  return(to_flextable(model, digits = digits, p_values = p_values))
}

#' @export
#' @method to_flextable list
to_flextable.list <- function(object, digits = 4, merged = FALSE,
                              p_values = TRUE, ...) {
  result <- list()
  if(merged) {
    for (index in seq(1, ceiling((length(object) + 1)/2), 2)) {
      object_left <- object[[index]]
      object_right <- object[[index + 1]]

      ft <- merged_flextable(object_left, object_right)
      result <- append(result, list(ft))
    }

    if (length(object) %% 2 == 1) {
      ft <- to_flextable(object[[length(object)]], p_values = p_values)
      result <- append(result, list(ft))
    }

  } else {
    # Seperate tables for each method
    for(test in object) {
      ft <- to_flextable(test, digits = digits, p_values = p_values)
      result <- append(result, list(ft))
    }
  }

  return(result)
}




# Creates a single dataframe that contains the data needed to create the table
# The 4th column contains NA for all rows corresponding to the indirect effects
# as the 3rd and 4th column will be later merged to create the column for
# the Confidence Interval

prep_data_table <- function(test_model, digits = 4, p_values = TRUE) {
  sm <- summary(test_model)$summary

  if (test_model$fit$model == "serial") {
    if (length(sm$m) == 1) {
      directrows <- 3 * (length(sm$x)) + 1
      indirectrows <- length(sm$x)

    } else if (length(sm$m) == 2) {
      # to generate table for 2 serial mediators
      directrows <- 4 * length(sm$x) + 3
      indirectrows <- 3 * length(sm$x)
    } else if (length(sm$m) == 3) {
      # Table for 3 serial mediators
      directrows <- 4 * length(sm$x) + 5 + 2
      indirectrows <- (3 + 3 + 1) * length(sm$x)
    }
  } else {
    #Model is parallel
    rows = 2 * (length(sm$x) * length(sm$m)) + 2 * length(sm$x) + length(sm$m)
    indirectrows = length(sm$x) * length(sm$m)
    directrows = rows - indirectrows
  }

  # Create dataframe for the direct effects
  df_dir <- data.frame(matrix(NA, nrow = directrows, ncol = 5))

  row <- 1
  for (med in sm$m) {
    if (length(sm$m) > 1){
      coefs_a <- sm$fit_mx[med][[1]]$coefficients
    } else {
      coefs_a <- sm$fit_mx$coefficients
    }

    #Add a paths
    for (reg in sm$x) {
      df_dir[row, 1] <- paste(reg,"->", med, paste0("(a", row, ")"))
      df_dir[row, 2:5] <- coefs_a[reg, 2:5]
      row <- row + 1
    }
  }
  a_paths <- row

  coefs_b <- sm$fit_ymx$coefficients
  for (med in sm$m) {
    #Add b paths
    df_dir[row, 1] <- paste(med ,"->" , sm$y,
                            paste0("(b", row - a_paths + 1, ")"))
    df_dir[row, 2:5] <- coefs_b[med, 2:5]
    row <- row + 1
  }
  b_paths <- row

  # Add c path (Direct effect)
  for (reg in sm$x){
    df_dir[row, 1] <- paste(reg,'->', sm$y,
                            paste0("(c", row - b_paths + 1, ")"))
    df_dir[row, 2:5] <- sm$direct[reg, 2:5]
    row <- row + 1
  }
  c_paths <- row

  # Add c' path (Total effect)
  for (reg in sm$x) {
    df_dir[row, 1] <- paste(reg, "->", sm$y,
                            paste0("(c'", row - c_paths + 1, ")"))
    df_dir[row, 2:5] <- sm$total[reg, 2:5]
    row <- row + 1
  }

  if (test_model$fit$model == "serial" && length(sm$m) > 1) {

    if (length(sm$m) == 2) {
      df_dir[row, 1] <- paste(paste(sm$m, collapse = "->"), "(d)")
      df_dir[row, 2:5] <- as.numeric(sm$fit_mx[sm$m[2]][[1]]$coefficients[sm$m[1],2:5])
      row <- row + 1

    } else if (length(sm$m) == 3) {
      d_paths <- names(test_model$d)

      for (path in d_paths) {
        meds <- strsplit(path, split = "->")[[1]]
        med1 <- meds[1]
        med2 <- meds[2]

        coefs <- sm$fit_mx[med2][[1]]$coefficients[med1, 2:5]
        df_dir[row, 1] <- paste(path, "(d)")
        df_dir[row, 2:5] <- as.numeric(coefs)
        row <- row + 1
      }
    }
  }

  if (p_values) {
    pvals <- p_value(test_model, parm = "indirect")
  }
  # changing number of columns from 4 to 5. Last column should contain p-vals
  # 4th column should be empty

  #Add indirect effects (a (d) b paths)
  df_ind <- data.frame(matrix(0, nrow = indirectrows, ncol = 4))

  if (test_model$fit$model == "serial" ){
    row <- 1
    for (reg in sm$x) {
      # The effectname in this case is "reg -> med_1 -> ... -> med_n" with
      # n for 1 up to 3. The order of mediators remains the same but can skip
      # a value.

      # indirect_effects will contain the different permutations of mediators

      indirect_effects <- list()

      if (length(sm$m) >= 1) {
        # add 1 to the list
        indirect_effects <- append(indirect_effects, list(1))
      }
      if (length(sm$m) >= 2) {
        # add 2 and possible permutations of 1 and 2
        indirect_effects <- append(indirect_effects, list(c(2)))
        indirect_effects <- append(indirect_effects, list(c(1:2)))
      }
      if (length(sm$m) == 3) {
        # add 3 and possible choices from 1,2,3 where the numbers are ordered

        indirect_effects <- append(indirect_effects, list(c(3)))
        indirect_effects <- append(indirect_effects, list(c(1,3)))
        indirect_effects <- append(indirect_effects, list(c(2:3)))
        indirect_effects <- append(indirect_effects, list(c(1:3)))
      }

      for (effect in indirect_effects) {
        effectname <- paste(reg, paste0(sm$m[effect], collapse = "->"),
                            sep = "->")

        if (length(sm$x) == 1) {
          effectname <- gsub(pattern = paste0(reg,"->"),
                             replacement = "",
                             x = effectname)
        }

        df_ind[row,1] <- paste(effectname, "(Indirect)")

        if (length(sm$m) > 1) {
          df_ind[row, 2] <- test_model$indirect[effectname][[1]]

          lower <- round(test_model$ci[effectname, 1], digits)
          upper <- round(test_model$ci[effectname, 2], digits)

        } else {
          df_ind[row,2] <- test_model$indirect[reg][[1]]


          lower <- round(test_model$ci[1], digits)
          upper <- round(test_model$ci[2], digits)
        }

        df_ind[row, 3] <- paste0("("(), lower, ",",upper,")")

        if (p_values) {
          df_ind[row, 4] <- pvals[paste("Indirect", effectname, sep = "_")][[1]]
        }

        row <- row + 1
      }
    }

  } else {
    # Parallel model
    row <- 1
    for (reg in sm$x) {
      for (med in sm$m) {
        effectname <- paste(reg, "->", med, sep = "")
        df_ind[row,1] <- paste(effectname, "(Indirect)")

        if (length(sm$m) > 1) {
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[effectname, 1], digits)
            upper <- round(test_model$ci[effectname, 2], digits)
            df_ind[row, 2] <- test_model$indirect[effectname][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", effectname,
                                          sep = "_")][[1]]

          } else {
            # Only 1 explanatory variable, multiple mediators
            lower <- round(test_model$ci[med, 1], digits)
            upper <- round(test_model$ci[med, 2], digits)
            df_ind[row, 2] <- test_model$indirect[med][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", med, sep = "_")][[1]]

          }
        } else {
          # Only 1 mediator
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[reg, 1], digits)
            upper <- round(test_model$ci[reg, 2], digits)
            df_ind[row,2] <- test_model$indirect[reg][[1]]

            if (p_values) {
              df_ind[row, 4] <- pvals[paste("Indirect", reg, sep = "_")][[1]]
            }
          } else {
            # only 1 indirect effect
            df_ind[row,2] <- test_model$indirect
            lower <- round(test_model$ci[1], digits)
            upper <- round(test_model$ci[2], digits)

            if (p_values) {
              df_ind[row, 4] <- pvals["Indirect"][[1]]
            }
          }
        }
        df_ind[row, 3] <- paste("(", lower, ", ", upper,")", sep = "")
        row <- row + 1
      }
    }
  }

  # Create the table from the dataframes and round columns
  df_rounded <- data.frame(lapply(df_dir, function(y) {
    if(is.numeric(y)) round(y, digits) else y}))
  df_ind_rounded <- data.frame(lapply(df_ind, function(y) {
    if(is.numeric(y)) round(y, digits) else y}))

  colnames(df_ind_rounded) <- c("Indirect Effects", "Estimate",
                                "Confidence Interval", "p-value")

  colnames(df_rounded) <- c("Direct Effects", "Estimate", "Std. Error",
                            "z statistic", "p-value")

  dim_ind <- dim(df_ind_rounded)
  dim_dir <- dim(df_rounded)

  # Copying data from matrix with 4 columns to one with 5 columns to get the
  # same dimensions for direct and indirect effects
  df_ind_merge <- as.data.frame(matrix(NA, nrow = dim_ind[1], ncol = dim_ind[2]))
  df_ind_merge[,1:3] <- df_ind_rounded[,1:3]
  df_ind_merge[,5] <- df_ind_rounded[,4]

  df_ind_merge <- rbind(c("Indirect Effects", "Estimate",
                          "Confidence Interval","", "p-value"), df_ind_merge)

  df_dir_merge <- df_rounded

  # Set colnames equal so that rbind can be applied
  colnames(df_dir_merge) <- colnames(df_ind_merge)
  df_dir_merge <- data.frame(apply(df_dir_merge, FUN = as.character,
                                   MARGIN = 2))

  df_stacked <- rbind(df_dir_merge, df_ind_merge)
  colnames(df_stacked) <- c("Direct Effects", "Estimate", "Std. Error",
                            "z statistic", "p-value")
  return(list(df_stacked, c(a_paths, b_paths, c_paths)))
}


get_method_robmed <- function(test_model) {
  # TODO implement this function to return the proper method type for different
  # methods included in robmed based on robust, method, and family attributes
  if (test_model$fit$robust %in% c("MM", TRUE)) {
    return("robmed")
  } else {
    return("OLS Bootstrap")
  }
}

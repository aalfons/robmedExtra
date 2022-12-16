#' Export result table to Word
#'
#' Export the table containing the results of mediation analysis to a Microsoft Word
#' document.
#'
#' @param test_model an object inheriting from class
#' \code{"\link{test_mediation}"} or a list of objects of that class.
#' of object
#'
#' @param digits a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#'
#' @param orientation the  orientation in which the table is displayed in the
#' document. When set to portrait, the tables are displayed
#' underneath each other. When set to landscape, the tables are displayed side
#' by side.
#'
#'
#' @return An object of class \code{"\link{rdocx}"}, containing a table of the
#' results of the provided objects.
#'
#' @imporFrom officer read_docx body_end_section_landscape body_add_break
#'
#' @importFrom flextable flextable width add_header_row body_add_flextable align
#' set_flextable_defaults hline padding add_footer_lines
#'
#'
#'
#' @author Vincent Drenth
#'
#'
#' @examples
#' data("BSG2014")
#'
#' boot_robust <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014)
#'
#' table <- export_table_MSWord(boot_robust)
#' print(table, target = "table_robust.docx")
#'
#'
#' boot_ols <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014,
#'                               robust = FALSE)
#'
#' tables <- export_table_MSWord(list(boot_robust, boot_ols))
#'
#' print(tables, target = "tables_landscape.docx")
#'
#'
#' @export

export_table_MSWord <- function(test_model, ...) {
  UseMethod("export_table_MSWord")
}

#' @rdname export_table_MSWord
#' @method export_table_MSWord list
#' @export
export_table_MSWord.list <- function(test_model,
                                     orientation = c("landscape", "portrait"),
                                     ...) {
  if (length(test_model) == 1) {
    return(export_table_MSWord(test_model[[1]]))
  }

  model_robust = Filter(function(x) x$fit$robust != FALSE, test_model)[[1]]
  model_ols = Filter(function(x) (x$fit$robust == FALSE), test_model)[[1]]

  tables_robust <- create_tables(model_robust)
  tables_ols <- create_tables(model_ols)

  doc <- officer::read_docx()

  if (orientation == "landscape") {
    doc <- officer::body_end_section_landscape(doc)
    for (index in seq(1, ceiling((length(test_model) + 1)/2), 2)) {
      tables_left <- create_tables(test_model[[index]])
      direct_data_left <- tables_left$direct$body$dataset
      indirect_data_left <- tables_left$indirect$body$dataset

      tables_right <- create_tables(test_model[[index + 1]])
      direct_data_right <- tables_right$direct$body$dataset
      indirect_data_right <- tables_right$indirect$body$dataset

      colnames(direct_data_right) <- unlist(lapply(colnames(direct_data_right),
                                                   FUN = function(x) paste0(x, "\r")))
      colnames(indirect_data_right) <- unlist(lapply(colnames(indirect_data_right),
                                                     FUN = function(x) paste0(x, "\r")))

      direct_data_right <- direct_data_right[,2:ncol(direct_data_right)]
      indirect_data_right <- indirect_data_right[,2:ncol(indirect_data_right)]

      direct_data <- data.frame(direct_data_left, direct_data_right)
      indirect_data <- data.frame(indirect_data_left, indirect_data_right)

      ft_direct <- flextable::flextable(direct_data)
      ft_direct <- flextable::width(ft_direct, j = 1, width = 2, unit = "in")
      ft_direct <- flextable::add_header_row(ft_direct,
                                             values = c(" ", "METHOD", "METHOD"),
                                             top = TRUE, colwidths = c(1, 4, 4))
      ft_direct <- align(ft_direct, align = "center", part = "all")

      ft_indirect <- flextable::flextable(indirect_data)
      ft_indirect <- flextable::width(ft_indirect, j = 1, width = 2,
                                      unit = "in")
      ft_indirect <- flextable::width(ft_indirect, j = 3, width = 1.5,
                                      unit = "in")
      ft_indirect <- flextable::width(ft_indirect, j = 6, width = 1.5,
                                      unit = "in")
      ft_indirect <- flextable::align(ft_indirect, align = "center",
                                      part = "all")

      doc <- flextable::body_add_flextable(doc, ft_direct)
      doc <- flextable::body_add_flextable(doc, ft_indirect)
      doc <- officer::body_add_break(doc, "after")
    }

    if (length(test_model) %% 2 == 1) {
      tables <- create_tables(test_model[[length(test_model)]])
      doc <- flextable::body_add_flextable(doc, tables$direct)
      doc <- flextable::body_add_flextable(doc, tables$indirect)
    }

    doc <- officer::body_end_section_landscape(doc)
  } else if (orientation == "portrait") {
    count_page <- 0
    for (test_object in test_model) {
      tables <- create_tables(test_object)
      doc <- flextable::body_add_flextable(doc, tables$direct)
      doc <- flextable::body_add_flextable(doc, tables$indirect)
      count_page <- count_page + 1
      if (count_page %% 2 == 0) {
        doc <- officer::body_add_break(doc, "after")
      }
    }
  }
  return(doc)
}

#' @rdname export_table_MSWord
#' @method export_table_MSWord test_mediation
#' @export
#'

export_table_MSWord.test_mediation <- function(test_model, digits = 4, ...) {
  tables <- create_tables(test_model = test_model, digits = digits)
  ft_direct <- tables$direct
  ft_indirect <- tables$indirect

  doc <- read_docx()
  doc <- body_add_flextable(doc, ft_direct)
  doc <- body_add_flextable(doc, ft_indirect)
  return(doc)
}

# Creates a list containing both the flextable with direct and indirect effects.
create_tables <- function(test_model, digits = 4) {

  sm <- summary(test_model)$summary

  if (test_model$fit$model == "serial") {
    if (length(sm$m) == 1) {
      directrows <- 2 * (length(sm$x)) + 1
      indirectrows <- length(sm$x)

    } else if (length(sm$m) == 2) {
      #Generate table for 2 serial mediators
      directrows <- 3 * (1 + length(sm$x))
      indirectrows <- 3 * length(sm$x)
    }
  } else {
    #Model is parallel
    rows = 2 * (length(sm$x) * length(sm$m)) + 2 * length(sm$x) + length(sm$m)
    indirectrows = length(sm$x) * length(sm$m)
    directrows = rows - indirectrows
  }

  df_dir <- as.data.frame(matrix(NA, nrow = directrows, ncol = 5))
  colnames(df_dir) <- c("Direct Effects", 'Estimate', 'Std. Error', 'z statistic', 'p-value')

  row <- 1
  for (med in sm$m) {
    if (length(sm$m) > 1){
      coefs_a <- sm$fit_mx[med][[1]]$coefficients
    } else {
      coefs_a <- sm$fit_mx$coefficients
    }

    #Add a paths
    for (reg in sm$x) {
      df_dir[row, 1] <- paste(reg, med, sep = "->")
      df_dir[row, 2:5] <- coefs_a[reg, 2:5]
      row <- row + 1
    }
  }
  a_paths <- row

  coefs_b <- sm$fit_ymx$coefficients
  for (med in sm$m) {
    #Add b paths
    df_dir[row, 1] <- paste('(X),',med ,'->' , sm$y)
    df_dir[row, 2:5] <- coefs_b[med, 2:5]
    row <- row + 1
  }
  b_paths <- row

  # Add c path (Direct effect)
  for (reg in sm$x){
    df_dir[row, 1] <- paste(reg,'->', sm$y, '(direct)')
    df_dir[row, 2:5] <- sm$direct[reg, 2:5]
    row <- row + 1
  }
  c_paths <- row

  # Add c' path (Total effect)
  for (reg in sm$x) {
    df_dir[row, 1] <- paste(reg, '->', sm$y, '(total)')
    df_dir[row, 2:5] <- sm$total[reg, 2:5]
    row <- row + 1
  }

  pvals <- p_value(test_model, parm = 'indirect')

  #Add indirect effects (a (d) b paths)
  df_ind <- data.frame(matrix(0, nrow = indirectrows, ncol = 4))
  colnames(df_ind) <- c("Indirect Effects", 'Estimate', 'Confidence Interval', 'p-value')
  if (test_model$fit$model == "serial" ){
    row <- 1
    for (reg in sm$x) {
      # Through only first or only second mediator
      for (med in sm$m) {
        effectname <- paste(reg, '->', med, sep ='')
        df_ind[row,1] <- paste(effectname, '(Indirect)')

        if (length(sm$m) > 1) {
          df_ind[row, 2] <- test_model$indirect[effectname][[1]]
          lower <- round(test_model$ci[effectname, 1], digits)
          upper <- round(test_model$ci[effectname, 2], digits)
        } else {
          df_ind[row,2] <- test_model$indirect[reg][[1]]
          lower <- round(test_model$ci[1], digits)
          upper <- round(test_model$ci[2], digits)
        }

        df_ind[row, 3] <- paste('(', lower, ',',upper,')', sep = '')

        row <- row + 1
      }

      # Path through both mediators
      effectname <- paste(reg, '->', sm$m[1], '->', sm$m[2], sep = '')

      df_ind[row,1] <- paste(effectname, '(Indirect)', sep = '')
      df_ind[row, 2] <- test_model$indirect[effectname][[1]]

      lower <- round(test_model$ci[effectname, 1], digits)
      upper <- round(test_model$ci[effectname, 2], digits)

      df_ind[row, 3] <- paste('(', lower, ',', upper,')', sep = '')
      row <- row + 1
    }
  } else {
    # Parallel model
    row <- 1
    for (reg in sm$x) {
      for (med in sm$m) {
        effectname <- paste(reg, '->', med, sep ='')
        df_ind[row,1] <- paste(effectname, '(Indirect)')

        if (length(sm$m) > 1) {
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[effectname, 1], digits)
            upper <- round(test_model$ci[effectname, 2], digits)
            df_ind[row, 2] <- test_model$indirect[effectname][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", effectname, sep = '_')][[1]]

          } else {
            # Only 1 explanatory variable, multiple mediators
            lower <- round(test_model$ci[med, 1], digits)
            upper <- round(test_model$ci[med, 2], digits)
            df_ind[row, 2] <- test_model$indirect[med][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", med, sep = '_')][[1]]

          }
        } else {
          # Only 1 mediator
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[reg, 1], digits)
            upper <- round(test_model$ci[reg, 2], digits)
            df_ind[row,2] <- test_model$indirect[reg][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", reg, sep = '_')][[1]]

          } else {
            # only 1 indirect effect
            df_ind[row,2] <- test_model$indirect
            lower <- round(test_model$ci[1], digits)
            upper <- round(test_model$ci[2], digits)
            df_ind[row, 4] <- pvals["Indirect"][[1]]

          }
        }
        df_ind[row, 3] <- paste('(', lower, ',', upper,')', sep = '')
        row <- row + 1
      }
    }
  }

  # Create the table from the dataframes
  df_rounded <- data.frame(lapply(df_dir, function(y) if(is.numeric(y)) round(y, digits) else y))
  df_ind_rounded <- data.frame(lapply(df_ind, function(y) if(is.numeric(y)) round(y, digits) else y))

  set_flextable_defaults(
    font.size = 10,
    padding = 2,
    background.color = 'white')

  ft_direct <- flextable(df_rounded)
  ft_direct <- width(ft_direct, j = 1, width = 2.5, unit = "in")
  ft_direct <- width(ft_direct, j = 2:5, width = 1, unit = "in")
  ft_direct <- add_header_row(ft_direct, top = TRUE, values = c("METHOD"), colwidths = c(5))
  ft_direct <- align(ft_direct, i = 1:directrows, j = 2:5, align = "center", part = "body")
  ft_direct <- align(ft_direct, align = "center", part = "header")

  # Add spacing and a line between different kinds of paths
  ft_direct <- padding(ft_direct, i = a_paths, padding.bottom =  5, part = "body")
  ft_direct <- padding(ft_direct, i = b_paths, padding.bottom =  5, part = "body")
  ft_direct <- padding(ft_direct, i = c_paths, padding.bottom =  5, part = "body")
  ft_direct <- padding(ft_direct, i = directrows, padding.bottom =  10, part = "body")

  ft_direct <- hline(ft_direct, i = a_paths - 1, border = fp_border("gray"), part = "body")
  ft_direct <- hline(ft_direct, i = b_paths - 1, border = fp_border("gray"), part = "body")
  ft_direct <- hline(ft_direct, i = c_paths - 1, border = fp_border("gray"), part = "body")

  ft_indirect <- flextable(df_ind_rounded)
  ft_indirect <- width(ft_indirect, j = 1, width = 2.5, unit = "in")
  ft_indirect <- width(ft_indirect, j = 3, width = 2, unit = "in")
  ft_indirect <- width(ft_indirect, j = c(2,4), width = 1, unit = "in")
  ft_indirect <- align(ft_indirect, i = 1:indirectrows, j = 2:4, align = "center", part = "body")
  ft_indirect <- align(ft_indirect, j = 2:4, align = "center", part = "header")
  ft_indirect

  ft_indirect <- add_footer_lines(ft_indirect, paste('Sample size = ', nrow(test_model$fit$data),
                                                     '. Number of bootstrap samples = ', test_model$R , '.\n',
                                                     '†p < .1. *p < .05. **p < .01. ***p < .001.'))
  result = list()
  result$direct <- ft_direct
  result$indirect <- ft_indirect

  return(result)
}


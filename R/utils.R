# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# Convert summary of results from mediation analysis to list of tables -----

## convert summary of results from mediation analysis to list of tables for
## total, direct, indirect effects, and additional information: this does the
## heavy lifting for to_flextable() and to_latex()

# generic function
get_mediation_tables <- function(object, ...) UseMethod("get_mediation_tables")

# method for summary of results from mediation analysis
get_mediation_tables.summary_test_mediation <- function(object,
                                                        p_value = FALSE,
                                                        digits = 3L,
                                                        big.mark = NULL,
                                                        decimal.mark = getOption("OutDec"),
                                                        ...) {
  # initializations
  summary <- object$summary
  object <- object$object
  # obtain formatted tables for total, direct, and indirect effects
  df_total <- get_total_table(summary, digits = digits, big.mark = "",
                              decimal.mark = decimal.mark, ...)
  df_direct <- get_direct_table(summary, digits = digits, big.mark = "",
                                decimal.mark = decimal.mark, ...)
  df_indirect <- get_indirect_table(object, p_value = p_value, digits = digits,
                                    big.mark = "", decimal.mark = decimal.mark,
                                    ...)
  # construct list of tables
  tables <- list(total = df_total, direct = df_direct, indirect = df_indirect)
  # add information on variables
  tables <- c(tables, summary[c("x", "m", "y", "covariates")])
  # add information on sample size and number of bootstrap samples
  if (is.null(big.mark)) big.mark <- if (decimal.mark == ".") "," else ""
  tables$n <- formatC(summary$n, format = "d", big.mark = big.mark)
  # if applicable, add information on number of bootstrap samples
  R <- object$R
  if (!is.null(R)) tables$R <- formatC(R, format = "d", big.mark = big.mark)
  # return list of tables
  tables
}

# list of results from mediation analysis of summaries thereof
get_mediation_tables.list <- function(object, type = c("boot", "data"),
                                      p_value = FALSE, digits = 3L,
                                      big.mark = NULL,
                                      decimal.mark = getOption("OutDec"),
                                      ...) {
  # initializations
  is_mediation <- sapply(object, inherits, "test_mediation")
  is_summary <- sapply(object, inherits, "summary_test_mediation")
  object <- object[is_mediation | is_summary]
  if (length(object) == 0L) {
    stop('no objects inheriting from class "test_mediation" or ',
         '"summary_test_mediation"')
  }
  # make sure we have summary objects
  object[is_mediation] <- lapply(object[is_mediation], summary,
                                 type = type, plot = FALSE)
  # extract mediation objects and summaries
  object_list <- lapply(object, "[[", "object")
  summary_list <- lapply(object, "[[", "summary")
  # check that variables are the same
  components <- c("x", "y", "m", "covariates")
  variables <- summary_list[[1L]][components]
  variable_list <- lapply(summary_list[-1L], "[", components)
  all_identical <- all(sapply(variable_list, identical, variables))
  if (!all_identical) {
    stop("all mediation objects must use the same variables")
  }
  # check that number of observations is the same for all objects
  n <- unique(sapply(summary_list, "[[", "n"))
  if (length(n) > 1L) {
    stop("number of observations must be the same for all mediation objects")
  }
  # check that mediation model is the same for all objects
  model <- unique(do.call(c, lapply(summary_list, "[[", "model")))
  if (length(model) > 1L) {
    stop("mediation model must be the same for all objects")
  }
  # check that number of bootstrap samples is the same for all objects
  R <- unique(do.call(c, lapply(object_list, "[[", "R")))
  if (length(R) > 1L) {
    stop("number of bootstrap samples must be the same for all ",
         "mediation objects")
  }
  # check that confidence level is the same for all objects
  level <- unique(do.call(c, lapply(object_list, "[[", "level")))
  if (length(level) > 1L) {
    stop("confidence level must be the same for all mediation objects")
  }
  # check names of list elements
  methods <- names(object)
  if (is.null(methods)) {
    methods <- sapply(object_list, get_method_name)
    names(object_list) <- names(summary_list) <- methods
  } else {
    is_empty <- methods == ""
    if (any(is_empty)) {
      methods[is_empty] <- sapply(object_list[is_empty], get_method_name)
      names(object_list) <- names(summary_list) <- methods
    }
  }
  # obtain list of formatted tables for total, direct, and indirect effects
  df_total_list <- lapply(summary_list, get_total_table, digits = digits,
                          big.mark = "", decimal.mark = decimal.mark, ...)
  df_direct_list <- lapply(summary_list, get_direct_table, digits = digits,
                           big.mark = "", decimal.mark = decimal.mark, ...)
  df_indirect_list <- lapply(object_list,get_indirect_table,
                             p_value = p_value, digits = digits,
                             big.mark = "", decimal.mark = decimal.mark,
                             ...)
  # construct return object
  tables <- list(methods = methods, total = df_total_list,
                 direct = df_direct_list, indirect = df_indirect_list)
  # add information on variables
  tables <- c(tables, variables)
  # add information on sample size and number of bootstrap samples
  if (is.null(big.mark)) big.mark <- if (decimal.mark == ".") "," else ""
  tables$n <- formatC(n, format = "d", big.mark = big.mark)
  if (!is.null(R)) tables$R <- formatC(R, format = "d", big.mark = big.mark)
  # return list of tables
  tables
}


# obtain table of total effects
get_total_table <- function(summary, digits = 3L, format = "f",
                            flag = " ", big.mark = "", ...) {
  # extract matrix containing effect summaries
  total <- extract_total(summary)
  # convert matrix to formatted data frame
  format_effect_table(total, label = "Total Effect", digits = digits,
                      format = format, flag = flag, big.mark = big.mark,
                      ...)
}

# obtain table of direct effects
get_direct_table <- function(summary, digits = 3L, format = "f",
                             flag = " ", big.mark = "", ...) {
  # extract matrix containing effect summaries
  direct <- rbind(extract_a(summary),
                  extract_d(summary),
                  extract_b(summary),
                  extract_direct(summary))
  # convert matrix to formatted data frame
  format_effect_table(direct, label = "Direct Effect", digits = digits,
                      format = format, flag = flag, big.mark = big.mark,
                      ...)
}

## obtain table of indirect effects, possibly including p values
#' @importFrom robmed p_value
get_indirect_table <- function(object, p_value = FALSE, digits = 3L,
                               format = "f", flag = " ", big.mark = "",
                               ...) {
  # initializations
  have_boot <- inherits(object, "boot_test_mediation")
  level <- if (have_boot) object$level
  # extract matrix containing effect summaries
  indirect <- extract_indirect(object)
  # if requested, compute p values of bootstrap tests for indirect effects
  if (have_boot && p_value) {
    p_value <- robmed::p_value(object, parm = "indirect", digits = digits)
  } else p_value <- NULL
  # convert matrix to formatted data frame
  format_indirect_table(indirect, level = level, p_value = p_value,
                        digits = digits, format = format, flag = flag,
                        big.mark = big.mark, ...)
}

# construct default name for method
get_method_name <- function(object) {
  # get label for estimation method
  fit <- object$fit
  robust <- fit$robust
  if (inherits(fit, "reg_fit_mediation")) {
    if (is.character(robust)) {
      if (robust == "MM") method <- "Robust"
      else if (robust == "median") method <- "Median"
      else stop("unknown robust estimation method")
    } else if (fit$family == "gaussian") method <- "OLS"
    else method <- "SNT"
  } else if (inherits(fit, "cov_fit_mediation")) {
   method <- if (robust) "Winsorized" else "Covariance"
  } else stop("unknown estimation method")
  # get label for type of test
  if (inherits(object, "boot_test_mediation")) test <- "Bootstrap"
  else if (inherits(object, "sobel_test_mediation")) test <- "Sobel"
  else stop("unknown test for indirect effect")
  # put labels together
  if (method == "Robust" && test == "Bootstrap") "ROBMED"
  else paste(method, test)
}


# Extract effects from results and summaries of mediation analysis -----
# (code to construct labels is rather ugly)

# extract effect summaries for a path
# summary ... 'summary' component of object of class "summary_test_mediation"
extract_a <- function(summary) {
  # initializations
  x <- summary$x
  m <- summary$m
  p_x <- length(x)
  p_m <- length(m)
  # extract effect summaries for each independent variable
  if (inherits(summary, "summary_reg_fit_mediation")) {
    # mediation model fitted via regressions
    if (p_x == 1L) {
      # only one independent variable
      a <- .extract_a(x, summary$fit_mx)
    } else {
      # multiple independent variables
      a_list <- lapply(x, .extract_a, summary$fit_mx)
      a <- do.call(rbind, a_list)
    }
  } else {
    # if the mediation model is fitted via the covariance matrix, we already
    # have the relevant information in a component of the summary object
    a <- summary$a
  }
  # for bootstrapped effect summaries, keep only bootstrap point estimate
  a <- keep_estimate(a)
  # construct labels
  seq_x <- seq_len(p_x)
  seq_m <- seq_len(p_m)
  if (p_x == 1L) {
    if (p_m == 1L) {
      label_x <- "X"
      label_m <- "M"
      label_a <- "a"
    } else {
      label_x <- "X"
      label_m <- paste0("M", seq_m)
      label_a <- paste0("a", seq_m)
    }
  } else {
    if (p_m == 1L) {
      label_x <- paste0("X", seq_x)
      label_m <- "M"
      label_a <- paste0("a", seq_x)
    } else {
      label_x <- rep(paste0("X", seq_x), each = p_m)
      label_m <- rep(paste0("M", seq_m), times = p_x)
      label_a <- paste0("a", rep(seq_m, times = p_x), rep(seq_x, each = p_m))
    }
  }
  # add labels
  rownames(a) <- paste0(label_x, "->", label_m, " (", label_a, ")")
  # return effect(s)
  a
}

## extract effect summaries for a path for one independent variable
#' @importFrom stats coef
.extract_a <- function(x, fit) {
  if (inherits(fit, "list")) {
    # multiple mediators
    coef_list <- lapply(fit, function(current) coef(current)[x, , drop = FALSE])
    do.call(rbind, coef_list)
  } else {
    # only one mediator
    coef(fit)[x, , drop = FALSE]
  }
}

## extract effect summaries for d path (if existing) from summary object
## summary ... 'summary' component of object of class "summary_test_mediation"
#' @importFrom stats coef
extract_d <- function(summary) {
  # initializations
  model <- summary$model
  have_serial <- !is.null(model) && model == "serial"
  # currently only implemented for two or three hypothesized mediators
  if (have_serial) {
    # further initializations
    m <- summary$m
    p_m <- length(m)
    fit_list <- summary$fit_mx
    # only implemented for two or three serial mediators
    if (p_m == 2L) {
      # two serial mediators
      d <- coef(fit_list[[m[2L]]])[m[1L], , drop = FALSE]
      rownames(d) <- "M1->M2 (d21)"
    } else if (p_m == 3L) {
      # three serial mediators
      d <- rbind(coef(fit_list[[m[2L]]])[m[1L], , drop = FALSE],
                 coef(fit_list[[m[3L]]])[m[1L:2L], ])
      rownames(d) <- c("M1->M2 (d21)", "M1->M3 (d31)", "M2->M3 (d32)")
    }
  } else d <- NULL
  # for bootstrapped effect summaries, keep only bootstrap point estimate
  keep_estimate(d)
}

## extract effect summaries for b path from summary object
## summary ... 'summary' component of object of class "summary_test_mediation"
#' @importFrom stats coef
extract_b <- function(summary) {
  # initializations
  m <- summary$m
  p_m <- length(m)
  # extract effect summaries
  if (inherits(summary, "summary_reg_fit_mediation")) {
    b <- coef(summary$fit_ymx)[m, , drop = FALSE]
  } else b <- summary$b
  # for bootstrapped effect summaries, keep only bootstrap point estimate
  b <- keep_estimate(b)
  # add labels
  if (p_m == 1L) {
    label_m <- "M"
    label_b <- "b"
  } else {
    seq_m <- seq_len(p_m)
    label_m <- paste0("M", seq_m)
    label_b <- paste0("b", seq_m)
  }
  rownames(b) <- paste0(label_m, "->Y (", label_b, ")")
  # return effect summaries
  b
}

# extract summaries of direct effects of X on Y: this is easier since they are
# stored in a specific component of the summary object
# summary ... 'summary' component of object of class "summary_test_mediation"
extract_direct <- function(summary) {
  # extract effect summaries: keep only bootstrap point estimate where relevant
  direct <- keep_estimate(summary$direct)
  p_x <- nrow(direct)
  # add labels
  if (p_x == 1L) {
    label_x <- "X"
    label_direct <- "c"
  } else {
    seq_x <- seq_len(p_x)
    label_x <- paste0("X", seq_x)
    label_direct <- paste0("c", seq_x)
  }
  rownames(direct) <- paste0(label_x, "->Y (", label_direct, ")")
  # return effect summaries
  direct
}

# extract summaries of total effects of X on Y: this is easier since they are
# stored in a specific component of the summary object
# summary ... 'summary' component of object of class "summary_test_mediation"
extract_total <- function(summary) {
  # extract effect summaries: keep only bootstrap point estimate where relevant
  total <- keep_estimate(summary$total)
  p_x <- nrow(total)
  # add labels
  if (p_x == 1L) {
    label_x <- "X"
    label_total <- "c'"
  } else {
    seq_x <- seq_len(p_x)
    label_x <- paste0("X", seq_x)
    label_total <- paste0("c'", seq_x)
  }
  rownames(total) <- paste0(label_x, "->Y (", label_total, ")")
  # return effect summaries
  total
}

# extract summaries of indirect effects of X on Y: information needs to be
# collected from different components of an object of class "test_mediation",
# in a similar way as in the corresponding print() method
extract_indirect <- function(object) {

  # initializations
  p_x <- length(object$fit$x)
  p_m <- length(object$fit$m)
  model <- object$fit$model
  have_simple <- is.null(model) || model == "simple"
  contrast <- object$fit$contrast          # only implemented for regression fit
  have_contrast <- is.character(contrast)  # but this always works
  # extract effect summaries
  if (inherits(object, "boot_test_mediation")) {
    indirect <- cbind(Estimate = object$indirect,
                      if (have_simple) t(object$ci) else object$ci)
  } else {
    indirect <- cbind(object$fit$indirect, object$se,
                      object$statistic, object$p_value)
    cn <- switch(object$alternative, twosided = "Pr(>|z|)",
                 less = "Pr(<z)", greater = "Pr(>z)")
    colnames(indirect) <- c("Estimate", "Std. Error", "z value", cn)
  }

  # construct and add labels
  if (p_x > 1L && p_m > 1L) {

    # multiple independent variables and multiple mediators: labels have to be
    # constructed in a different manner due to possible contrasts

    # initializations
    seq_x <- seq_len(p_x)
    # prepare labels for individual indirect paths
    if (model == "serial") {
      # serial mediators
      if (p_m == 2L) {
        label_x <- sapply(seq_x, function(j) rep(paste0("X", j), each = 4L))
        label_m <- c("...", "M1", "M2", "M1->M2")
        label_indirect <- sapply(
          seq_x, function(j, label) sprintf(label, j),
          label = c("total", "a1%db1", "a2%db2", "a1%dd21b2")
        )
      } else {
        label_x <- sapply(seq_x, function(j) rep(paste0("X", j), each = 8L))
        label_m <- c("...", "M1", "M2", "M3", "M1->M2",
                     "M1->M3", "M2->M3", "M1->M2->M3")
        label_indirect <- sapply(
          seq_x, function(j, label) sprintf(label, j),
          label = c("total", "a1%db1", "a2%db2", "a3%db3", "a1%dd21b2",
                    "a1%dd31b3", "a2%dd32b3", "a1%dd21d32b3")
        )
      }
    } else {
      # parallel mediators
      seq_m <- seq_len(p_m)
      label_x <- sapply(seq_x, function(j) rep(paste0("X", j), each = p_m+1L))
      # label_m <- replicate(p_x, c("...", paste0("M", seq_m)))
      label_m <- c("...", paste0("M", seq_m))
      label_indirect <- sapply(
        seq_x, function(j, label) sprintf(label, j),
        label = c("total", paste0("a", seq_m, "%db", seq_m))
      )
    }
    # construct labels for indirect paths
    label_path <- sapply(seq_x, function(j) {
      paste0(label_x[, j], "->", label_m, "->Y (", label_indirect[, j], ")")
    })
    # if applicable, construct labels of contrasts
    if (have_contrast) {
      label_contrast <- apply(label_indirect[-1L, , drop = FALSE], 2L,
                               get_contrast_labels, type = contrast)
    } else label_contrast <- NULL
    # add labels to indirect effects
    rownames(indirect) <- c(rbind(label_path, label_contrast))

  } else {

    # first indirect effects are reported, followed by contrasts
    if (have_simple) {
      # simple mediation model
      label_x <- "X"
      label_m <- "M"
      label_indirect <- "ab"
      label_contrast <- NULL
    } else if (model == "multiple") {
      # multiple independent variables but only one mediator
      seq_x <- seq_len(p_x)
      label_x <- paste0("X", seq_x)
      label_m <- "M"
      label_indirect <- paste0("ab", seq_x)
      # if applicable, construct labels of contrasts
      if (have_contrast) {
        label_contrast <- get_contrast_labels(label_indirect, type = contrast)
      } else label_contrast <- NULL
    } else {
      # multiple mediators but only one independent variable
      if (model == "serial") {
        # serial mediators
        if (p_m == 2L) {
          label_x <- rep("X", times = 4L)
          label_m <- c("...", "M1", "M2", "M1->M2")
          label_indirect <- c("total", "a1b1", "a2b2", "a1d21b2")
        } else {
          label_x <- rep("X", times = 8L)
          label_m <- c("...", "M1", "M2", "M3", "M1->M2",
                       "M1->M3", "M2->M3", "M1->M2->M3")
          label_indirect <- c("total", "a1b1", "a2b2", "a3b3", "a1d21b2",
                              "a1d31b3", "a2d32b3", "a1d21d32b3")
        }
      } else {
        # parallel mediators
        seq_m <- seq_len(p_m)
        label_x <- rep("X", times = p_m + 1L)
        label_m <- c("...", paste0("M", seq_m))
        label_indirect <- c("total", paste0("a", seq_m, "b", seq_m))
      }
      # if applicable, construct labels of contrasts
      if (have_contrast) {
        label_contrast <- get_contrast_labels(label_indirect[-1L],
                                              type = contrast)
      } else label_contrast <- NULL
    }
    # add labels to indirect effects
    label_path <- paste0(label_x, "->", label_m, "->Y (", label_indirect, ")")
    rownames(indirect) <- c(label_path, label_contrast)

  }

  # return effect summaries
  indirect
}

# keep only the relevant point estimate from matrix of effect summaries
keep_estimate <- function(coef_mat) {
  # initializations
  cn <- colnames(coef_mat)
  have_boot <- all(c("Data", "Boot") %in% cn)
  # if we have bootstrapped effect summaries, keep only the bootstrap estimate
  if (have_boot) {
    keep <- cn != "Data"
    coef_mat <- coef_mat[, keep, drop = FALSE]
    rename <- colnames(coef_mat) == "Boot"
    colnames(coef_mat)[rename] <- "Estimate"
  }
  # return effect summaries
  coef_mat
}

## obtain labels on how contrasts of indirect effects are computed
#' @importFrom utils combn
get_contrast_labels <- function(labels, type = "estimates") {
  # compute combinations of names
  combinations <- combn(labels, 2, simplify = FALSE)
  n_contrasts <- length(combinations)
  # obtain information on contrasts
  if (type == "estimates") {
    fun <- function(labels) paste(labels, collapse = "-")
  } else if (type == "absolute") {
    fun <- function(labels) {
      paste(paste0("|", labels, "|"), collapse = "-")
    }
  } else stop("type of contrasts not implemented")
  # return contrast labels
  sapply(combinations, fun)
}


# Format table of effect summaries -----

# convert matrix of total or direct effect summaries to formatted data frame
format_effect_table <- function(object, label = "Effect", ...) {
  # format numbers and replace missing values
  object <- formatC(object, ...)
  object <- gsub("NA", "  ", object, fixed = TRUE)
  # convert to data frame and fix names
  df <- data.frame(rownames(object), object, check.names = FALSE,
                   fix.empty.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- get_table_names(label, object)
  row.names(df) <- NULL
  # return data frame
  df
}

# convert matrix of indirect effect summaries to formatted data frame
format_indirect_table <- function(object, level = 0.95, p_value = NULL, ...) {
  # initializations
  have_boot <- !is.null(level)
  have_p_value <- !is.null(p_value)
  label <- "Indirect Effect"
  # format numbers
  object <- formatC(object, ...)
  # construct data frame and fix column names
  if (have_boot) {
    # format confidence intervals and construct column name
    ci <- paste0("(", object[, "Lower"], ", ", object[, "Upper"], ")")
    ci_label <- paste0(format(100 * level, trim = TRUE),
                       "% Confidence Interval")
    # construct data frame and fix column names
    df <- data.frame(rownames(object), object[, "Estimate"], ci,
                     check.names = FALSE, fix.empty.names = FALSE,
                     stringsAsFactors = FALSE)
    names(df) <- c(label, "Estimate", ci_label)
    # add p values if supplied
    if (have_p_value) {
      p_value <- formatC(p_value, ...)
      df <- cbind(df, "p Value" = p_value)
    }
  } else {
    # convert to data frame and fix column names
    df <- data.frame(rownames(object), object, check.names = FALSE,
                     fix.empty.names = FALSE, stringsAsFactors = FALSE)
    names(df) <- get_table_names(label, object)
  }
  # fix row names and return data frame
  row.names(df) <- NULL
  df
}

# convert column names from R style to nicer names and add label for effects
get_table_names <- function(label, object) {
  # extract column names
  cn <- colnames(object)
  # convert R-style names to nicer ones
  cn <- gsub("value", "Statistic", cn, fixed = TRUE)
  cn <- gsub("Pr\\(.*\\)", "p Value", cn, fixed = FALSE)
  # return column names
  c(label, cn)
}


## Construct table note -----

#' @importFrom flextable as_i as_sub get_flextable_defaults
get_table_note <- function(x, m, y, covariates, n, R = NULL,
                           type = "flextable") {
  # initializations
  text_note <- "Note"
  if (type == "flextable") {
    big_mark <- flextable::get_flextable_defaults()$big.mark
  } else big_mark <- ","
  # construct note for sample size
  sample_info <- paste0(" Sample size = ", n, ".")
  # if applicable, construct note for number of bootstrap samples
  if (is.null(R)) boot_info <- ""
  else boot_info <- paste0(" Number of bootstrap samples = ", R, ".")
  # initializations for note on variables
  p_x <- length(x)
  p_m <- length(m)
  # define various text chunks for information on variables
  sep <- if (p_x == 1L && p_m == 1L) ", " else "; "
  text_x <- sprintf("Independent variable%s: ", if (p_x > 1L) "s" else "")
  label_x <- "X"
  seq_x <- if (p_x > 1L) seq_len(p_x)
  text_m <- sprintf("%shypothesized mediator%s: ", sep,
                    if (p_m > 1L) "s" else "")
  label_m <- "M"
  seq_m <- if (p_m > 1L) seq_len(p_m)
  text_y <- sprintf("%sdependent variable: ", sep)
  label_y <- "Y"
  # construct text string with information on control variables
  if (length(covariates) == 0L) covariate_info <- ""
  else {
    covariate_info <- paste0(sep, "control variables: ",
                             paste(covariates, collapse = ", "))
  }
  # construct text chunks for note on variables
  if (type == "flextable") {
    # construct first chunk for note
    first_chunk <- flextable::as_i(text_note)
    prefix_x <- paste(".", text_x)
    # independent variables
    if (p_x > 1L) {
      tmp <- mapply(function(current_x, j) {
        list(paste0(if (j == 1L) prefix_x else "), ", current_x, " (", label_x),
             flextable::as_sub(j))
      }, current_x = x, j = seq_len(p_x), SIMPLIFY = FALSE, USE.NAMES = FALSE)
      independent_chunks <- do.call(c, tmp)
      prefix_m <- paste0(")", text_m)
    } else {
      independent_chunks <- NULL
      prefix_m <- paste0(prefix_x, x, " (", label_x, ")", text_m)
    }
    # hypothesized mediators
    if (p_m > 1L) {
      tmp <- mapply(function(current_m, j) {
        list(paste0(if (j == 1L) prefix_m else "), ", current_m, " (", label_m),
             flextable::as_sub(j))
      }, current_m = m, j = seq_len(p_m), SIMPLIFY = FALSE, USE.NAMES = FALSE)
      mediator_chunks <- do.call(c, tmp)
      prefix_y <- paste0(")", text_y)
    } else {
      mediator_chunks <- NULL
      prefix_y <- paste0(prefix_m, m, " (", label_m, ")", text_y)
    }
    # construct list of last chunk for note
    last_chunk <- paste0(prefix_y, y, " (Y)", covariate_info, ".",
                         sample_info, boot_info)
    # put everything together
    note <- c(list(first_chunk), independent_chunks,
              mediator_chunks, list(last_chunk))
  } else if (type == "latex") {
    # construct text strings that list independent variables and mediators
    independent <- paste0(x, " ($", label_x,
                          if (p_x > 1L) paste0("_{", seq_len(p_x), "}"),
                          "$)")
    mediators <- paste0(m, " ($", label_m,
                        if (p_m > 1L) paste0("_{", seq_len(p_m), "}"),
                        "$)")
    # construct text string with information on variables
    note <- paste0("\\emph{", text_note, "}. ",
           text_x, paste(independent, collapse = ", "),
           text_m, paste(mediators, collapse = ", "),
           text_y, y, " ($Y$)", covariate_info, ".",
           sample_info, boot_info)
  } else stop("type of table not implemented")
  # return note
  note
}

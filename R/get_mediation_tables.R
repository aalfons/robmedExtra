# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------


## convert object containing results from mediation analysis to list of tables
## for total, direct, indirect effects, and additional information: this does
## the heavy lifting for to_flextable() and to_latex()
#' @importFrom robmed p_value
get_mediation_tables <- function(object, type = c("boot", "data"),
                                 digits = 3L, p_value = FALSE, ...) {
  # initializations
  have_boot <- inherits(object, "boot_test_mediation")
  if (have_boot) {
    type <- match.arg(type)
    level <- object$level
  } else {
    type <- "data"
    level <- NULL
  }
  # compute summary
  summary <- summary(object, type = type, plot = FALSE)
  # extract matrices containing effect summaries
  df_total <- extract_total(summary)
  df_direct <- rbind(extract_a(summary),
                     extract_d(summary),
                     extract_b(summary),
                     extract_direct(summary))
  df_indirect <- extract_indirect(object)
  # if requested, compute p values of bootstrap tests for indirect effects
  if (have_boot && p_value) {
    p_value_indirect <- robmed::p_value(object, parm = "indirect",
                                        digits = digits)
  } else p_value_indirect <- NULL
  # convert matrices to data frames
  df_total <- to_effect_table(df_total, digits = digits, type = type,
                              label = "Total Effect")
  df_direct <- to_effect_table(df_direct, digits = digits, type = type,
                               label = "Direct Effect")
  df_indirect <- to_indirect_table(df_indirect, digits = digits, level = level,
                                   p_value = p_value_indirect)
  # construct note on variables, sample size, and number of bootstrap samples
  variable_info <- do.call(get_variable_info,
                           summary$summary[c("x", "m", "y", "covariates")])
  sample_info <- get_sample_info(summary$summary$n)
  boot_info <- if (have_boot) get_boot_info(object$R)
  # return list of tables
  list(total = df_total, direct = df_direct, indirect = df_indirect,
       note = c(variable_info, sample_info, boot_info))
}


## internal functions to extract effects from results and summaries of
## mediation analysis (code to construct labels is rather ugly)

## extract effect(s) for a path from summary object
extract_a <- function(object) {
  # initializations
  summary <- object$summary
  object <- object$object
  x <- object$fit$x
  m <- object$fit$m
  p_x <- length(x)
  p_m <- length(m)
  # extract effect(s) for each independent variable
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

## extract effect(s) for a path for one independent variable
#' @importFrom stats coef
.extract_a <- function(x, fit) {
  if (inherits(fit, "list")) {
    # multiple mediators
    coef_list <- lapply(fit, function(current_fit) {
      coef(current_fit)[x, , drop = FALSE]
    })
    do.call(rbind, coef_list)
  } else {
    # only one mediator
    coef(fit)[x, , drop = FALSE]
  }
}

## extract effect(s) for d path (if existing) from summary object
#' @importFrom stats coef
extract_d <- function(object) {
  # initializations
  fit_list <- object$summary$fit_mx
  object <- object$object
  m <- object$fit$m
  p_m <- length(m)
  model <- object$fit$model
  have_serial <- !is.null(model) && model == "serial"
  # currently only implemented for two or three hypothesized mediators
  if (have_serial) {
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
  # return effect(s)
  d
}

## extract effect(s) for b path from summary object
#' @importFrom stats coef
extract_b <- function(object) {
  # initializations
  summary <- object$summary
  object <- object$object
  m <- object$fit$m
  p_m <- length(m)
  # extract effect(s)
  if (inherits(summary, "summary_reg_fit_mediation")) {
    b <- coef(summary$fit_ymx)[m, , drop = FALSE]
  } else b <- summary$b
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
  # return effect(s)
  b
}

## extract direct effect(s) of X on Y: this is easier since they are stored
## in a specific component of the summary object
extract_direct <- function(object) {
  # extract effect(s)
  direct <- object$summary$direct
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
  # return effect(s)
  direct
}

## extract total effect(s) of X on Y: this is easier since they are stored
## in a specific component of the summary object
extract_total <- function(object) {
  # extract effect(s)
  total <- object$summary$total
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
  # return effect(s)
  total
}

## extract indirect effect(s) of X on Y: information needs to be collected from
## different components of an object of class "test_mediation", in the same way
## as in the corresponding print() method
extract_indirect <- function(object) {
  # initializations
  p_x <- length(object$fit$x)
  p_m <- length(object$fit$m)
  model <- object$fit$model
  have_simple <- is.null(model) || model == "simple"
  contrast <- object$fit$contrast
  if (is.null(contrast)) contrast <- FALSE
  if (contrast) stop("pairwise contrasts of indirect effects not yet supported")
  # extract effect(s)
  if (inherits(object, "boot_test_mediation")) {
    indirect <- cbind(Data = object$fit$indirect, Boot = object$indirect,
                      if (have_simple) t(object$ci) else object$ci)
  } else {
    indirect <- cbind(object$fit$indirect, object$se,
                      object$statistic, object$p_value)
    cn <- switch(object$alternative, twosided = "Pr(>|z|)",
                 less = "Pr(<z)", greater = "Pr(>z)")
    colnames(indirect) <- c("Estimate", "Std. Error", "z value", cn)
  }
  # construct labels
  if (have_simple) {
    # simple mediation model
    label_x <- "X"
    label_m <- "M"
    label_indirect <- "ab"
  } else if (model == "parallel") {
    # parallel mediators
    seq_m <- seq_len(p_m)
    if (p_x == 1) {
      # one independent variable
      label_x <- rep("X", times = p_m + 1L)
      label_m <- c("...", paste0("M", seq_m))
      label_indirect <- c("total", paste0("a", seq_m, "b", seq_m))
    } else {
      # multiple independent variables
      seq_x <- seq_len(p_x)
      label_x <- rep(paste0("X", seq_x), each = p_m + 1L)
      label_m <- rep(c("...", paste0("M", seq_m)), times = p_x)
      label_indirect <- sapply(
        seq_x, function(j, label) sprintf(label, j),
        label = c("total", paste0("a", seq_m, "%db", seq_m))
      )
    }
  } else if (model == "serial") {
    # serial mediators
    if (p_x == 1) {
      # one independent variable
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
      # multiple independent variables
      seq_x <- seq_len(p_x)
      if (p_m == 2L) {
        label_x <- rep(paste0("X", seq_x), each = 4L)
        label_m <- rep(c("...", "M1", "M2", "M1->M2"), times = p_x)
        label_indirect <- sapply(
          seq_x, function(j, label) sprintf(label, j),
          label = c("total", "a1%db1", "a2%db2", "a1%dd21b2")
        )
      } else {
        label_x <- rep(paste0("X", seq_x), each = 8L)
        label_m <- rep(c("...", "M1", "M2", "M3", "M1->M2",
                         "M1->M3", "M2->M3", "M1->M2->M3"),
                       times = p_x)
        label_indirect <- sapply(
          seq_x, function(j, label) sprintf(label, j),
          label = c("total", "a1%db1", "a2%db2", "a3%db3", "a1%dd21b2",
                    "a1%dd31b3", "a2%dd32b3", "a1%dd21d32b3")
        )
      }
    }
  } else {
    # single mediator but multiple independent variables
    seq_x <- seq_len(p_x)
    label_x <- paste0("X", seq_x)
    label_m <- "M"
    label_indirect <- paste0("ab", seq_x)
  }
  # add labels
  rownames(indirect) <- paste0(label_x, "->", label_m, "->Y (",
                               label_indirect, ")")
  # return effect(s)
  indirect
}


## convert matrix of effect summaries to data frame
to_effect_table <- function(object, digits = 3L, type = "boot",
                            label = "Effect") {
  # make sure label is in plural if we have multiple effects in the table
  plural <- if (nrow(object) > 1) "s" else ""
  label <- paste0(label, plural)
  # extract relevant information
  if (type == "boot") {
    # use bootstrap estimates and  corresponding z tests
    keep <- colnames(object) != "Data"
    object <- object[, keep, drop = FALSE]
  }
  # format numbers and replace missing values
  object <- formatC(object, digits = digits, format = "f")
  object <- gsub("NA", "  ", object, fixed = TRUE)
  # convert to data frame and fix names
  df <- data.frame(rownames(object), object, check.names = FALSE,
                   fix.empty.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- c(label, convert_column_names(object))
  row.names(df) <- NULL
  # return data frame
  df
}

## convert matrix of indirect effect summary to data frame
to_indirect_table <- function(object, digits = 3L, level = 0.95,
                              p_value = NULL) {
  # initializations
  have_boot <- !is.null(level)
  have_p_value <- !is.null(p_value)
  plural <- if (nrow(object) > 1) "s" else ""
  label <- paste0("Indirect Effect", plural)
  # extract relevant information
  if (have_boot) {
    keep <- colnames(object) != "Data"
    object <- object[, keep, drop = FALSE]
  }
  # format numbers
  object <- formatC(object, digits = digits, format = "f")
  # construct data frame and fix column names
  if (have_boot) {
    # format confidence intervals and construct column name
    ci <- paste0("(", object[, "Lower"], ", ", object[, "Upper"], ")")
    ci_label <- paste0(format(100 * level, trim = TRUE),
                       "% Confidence Interval")
    # construct data frame
    df <- data.frame(rownames(object), object[, "Boot", drop = FALSE], ci,
                     check.names = FALSE, fix.empty.names = FALSE,
                     stringsAsFactors = FALSE)
    # fix column names
    names(df) <- c(label, "Estimate", ci_label)
    if (have_p_value) df <- cbind(df, "p Value" = p_value)
  } else {
    # convert to data frame and fix column names
    df <- data.frame(rownames(object), object, check.names = FALSE,
                     fix.empty.names = FALSE, stringsAsFactors = FALSE)
    names(df) <- c(label, convert_column_names(object))
  }
  # fix row names and return data frame
  row.names(df) <- NULL
  df
}


## convert column names from R style to nicer names for tables
convert_column_names <- function(object) {
  # extract column names
  cn <- colnames(object)
  # convert R-style names to nicer ones
  cn[cn == "Boot"] <- "Estimate"
  cn[cn == "z value"] <- "z Statistic"
  cn[cn == "t value"] <- "t Statistic"
  cn[substr(cn, 1L, 3L) == "Pr("] <- "p Value"
  # return column names
  cn
}


## internal functions to get information for table caption

get_variable_info <- function(x, m, y, covariates = character()) {
  # initializations
  p_x <- length(x)
  p_m <- length(m)
  sep <- if (p_x == 1L && p_m == 1L) ", " else "; "
  # construct text string with information on variables
  if (length(covariates) == 0L) covariate_info <- ""
  else {
    covariate_info <- paste0(sep, "control variables: ",
                             paste(covariates, collapse = ", "))
  }
  independent <- paste0(x, " (X", if (p_x > 1L) seq_len(p_x), ")")
  mediators <- paste0(m, " (M", if (p_m > 1L) seq_len(p_m), ")")
  paste0("Independent variable", if (p_x > 1L) "s", ": ",
         paste(independent, collapse = ", "), sep,
         "hypothesized mediator", if (p_m > 1L) "s", ": ",
         paste(mediators, collapse = ", "), sep,
         "dependent variable: ", y, " (Y)",
         covariate_info, ".")
}

# get_sample_info <- function(n, R = NULL) {
#   # construct text string with information on bootstrap samples
#   if (is.null(R)) boot_info <- ""
#   else {
#     boot_info <- paste(", and the number of bootstrap samples is",
#                        formatC(R, big.mark = ","))
#   }
#   # combine with information on sample size
#   paste0("The sample size is n=", n, boot_info, ".")
# }

get_sample_info <- function(n) sprintf("Sample size = %d.", n)

get_boot_info <- function(R) {
  paste0("Number of bootstrap samples = ", formatC(R, big.mark = ","), ".")
}

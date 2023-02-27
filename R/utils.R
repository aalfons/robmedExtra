# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# Convert summary of results from mediation analysis to list of tables -----

## convert summary of results from mediation analysis to list of tables for
## total, direct, indirect effects, and additional information: this does the
## heavy lifting for to_flextable() and to_latex()
## object ... object of class "summary_test_mediation"
#' @importFrom robmed p_value
get_mediation_tables <- function(object, p_value = FALSE, digits = 3L, ...) {
  # initializations
  summary <- object$summary
  object <- object$object
  have_boot <- inherits(object, "boot_test_mediation")
  level <- if (have_boot) object$level
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
  df_total <- to_effect_table(df_total, digits = digits,
                              label = "Total Effect")
  df_direct <- to_effect_table(df_direct, digits = digits,
                               label = "Direct Effect")
  df_indirect <- to_indirect_table(df_indirect, digits = digits, level = level,
                                   p_value = p_value_indirect)
  # construct note on variables, sample size, and number of bootstrap samples
  variable_info <- do.call(get_variable_info,
                           summary[c("x", "m", "y", "covariates")])
  sample_info <- get_sample_info(summary$n)
  boot_info <- if (have_boot) get_boot_info(object$R)
  # return list of tables
  list(total = df_total, direct = df_direct, indirect = df_indirect,
       note = c(variable_info, sample_info, boot_info))
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


# Convert to table of effect summaries -----


# convert summaries of total and direct effects
to_effect_table <- function(object, ...) UseMethod("to_effect_table")

# default method converts matrix of effect summaries to formatted data frame
to_effect_table.default <- function(object, digits = 3L, label = "Effect",
                                    ...) {
  # format numbers and replace missing values
  object <- formatC(object, digits = digits, format = "f")
  object <- gsub("NA", "  ", object, fixed = TRUE)
  # convert to data frame and fix names
  df <- data.frame(rownames(object), object, check.names = FALSE,
                   fix.empty.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- get_table_names(label, object)
  row.names(df) <- NULL
  # return data frame
  df
}

## further prepare the formatted data frame for LaTeX or convert to flextable
to_effect_table.data.frame <- function(object, which = "flextable",
                                       align = NULL, ...) {
  if (which == "flextable") {
    # # format the table body with nicer unicode symbols
    # object[, 1L] <- format_unicode_label(object[, 1L])
    # object[, -1L] <- lapply(object[, -1L], format_unicode_column)
    # # create flextable
    # object <- flextable::qflextable(object)
    # # set column alignment
    # object <- flextable::valign(object, valign = "bottom", part = "header")
    # for (j in seq_along(align)) {
    #   object <- flextable::align(object, j = j, align = align[j], part = "all")
    # }
  } else if (which == "latex") {
    # format the table header for LaTeX
    names(object) <- format_latex_header(object)
    # format the table body for LaTeX
    object[, 1L] <- format_latex_label(object[, 1L])
    object[, -1L] <- lapply(object[, -1L], format_latex_column)
  } else stop("type of table not implemented")
  # return prepared or converted object
  object
}


# convert summaries of indirect effects
to_indirect_table <- function(object, ...) UseMethod("to_indirect_table")

# default method converts matrix of effect summaries to formatted data frame
to_indirect_table.default <- function(object, digits = 3L, level = 0.95,
                                      p_value = NULL, ...) {
  # initializations
  have_boot <- !is.null(level)
  have_p_value <- !is.null(p_value)
  label <- "Indirect Effect"
  # format numbers
  object <- formatC(object, digits = digits, format = "f")
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
    if (have_p_value) df <- cbind(df, "p Value" = p_value)
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

# further prepare the formatted data frame for LaTeX or convert to flextable
to_indirect_table.data.frame <- function(object, which = "flextable",
                                         width, align, ...) {
  if (which == "flextable") {
    # # first perform the same formatting as for total and direct effects
    # object <- to_effect_table(object, which = which, align = align)
  } else if (which == "latex") {
    # first perform the same formatting as for total and direct effects
    object <- to_effect_table(object, which = which)
    # wrap confidence interval in \multicolumn statement
    cn <- names(object)
    pos_ci <- grep("Confidence Interval", cn, fixed = TRUE)
    if (length(pos_ci) == 1L) {
      multicolumn <- "\\multicolumn{%d}{%s}{%s}"
      names(object)[pos_ci] <- sprintf(multicolumn, width, align, cn[pos_ci])
      object[[pos_ci]] <- sprintf(multicolumn, width, align, object[[pos_ci]])
    }
  } else stop("type of table not implemented")
  # return prepared or converted object
  object
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

# format the table header for LaTeX
format_latex_header <- function(object) {
  # extract column names
  cn <- names(object)
  # format for LaTeX
  cn <- gsub("^(([ptz]) )", "$\\2$ ", cn, fixed = FALSE)
  cn <- gsub("%", "\\%", cn, fixed = TRUE)
  # return formatted column names
  cn
}

# format the label column of a table for LaTeX
format_latex_label <- function(label) {
  # first call format_latex_column(), which puts cells in math mode
  label <- format_latex_column(label)
  # format arrows and ellipses nicely
  label <- gsub("->", " \\rightarrow ", label, fixed = TRUE)
  label <- gsub("...", " \\ldots ", label, fixed = TRUE)
  # ensure that indices are formatted as subscripts
  label <- gsub("([0-9]+)", "_{\\1}", label, fixed = FALSE)
  # ensure that a space interrupts math mode so that the space is maintained
  label <- gsub(" (", "$ $(", label, fixed = TRUE)
  # ensure that label for total indirect effect is in text mode
  label <- gsub("$(total)$", "(total)", label, fixed = TRUE)
  # return label
  label
}

# format a column of a table for LaTeX
format_latex_column <- function(column) {
  paste0("$", column, "$")
}

# format the table note for LaTeX
format_latex_note <- function(note) {
  # ensure that symbols for variables (in parentheses) are in math mode
  note <- gsub("\\(([MXY])\\)", "($\\1$)", note, fixed = FALSE)
  note <- gsub("\\(([MXY])([0-9]+)\\)", "($\\1_{\\2}$)", note, fixed = FALSE)
  # return note
  note
}


## Constructs notes for table -----

# construct note on variables
get_variable_info <- function(x, m, y, covariates = character()) {
  # initializations
  p_x <- length(x)
  p_m <- length(m)
  sep <- if (p_x == 1L && p_m == 1L) ", " else "; "
  # construct text string with information on control variables
  if (length(covariates) == 0L) covariate_info <- ""
  else {
    covariate_info <- paste0(sep, "control variables: ",
                             paste(covariates, collapse = ", "))
  }
  # construct text strings that list independent variables and mediators
  independent <- paste0(x, " (X", if (p_x > 1L) seq_len(p_x), ")")
  mediators <- paste0(m, " (M", if (p_m > 1L) seq_len(p_m), ")")
  # construct text string with information on variables
  paste0("Independent variable", if (p_x > 1L) "s", ": ",
         paste(independent, collapse = ", "), sep,
         "hypothesized mediator", if (p_m > 1L) "s", ": ",
         paste(mediators, collapse = ", "), sep,
         "dependent variable: ", y, " (Y)",
         covariate_info, ".")
}

# construct note on sample size
get_sample_info <- function(n) {
  sprintf("Sample size = %d.", n)
}

# construct note on number of bootstrap samples
get_boot_info <- function(R) {
  paste0("Number of bootstrap samples = ", formatC(R, big.mark = ","), ".")
}

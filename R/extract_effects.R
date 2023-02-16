# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------


## internal functions to extract effects from regression summaries


# extract effect(s) for a path for one independent variable
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

# extract effect(s) for a path
extract_a <- function(x, m, fit) {
  # initializations
  p_x <- length(x)
  p_m <- length(m)
  # extract effect(s) for each independent variable
  if (p_x == 1L) {
    # only one independent variable
    a <- .extract_a(x, fit)
  } else {
    # multiple independent variables
    a_list <- lapply(x, .extract_a, fit)
    a <- do.call(rbind, a_list)
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

# extract effect(s) for b path
extract_b <- function(m, fit) {
  # initializations
  p_m <- length(m)
  # extract effect(s)
  b <- coef(fit)[m, , drop = FALSE]
  rownames(b) <- NULL
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

# extract effect(s) for d path
extract_d <- function(m, fit_list) {
  # initializations
  p_m <- length(m)
  # currently only implemented for two or three hypothesized mediators
  if (p_m == 2L) {
    # two serial mediators
    d <- coef(fit_list[[m[2L]]])[m[1L], , drop = FALSE]
    rownames(d) <- "M1->M2 (d21)"
  } else if (p_m == 3L) {
    # three serial mediators
    d <- rbind(coef(fit_list[[m[2L]]])[m[1L], , drop = FALSE],
               coef(fit_list[[m[3L]]])[m[1L:2L], ])
    rownames(d) <- c("M1->M2 (d21)", "M1->M3 (d31)", "M2->M3 (d32)")
  } else d <- NULL
  # return effect(s)
  d
}

# extract direct effect(s): this is easier since they are stored in a specific
# component of the summary object
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

# extract total effect(s): this is easier since they are stored in a specific
# component of the summary object
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

# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------


## internal functions to extract effects from regression summaries


# TODO: add unname() since different names will be eventually used
#       (see also extract_effect.R in package robmed)

# TODO: extract effect(s) for a path for one independent variable
# extract_a <- function(x, fit) {
#   if (inherits(fit, "list")) {
#     # multiple mediators
#     sapply(fit, function(current_fit) coef(current_fit)[x, , drop = FALSE])
#   } else {
#     # only one mediator
#     coef(fit)[x, , drop = FALSE]
#   }
# }

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
  data.frame(Effect = paste0(label_m, "->Y (", label_b, ")"), b,
             check.names = FALSE)
}

# TODO: extract effect(s) for d path
# extract_d <- function(m, fit_list) {
#   # initializations
#   p_m <- length(m)
#   # currently only implemented for two or three hypothesized mediators
#   if (p_m == 2L) {
#     # two serial mediators
#     d <- unname(coef(fit_list[[m[2L]]])[m[1L]])
#   } else if (p_m == 3L) {
#     # three serial mediators
#     d <- c(coef(fit_list[[m[2L]]])[m[1L]], coef(fit_list[[m[3L]]])[m[1L:2L]])
#     names(d) <- paste(names(d), m[c(2L, 3L, 3L)], sep = "->")
#   } else d <- NULL
#   # return effect(s)
#   d
# }

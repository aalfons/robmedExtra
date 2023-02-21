# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam


to_latex <- function(object, type = c("boot", "data"), digits = 3L,
                     p_value = FALSE, align = "lrrrr", align_ci = "c",
                     ...) {
  # call workhorse function to format tables
  tables <- get_mediation_tables(object, type = type, digits = digits,
                                 p_value = p_value)
  # add alignment specification and set class
  tables$align <- align
  tables$align_ci <- align_ci
  class(tables) <- "latex_tables"
  # return object for LaTeX tables
  tables
}


print.latex_tables <- function(x, ...) {
  # initialize LaTeX table
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{", x$align, "}\n", sep = "")
  cat("\\hline\\noalign{\\smallskip}\n")
  # write table header for total effects
  cat(paste(names(x$total), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body for total effects
  total <- lapply(x$total, fix_cells)
  lines <- paste(do.call(paste_amp, total), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table header for direct effects
  cat(paste(names(x$direct), collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body for direct effects
  direct <- lapply(x$direct, fix_cells)
  lines <- paste(do.call(paste_amp, direct), "\\\\ \n")
  cat(lines, sep = "")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # some preparations for table of indirect effects
  indirect <- lapply(x$indirect, fix_cells)
  cn <- names(indirect)
  have_p_value <- "p Value" %in% cn
  which_ci <- grep("Confidence Interval", cn, fixed = TRUE)
  # write table header for indirect effects
  cn[which_ci] <- paste0("\\multicolumn{",
                         if (have_p_value) 2 else 3, "}{",
                         x$align_ci, "}{",
                         gsub("%", "\\%", cn[which_ci], fixed = TRUE),
                         "}")
  cat(paste(cn, collapse = " & "), "\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  # write table body for indirect effects
  indirect[[which_ci]] <- paste0("\\multicolumn{",
                                 if (have_p_value) 2 else 3, "}{",
                                 x$align_ci, "}{", indirect[[which_ci]], "}")
  lines <- paste(do.call(paste_amp, indirect), "\\\\ \n")
  cat(lines, sep = "")
  # finalize LaTeX table
  cat("\\noalign{\\smallskip}\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  # add note
  cat("\\emph{Note}.", x$note)
}

# internal functions
fix_cells <- function(x) {
  x <-gsub("->", " \\rightarrow ", x, fixed = TRUE)
  paste0("$", x, "$")
}
paste_amp <- function(..., sep = " & ") paste(..., sep = sep)

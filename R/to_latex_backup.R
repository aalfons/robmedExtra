## Generate LaTeX code for table
##
## Generates LaTeX code that displays a table containing results of mediation
## analyis.
##
## @param test_model an object inheriting from class
## \code{"\link{test_mediation}"} or a list of objects of that class.
## @param digits a positive integer, which determines the number of decimals
## that should be displayed in the table. The default is to display 4 decimals.
##
## @return A character object containing the latex code that produces a table of
## the results of the test_mediation object.
##
## @examples
## data("BSG2014")
##
## boot_robust <- test_mediation(TeamCommitment ~
##                                 m(TaskConflict) +
##                                   ValueDiversity,
##                               data = BSG2014)
## to_latex(boot_robust)
##
## @importFrom xtable xtable print.xtable
##
## @export
# to_latex <- function(test_model, digits = 4) {
#   table <- to_flextable(test_model = test_model, digits = digits)
#   dataset <- table$body$data
#   indirect_start <- which(dataset[,1] == "Indirect Effects")
#
#   full_table <- xtable(dataset, align = "llcccc")
#   capture.output(final_table_character <- print(full_table, booktabs = T,
#                                                 hline.after = (-1:nrow(dataset)),
#                                                 include.rownames = F, type = "latex"))
#
#   final_table_character <- gsub(pattern = "(\\(-[^)]*\\))",
#                                 replacement = paste0("\\\\multicolumn{2}{c}{",
#                                                      "\\1", "}"),
#                                 x = final_table_character)
#
#   final_table_character <- gsub(pattern = "(Confidence Interval &  )",
#                                 replacement = "\\\\multicolumn{2}{c}{Confidence Interval}",
#                                 x = final_table_character)
#
#   final_table_character <- gsub(pattern = "&  &",
#                                 replacement = "&",
#                                 x = final_table_character)
#
#   cat(final_table_character, sep = "\n")
#   return(final_table_character)
# }

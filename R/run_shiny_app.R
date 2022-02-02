# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Shiny app: Simulation for mediation analysis with outliers
#'
#' Compare various bootstrap procedures for mediation analysis on simulated
#' data.
#'
#' The default settings follow the simulation design of Zu & Yuan (2010).  You
#' can adjust the total number of observations, the values of the coefficients
#' in the mediation model, the number of outliers, as well as the outlier
#' shifts in the individual variables.
#'
#' For each selected methods, the bootstrap distribution of the indirect
#' effect is shown together with a shaded area representing the 95\% confidence
#' interval.  If the app does not update quickly enough, try to reduce the
#' number of bootstrap samples.
#'
#' @author Andreas Alfons
#'
#' @references
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2021) A robust bootstrap test
#' for mediation analysis.  \emph{Organizational Research Methods},
#' \doi{10.1177/1094428121999096}.
#'
#' Zu, J. and Yuan, K.-H. (2010) Local influence and robust procedures for
#' mediation analysis. \emph{Multivariate Behavioral Research}, \bold{45}(1),
#' 1--44.
#'
#' @seealso \code{\link{test_mediation}}
#'
#' @examples
#' \dontrun{
#' run_shiny_app()
#' }
#'
#' @keywords documentation
#'
#' @importFrom shiny runApp
#' @importFrom robmed test_mediation
#' @importFrom ggplot2 geom_vline scale_color_manual scale_fill_manual theme
#' @importFrom graphics legend
#' @importFrom stats rnorm
#' @export

run_shiny_app <- function() {
  # find application folder
  folder <- system.file("shiny-examples", "simulation", package = "robmedShiny")
  if (folder == "") {
    stop("Could not find shiny app.  Try re-installing package 'robmedShiny'.")
  }
  # run shiny app
  shiny::runApp(folder, display.mode = "normal")
}

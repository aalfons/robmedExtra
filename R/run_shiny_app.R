# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Shiny apps: Graphical user interface and data simulation
#'
#' Run one of the following \pkg{shiny} apps: a graphical user interface
#' for package \pkg{robmed}, or an app for exploring the behavior of various
#' bootstrap procedures for mediation analysis on simulated data.
#'
#' Function \code{start_GUI()} is a simple wrapper for
#' \code{run_shiny_app(which = "GUI")} to start the graphical user interface
#' for package \pkg{robmed}.
#'
#' This graphical user interface allows to select or import a data set, and to
#' perform the robust bootstrap test ROBMED (Alfons et al., 2022a) or the OLS
#' bootstrap test (e.g., Preacher & Hayes, 2004, 2008).
#'
#' In the app for exploring the behavior of  various bootstrap procedures for
#' mediation analysis on simulated data (\code{which = "simulation"}), the
#' default settings follow the simulation design of Zu & Yuan (2010).  You can
#' adjust the total number of observations, the values of the coefficients in
#' the mediation model, the number of outliers, as well as the outlier shifts
#' in the individual variables.
#'
#' For each selected methods, the bootstrap distribution of the indirect
#' effect is shown together with a shaded area representing the 95\% confidence
#' interval.  If the app does not update quickly enough, try to reduce the
#' number of bootstrap samples.
#'
#' @param which  a character string indicating which \pkg{shiny} app to run.
#' Possible values are \code{"GUI"} (the default) to start a graphical user
#' interface for package \pkg{robmed}, or \code{"simulation"} to run an app
#' for exploring the behavior of various bootstrap procedures for mediation
#' analysis on simulated data.
#'
#' @author
#' The graphical user interface for package \pkg{robmed} was written by
#' Andreas Alfons and Aurore Archimbaud based on an initial prototype by
#' Vincent Drenth.
#'
#' The app for exploring the behavior of various bootstrap procedures for
#' mediation analysis on simulated data was written by Andreas Alfons.
#'
#' @references
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022a) A Robust Bootstrap Test
#' for Mediation Analysis.  \emph{Organizational Research Methods},
#' \bold{25}(3), 591--617.  doi:10.1177/1094428121999096.
#'
#' Alfons, A., Ates, N.Y. and Groenen, P.J.F. (2022b) Robust Mediation
#' Analysis: The \R Package \pkg{robmed}.  \emph{Journal of Statistical
#' Software}, \bold{103}(13), 1--45.  doi:10.18637/jss.v103.i13.
#'
#' Preacher, K.J. and Hayes, A.F. (2004) SPSS and SAS Procedures for Estimating
#' Indirect Effects in Simple Mediation Models.  \emph{Behavior Research
#' Methods, Instruments, & Computers}, \bold{36}(4), 717--731.
#' doi:10.3758/bf03206553.
#'
#' Preacher, K.J. and Hayes, A.F. (2008) Asymptotic and Resampling Strategies
#' for Assessing and Comparing Indirect Effects in Multiple Mediator Models.
#' \emph{Behavior Research Methods}, \bold{40}(3), 879--891.
#' doi:10.3758/brm.40.3.879.
#'
#' Zu, J. and Yuan, K.-H. (2010) Local Influence and Robust Procedures for
#' Mediation Analysis.  \emph{Multivariate Behavioral Research}, \bold{45}(1),
#' 1--44.  doi:10.1080/00273170903504695.
#'
#' @seealso \code{\link{test_mediation}}
#'
#' @examples
#' \dontrun{
#'
#' # start graphical user interface
#' start_GUI()
#'
#' # run shiny app for simulating data
#' run_shiny_app("simulation")
#' }
#'
#' @keywords documentation
#'
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom flextable htmltools_value
#' @importFrom ggplot2 scale_color_manual theme
#' @importFrom grDevices dev.off pdf png
#' @importFrom robmed reg_control test_mediation weight_plot
#' @importFrom shiny runApp
#' @importFrom utils zip
#' @export

run_shiny_app <- function(which = c("GUI", "simulation")) {
  # initializations
  which <- match.arg(which)
  # find application folder
  folder <- system.file("shiny", which, package = "robmedExtra")
  if (folder == "") {
    stop("could not find shiny app, try re-installing package 'robmedExtra'")
  }
  # run shiny app
  shiny::runApp(folder, display.mode = "normal")
}


#' @rdname run_shiny_app
#' @export

start_GUI <- function() run_shiny_app("GUI")

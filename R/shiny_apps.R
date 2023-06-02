# ------------------------------------
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ------------------------------------

#' Shiny apps: Graphical user interface and data simulation
#'
#' Run one of the following \pkg{shiny} apps: a graphical user interface
#' for package \pkg{robmed}, or an app for exploring the behavior of various
#' bootstrap procedures for mediation analysis on simulated data.
#'
#' @param which  a character string indicating which \pkg{shiny} app to run.
#' Possible values are \code{"GUI"} (the default) to start a graphical user
#' interface for package \pkg{robmed}, or \code{"simulation"} to run an app
#' for exploring the behavior of various bootstrap procedures for mediation
#' analysis on simulated data.
#'
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom flextable htmltools_value
#' @importFrom ggplot2 geom_vline labs scale_color_manual scale_fill_manual
#' theme
#' @importFrom graphics legend
#' @importFrom grDevices dev.off pdf png
#' @importFrom robmed density_plot reg_control test_mediation weight_plot
#' @importFrom shiny runApp
#' @importFrom stats rnorm
#' @importFrom utils zip
#'
#' @noRd

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


#' Graphical user interface for (robust) mediation analysis
#'
#' Open a \pkg{shiny} app that provides a graphical user interface for (robust)
#' mediation analysis via package \pkg{robmed}.
#'
#' The graphical user interface allows to select or import a data set, to
#' perform the robust bootstrap test ROBMED (Alfons et al., 2022a) or the OLS
#' bootstrap test (e.g., Preacher & Hayes, 2004, 2008), and to easily export
#' results for reporting (including a replication file in the form of an \R
#' script).
#'
#' @param data  optional; a data frame to be selected by default in the
#' graphical user interface.  Note that if the supplied object is not a
#' data frame or does not exist in the global environment, the graphical
#' user interface is opened as if the argument were not supplied.
#'
#' @return
#' No return value, the function is called for its side effect of starting a
#' \pkg{shiny} app.
#'
#' @note
#' The graphical user interface is still experimental.  It may change in future
#' versions based on user feedback.
#'
#' @author
#' Andreas Alfons and Aurore Archimbaud, based on an initial prototype by
#' Vincent Drenth.
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
#' @seealso \code{\link{test_mediation}}
#'
#' @examples
#' # run example only from an interactive R session
#' if (interactive()) {
#'   # start graphical user interface
#'   robmed_GUI()
#' }
#'
#'
#' @export

robmed_GUI <- function(data = NULL) {
  # get name of supplied data frame, if any
  if (is.null(data)) df_name <- ""
  else {
    df_name <- deparse(substitute(data))
    if (!exists(df_name, envir = .GlobalEnv)) {
      msg <- sprintf("object '%s' does not exist in the R environment", df_name)
      warning(msg, immediate. = TRUE)
      df_name <- ""
    } else if (!is.data.frame(data)) {
      warning(sprintf("object '%s' is not a data frame", df_name),
              immediate. = TRUE)
      df_name <- ""
    }
  }
  # set argument to pass the name of the data frame to the shiny app
  GUI_args$set(df_name = df_name)
  # call initernal function to run the shiny app
  run_shiny_app("GUI")
}


#' Open a 'shiny' app for exploring mediation analysis on simulated data
#'
#' Open a \pkg{shiny} app for exploring the behavior of various bootstrap
#' procedures for mediation analysis on simulated data.
#'
#' The default settings are similar to one of the simulation designs of Alfons,
#' Ates & Groenen (2022a).  You can adjust the total number of observations,
#' the values of the coefficients in the (simple) mediation model, the number
#' of outliers, as well as outlier scaling factors and shifts in the individual
#' variables.
#'
#' For each selected method, the bootstrap distribution of the indirect
#' effect is shown together with a shaded area representing the 95\% confidence
#' interval.  If the app does not update quickly enough, try to reduce the
#' number of bootstrap samples.
#'
#' @return
#' No return value, the function is called for its side effect of starting a
#' \pkg{shiny} app.
#'
#' @author
#' Andreas Alfons
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
#' Yuan, Y. and MacKinnon, D.P. (2014) Robust Mediation Analysis Based on
#' Median Regression.  \emph{Psychological Methods}, \bold{19}(1), 1--20.
#' doi:10.1037/a0033820.
#'
#' Zu, J. and Yuan, K.-H. (2010) Local Influence and Robust Procedures for
#' Mediation Analysis.  \emph{Multivariate Behavioral Research}, \bold{45}(1),
#' 1--44.  doi:10.1080/00273170903504695.
#'
#' @seealso \code{\link{test_mediation}}
#'
#' @examples
#' # run example only from an interactive R session
#' if (interactive()) {
#'   # start graphical user interface
#'   simulation_app()
#' }
#'
#' @export

simulation_app <- function() run_shiny_app("simulation")

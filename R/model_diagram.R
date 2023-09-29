# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


#' Diagram of a mediation model
#'
#' Draw a diagram of a mediation model. Control variables are by default drawn
#' in gray to visually distinguish them from the main variables of interest.
#'
#' Methods first call \code{\link{setup_model_diagram}()} to obtain all
#' necessary information to produce the plot, then the
#' \code{"setup_model_diagram"} method is called to produce the plot.
#'
#' @param object  an object inheriting from class
#' \code{"\link[robmed]{fit_mediation}"} or
#' \code{"\link[robmed]{test_mediation}"} containing results from (robust)
#' mediation analysis.  For the default method, the names of the independent
#' variables of interest can also be specified via this argument.
#' @param width,height  numeric values giving the width and height of the boxes
#' representing the variables in the model diagram.
#' @param x  a character vector specifying the names of the independent
#' variables of interest to be displayed in the model diagram.
#' @param y  a character string specifying the name of the dependent
#' variable to be displayed in the model diagram.
#' @param m  a character vector specifying the names of the hypothesized
#' mediator variables to be displayed in the model diagram.
#' @param covariates  a character vector specifying the names of additional
#' covariates to be included as control variables in the model diagram.
#' @param model  a character string specifying the type of model in case of
#' multiple mediators.  Possible values are \code{"parallel"} (the default) for
#' the parallel multiple mediator model, or \code{"serial"} for the serial
#' multiple mediator model.  This is only relevant for drawing a diagram of a
#' model with multiple hypothesized mediators.
#' @param \dots  additional arguments to be passed down, eventually to
#' \code{\link[ggplot2]{geom_rect}()}, \code{\link[ggplot2]{geom_text}()},
#' and \code{\link[ggplot2]{geom_segment}()}.
#'
#' @return An object of class \code{"\link[ggplot2]{ggplot}"}.
#'
#' @author Andreas Alfons
#'
#' @seealso
#' \code{\link{fit_mediation}()}, \code{\link{test_mediation}()},
#' \code{\link{setup_model_diagram}()}
#'
#' @examples
#' ## drawing diagrams from scratch
#'
#' # simple mediation model
#' model_diagram(x = "ValueDiversity",
#'               y = "TeamCommitment",
#'               m = "TaskConflict",
#'               size = 3.5)
#'
#' # serial multiple mediators
#' model_diagram(x = "ValueDiversity",
#'               y = "TeamScore",
#'               m = c("TaskConflict", "TeamCommitment"),
#'               model = "serial",
#'               size = 3.5)
#'
#' # parallel multiple mediators and control variables
#' model_diagram(x = "SharedLeadership",
#'               y = "TeamPerformance",
#'               m = c("ProceduralJustice", "InteractionalJustice"),
#'               covariates = c("AgeDiversity", "GenderDiversity"),
#'               model = "parallel",
#'               size = 3.5)
#'
#'
#' ## draw a diagram from an object containing results from robust
#' ## mediation analysis
#'
#' # load data
#' data("BSG2014")
#' # seed to be used for the random number generator
#' seed <- 20211117
#' # perform mediation analysis via robust bootstrap test ROBMED
#' set.seed(seed)
#' robust_boot <- test_mediation(BSG2014,
#'                               x = "ValueDiversity",
#'                               y = "TeamCommitment",
#'                               m = "TaskConflict",
#'                               robust = TRUE)
#' # create model diagram
#' model_diagram(robust_boot, size = 3.5)
#'
#' @export

model_diagram <- function(object, ...) UseMethod("model_diagram")


#' @rdname model_diagram
#' @export

model_diagram.fit_mediation <- function(object, width = 8, height = 2, ...) {
  # obtain relevant information
  setup <- setup_model_diagram(object, width = width, height = height)
  # call method of setup object
  model_diagram(setup, ...)
}


#' @rdname model_diagram
#' @export

model_diagram.test_mediation <- function(object, width = 8, height = 2, ...) {
  # obtain relevant information
  setup <- setup_model_diagram(object, width = width, height = height)
  # call method of setup object
  model_diagram(setup, ...)
}


#' @rdname model_diagram
#' @export

model_diagram.default <- function(object, x = object, y, m, covariates = NULL,
                                  model = c("parallel", "serial"), width = 8,
                                  height = 2, ...) {
  # obtain relevant information
  setup <- setup_model_diagram(x = x, y = y, m = m, covariates = covariates,
                               model = model, width = width, height = height)
  # call method of setup object
  model_diagram(setup, ...)
}


#' @rdname model_diagram
#' @importFrom ggplot2 aes_string coord_fixed expansion ggplot
#' scale_color_manual scale_x_continuous scale_y_continuous theme_void
#' @export

model_diagram.setup_model_diagram <- function(object, ...) {
  # define style of arrow heads
  # generate plot
  ggplot() +
    geom_diagram_arrow(mapping = aes_string(x = "x", y = "y", xend = "xend",
                                            yend = "yend", color = "type"),
                       data = object$arrows, ...) +
    geom_diagram_box(mapping = aes_string(xmin = "xmin", xmax = "xmax",
                                          ymin = "ymin", ymax = "ymax",
                                          color = "type"),
                     data = object$boxes, ...) +
    geom_diagram_label(mapping = aes_string(x = "x", y = "y", label = "label",
                                           color = "type"),
                       data = object$boxes, ..., default_size = object$size) +
    coord_fixed() +
    scale_color_manual("", values = object$colors) +
    scale_x_continuous(expand = object$expand) +
    scale_y_continuous(expand = object$expand) +
    theme_void()
}


## custom geom for arrows
# - set suitable defaults for certain arguments: note that handling the line
#   width properly is somewhat complicated since it can be set via different
#   arguments ('linewidth" or 'lwd')
# - always use the default stat = "identity" and position = "identity"
# - avoid passing unknown arguments intended for other geoms
#' @importFrom grid arrow unit
#' @importFrom ggplot2 geom_segment standardise_aes_names
geom_diagram_arrow <- function(...,
                               # arguments with different default behavior
                               arrow = NULL, show.legend = FALSE,
                               # ignored arguments
                               stat, position, angle, bg, family, fill,
                               fontface, hjust, lineheight, size, vjust,
                               parse, nudge_x, nudge_y, check_overlap) {
  # the default arrow head needs to be set here rather than in the function
  # definition, otherwise there is an error that has to do with lazy evaluation
  # ("promise already under evaluation: recursive default argument reference")
  if (is.null(arrow)) {
    arrow <- arrow(angle = 15, length = unit(8, "pt"),
                   ends = "last", type = "closed")
  }
  # extract argument names
  arguments <- list(..., arrow = arrow, show.legend = show.legend)
  argument_names <- names(arguments)
  # replace argument names with standardized ones
  standardized_names <- standardise_aes_names(argument_names)
  names(arguments) <- standardized_names
  # set default arguments if not specified otherwise
  if (is.null(arguments$linewidth)) arguments$linewidth <- 0.5
  # call existing geom function
  do.call(geom_segment, arguments)
}


## custom geom for boxes
# - set suitable defaults for certain arguments: note that handling the line
#   width and fill color properly is somewhat complicated since they can be
#   set via different arguments ('linewidth" or 'lwd', 'fill' or 'bg')
# - always use the default stat = "identity" and position = "identity"
# - avoid passing unknown arguments intended for other geoms
#' @importFrom ggplot2 geom_rect standardise_aes_names
geom_diagram_box <- function(...,
                             # arguments with different default behavior
                             show.legend = FALSE,
                             # ignored arguments
                             stat, position, angle, family, fontface, hjust,
                             lineheight, size, vjust, arrow, arrow.fill,
                             parse, nudge_x, nudge_y, check_overlap) {
  # extract argument names
  arguments <- list(..., show.legend = show.legend)
  argument_names <- names(arguments)
  # replace argument names with standardized ones
  standardized_names <- standardise_aes_names(argument_names)
  names(arguments) <- standardized_names
  # set default arguments if not specified otherwise
  if (is.null(arguments$fill)) arguments$fill <- "white"
  if (is.null(arguments$linewidth)) arguments$linewidth <- 0.5
  # call existing geom function
  do.call(geom_rect, arguments)
}


## custom geom for label
# - set suitable defaults for certain arguments: note that handling the text
#   size properly is somewhat complicated since it can be set via different
#   arguments ('size" or 'cex')
# - always use the default stat = "identity" and position = "identity"
# - avoid passing unknown arguments intended for other geoms
#' @importFrom ggplot2 geom_text standardise_aes_names
geom_diagram_label <- function(...,
                               # argument to pass along the default size
                               default_size,
                               # arguments with different default behavior
                               show.legend = FALSE,
                               # ignored arguments
                               stat, position, angle, bg, fill, linetype,
                               linewidth, lty, lwd, arrow, arrow.fill) {
  # extract argument names
  arguments <- list(..., show.legend = show.legend)
  argument_names <- names(arguments)
  # replace argument names with standardized ones
  standardized_names <- standardise_aes_names(argument_names)
  names(arguments) <- standardized_names
  # set default arguments if not specified otherwise
  if (is.null(arguments$size)) arguments$size <- default_size
  # call existing geom function
  do.call(geom_text, arguments)
}

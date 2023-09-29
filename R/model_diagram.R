# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


#' @export
model_diagram <- function(object, ...) UseMethod("model_diagram")

#' @export
model_diagram.fit_mediation <- function(object, width = 8, height = 2, ...) {
  # obtain relevant information
  setup <- setup_model_diagram(object, width = width, height = height)
  # call method of setup object
  model_diagram(setup, ...)
}

#' @export
model_diagram.test_mediation <- function(object, width = 8, height = 2, ...) {
  # obtain relevant information
  setup <- setup_model_diagram(object, width = width, height = height)
  # call method of setup object
  model_diagram(setup, ...)
}

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

#' @importFrom ggplot2 coord_fixed expansion ggplot scale_color_manual
#' scale_x_continuous scale_y_continuous theme_void
#' @export
model_diagram.setup_model_diagram <- function(object, ...) {
  # define style of arrow heads
  # generate plot
  ggplot() +
    geom_diagram_arrow(mapping = aes(x = x, y = y, xend = xend, yend = yend,
                                     color = type),
                       data = object$arrows, ...) +
    geom_diagram_box(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                   ymax = ymax,color = type),
                     data = object$boxes, ...) +
    geom_diagram_label(mapping = aes(x = x, y = y, label = label, color = type),
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
#' @importFrom grid arrow
#' @importFrom ggplot2 geom_segment
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
#' @importFrom ggplot2 geom_rect
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
#' @importFrom ggplot2 geom_text
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

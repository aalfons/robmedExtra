# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


#' Set up information for a diagram of a mediation model
#'
#' Construct the relevant information for drawing a diagram of a mediation
#' model.
#'
#' This function is used internally by \code{\link{model_diagram}()}.  It
#' may also be useful for users who want to produce a similar plot, but who
#' want more control over what information to display or how to display that
#' information.
#'
#' Specifically, a user can modify the positions of the boxes representing the
#' variables in the \code{boxes} component of the returned object.  However,
#' note that the arrows connecting the boxes in the \code{arrows} component
#' need to be modified accordingly.
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
#' multiple mediator model.  This is only relevant for setting up a diagram of
#' a model with multiple hypothesized mediators.
#' @param \dots  additional arguments to be passed down.
#'
#' @return An object of class \code{"setup_model_diagram"} with the following
#' components:
#' \item{boxes}{a data frame containing the coordinates and other relevant
#' information on the boxes representing the variables in the model diagram.}
#' \item{arrows}{a data frame containing the coordinates and other relevant
#' information on the arrows representing the connections between the variables
#' in the model diagram.}
#' \item{colors}{a character vector of length two giving the colors to be used
#' for main variables of interest and for control variables, respectively.}
#' \item{size}{a numeric value giving the default text size.}
#' \item{expand}{a numeric vector giving the default scale expansion vectors
#' for the plot, see \code{\link[ggplot2]{expansion}()}.}
#'
#' @author Andreas Alfons
#'
#' @seealso
#' \code{\link{fit_mediation}()}, \code{\link{test_mediation}()},
#' \code{\link{model_diagram}()}
#'
#' @examples
#' ## parallel multiple mediators and control variables
#'
#' # set up information for the model diagram
#' setup <- setup_model_diagram(x = "ValueDiversity",
#'                              y = "TeamCommitment",
#'                              m = "TaskConflict")
#'
#' # draw the diagram
#' model_diagram(setup)
#'
#' # draw a similar diagram, but without the boxes
#' arrow_head <- grid::arrow(angle = 15, length = unit(8, "pt"),
#'                           ends = "last", type = "closed")
#' ggplot() +
#'   geom_segment(mapping = aes(x = x, y = y, xend = xend,
#'                              yend = yend),
#'                data = setup$arrows, arrow = arrow_head) +
#'   geom_text(mapping = aes(x = x, y = y, label = Label),
#'             data = setup$boxes) +
#'   coord_fixed() +
#'   xlim(min(setup$boxes$xmin), max(setup$boxes$xmax)) +
#'   ylim(min(setup$boxes$ymin), max(setup$boxes$ymax)) +
#'   theme_void()
#'
#' @export

setup_model_diagram <- function(object, ...) UseMethod("setup_model_diagram")


#' @rdname setup_model_diagram
#' @export

setup_model_diagram.fit_mediation <- function(object, ...) {
  # extract relevant information and call default method
  setup_model_diagram(x = object$x, y = object$y, m = object$m,
                      covariates = object$covariates,
                      model = object$model, ...)
}


#' @rdname setup_model_diagram
#' @export

setup_model_diagram.test_mediation <- function(object, ...) {
  # extract relevant information and call next method
  setup_model_diagram(object$fit, ...)
}


#' @rdname setup_model_diagram
#' @importFrom ggplot2 expansion
#' @export

setup_model_diagram.default <- function(object, x = object, y, m,
                                        covariates = NULL,
                                        model = c("parallel", "serial"),
                                        width = 8, height = 2, ...) {

  # check independent variable
  if (missing(x)) stop("no independent variable supplied")
  else if (!is.character(x)) {
    stop("independent variable must be specified as a character vector")
  }
  p_x <- length(x)
  if (p_x == 0L) stop("at least one independent variable required")
  # check dependent variable
  if (missing(y)) stop("no dependent variable supplied")
  else if (!is.character(y) || length(y) != 1L) {
    stop("dependent variable must be specified as a single character string")
  }
  # check hypothesized mediator variables
  if (missing(m)) stop("no hypothesized mediator variable supplied")
  else if (!is.character(m)) {
    stop("independent variable must be specified as a character vector")
  }
  p_m <- length(m)
  if (p_m == 0L) stop("at least one hypothesized mediator variable required")
  # check control variables
  if (!is.null(covariates) && !is.character(covariates)) {
    stop("control varialbes must be specified as a character vector")
  }
  p_covariates <- length(covariates)
  # control variables will be indicated to be of different relevance
  types <- c("main", "control")
  # combine independent variables and control variables
  # (there is no difference in terms of the model)
  predictors <- c(x, covariates)
  p_predictors <- length(predictors)
  types_predictors <- rep.int(types, times = c(p_x, p_covariates))
  # in case of multiple mediators, check for parallel or serial model
  if (p_m == 1L) model <- if (p_predictors == 1L) "simple" else "multiple"
  else {
    model <- match.arg(model)
    if (model == "serial") {
      if (p_m > 3L) {
        stop("serial multiple mediator model not implemented for ",
             "more than 3 hypothesized mediators")
      }
    }
  }

  # define useful sequences
  seq_predictors <- seq_len(p_predictors)
  seq_m <- seq_len(p_m)

  # define quantities for default spacing
  if (model == "serial") {
    if (p_m == 2L) {
      # two serial mediators
      h_space <- list(m = c(2, 4), y = 2)
      v_space <- list(x = 1, m = height + 2)
    } else {
      # three serial mediators
      h_space <- list(m = c(1.5, 3.5, 3.5), y = 1.5)
      v_space <- list(x = 1, m = height * c(1, 2, 1) + 4)
    }
  } else {
    h_space <- list(m = 4, y = 4)
    v_space <- list(x = 1, m = if (model == "simple") height + 2 else 1)
  }

  # define centers of boxes for explanatory variables
  center_predictors <- cbind(x = 0,
                             y = -(height + v_space$x) * (seq_predictors - 1))
  # define centers of boxes for mediators
  if (model == "serial") {
    # currently only implemented for two or three hypothesized mediators
    center_m <- cbind(x = width * seq_m + cumsum(h_space$m),
                      y = height + v_space$m)
  } else {
    # all other mediation models
    center_m <- cbind(x = width + h_space$m,
                      y = (height + v_space$m) * rev(seq_m))
  }
  # define centers of boxes for dependent variable
  center_y <- cbind(x = max(center_m[, "x"]) + width + h_space$y,
                    y = 0)

  # construct data frame for boxes
  df_boxes <- rbind(
    # for explanatory variables
    data.frame(x = center_predictors[, "x"],
               y = center_predictors[, "y"],
               Label = predictors,
               Type = types_predictors,
               stringsAsFactors = FALSE),
    # for mediators
    data.frame(x = center_m[, "x"],
               y = center_m[, "y"],
               Label = m,
               Type = types[1L],
               stringsAsFactors = FALSE),
    # for dependent variable
    data.frame(x = center_y[, "x"],
               y = center_y[, "y"],
               Label = y,
               Type = types[1L],
               stringsAsFactors = FALSE)
  )
  # add information on box size, as well as min and max coordinates
  df_boxes <- cbind(df_boxes,
                    width = width,
                    height = height,
                    xmin = df_boxes$x - width/2,
                    xmax = df_boxes$x + width/2,
                    ymin = df_boxes$y - height/2,
                    ymax = df_boxes$y + height/2)

  # store indices in data frame for boxes
  which_m <- p_predictors + seq_m
  which_y <- p_predictors + p_m + 1L

  # define step sizes for connections
  step_predictors <- height / (p_m + 2)        # outbound
  step_y <- height / (p_predictors + p_m + 1)  # inbound
  # construct data frames for arrows
  if (model == "serial") {
    # define step sizes for connections
    step_m_in <- height / c(p_predictors + 1, p_x + seq_m[-1])      # inbound
    step_m_out <- height / (rev(seq_m) + 1)  # outbound
    # connect independent variables to left edge,
    # but control variables to bottom edge
    h_offset <- pmax(seq_predictors - p_x - 1, 0)
    v_offset <- pmax(rev(seq_predictors) - p_covariates, 0)
    # construct data frame for arrows from explanatory variables to mediators
    df_arrows <- lapply(seq_m, function(i) {
      data.frame(x = df_boxes[seq_predictors, "xmax"],
                 y = df_boxes[seq_predictors, "ymax"] - step_predictors * i,
                 xend = df_boxes[which_m[i], "xmin"] + step_m_in[i] * h_offset,
                 yend = df_boxes[which_m[i], "ymin"] + step_m_in[i] * v_offset,
                 Type = types_predictors,
                 stringsAsFactors = FALSE)
    })
    df_arrows <- do.call(rbind, df_arrows)
    # add arrows between mediators
    if (p_m == 2L) {
      # two serial mediators
      df_arrows <- rbind(
        df_arrows,
        data.frame(x = df_boxes[which_m[1L], "xmax"],
                   y = df_boxes[which_m[1L], "ymax"] - step_m_out[1L],
                   xend = df_boxes[which_m[2L], "xmin"],
                   yend = df_boxes[which_m[2L], "ymax"] - step_m_in[2L],
                   Type = types[1L],
                   stringsAsFactors = FALSE)
      )
    } else {
      # three serial mediators
      df_arrows <- rbind(
        df_arrows,
        # from first mediator to second and third
        data.frame(x = df_boxes[which_m[1L], "xmax"],
                   y = df_boxes[which_m[1L], "ymax"] - step_m_out[1L] * 1:2,
                   xend = df_boxes[which_m[-1L], "xmin"],
                   yend = df_boxes[which_m[-1L], "ymax"] - step_m_in[-1L] * 1:2,
                   Type = types[1L],
                   stringsAsFactors = FALSE),
        # from second mediator to third
        data.frame(x = df_boxes[which_m[2L], "xmax"],
                   y = df_boxes[which_m[2L], "ymax"] - step_m_out[2L],
                   xend = df_boxes[which_m[3L], "xmin"],
                   yend = df_boxes[which_m[3L], "ymax"] - step_m_in[3L],
                   Type = types[1L],
                   stringsAsFactors = FALSE)
      )
    }
    # add arrows from mediators to dependent variable
    df_arrows <- rbind(
      df_arrows,
      data.frame(x = df_boxes[which_m, "xmax"],
                 y = df_boxes[which_m, "ymin"] + step_m_out,
                 xend = df_boxes[which_y, "xmin"],
                 yend = df_boxes[which_y, "ymax"] - step_y * rev(seq_m),
                 Type = types[1L],
                 stringsAsFactors = FALSE)
    )
  } else {
    # define step sizes for connections
    step_m <- height / (p_predictors + 1)  # inbound
    # construct data frame for arrows from explanatory variables to mediators
    df_arrows <- lapply(seq_m, function(i) {
      data.frame(x = df_boxes[seq_predictors, "xmax"],
                 y = df_boxes[seq_predictors, "ymax"] - step_predictors * i,
                 xend = df_boxes[which_m[i], "xmin"],
                 yend = df_boxes[which_m[i], "ymin"] + step_m * rev(seq_predictors),
                 Type = types_predictors,
                 stringsAsFactors = FALSE)
    })
    df_arrows <- do.call(rbind, df_arrows)
    # add arrows from mediators to dependent variable
    df_arrows <- rbind(
      df_arrows,
      data.frame(x = df_boxes[which_m, "xmax"],
                 y = df_boxes[which_m, "y"],
                 xend = df_boxes[which_y, "xmin"],
                 yend = df_boxes[which_y, "ymax"] - step_y * seq_m,
                 Type = types[1L],
                 stringsAsFactors = FALSE)
    )
  }
  # add arrows from explanatory variables to dependent variable
  df_arrows <- rbind(
    df_arrows,
    data.frame(x = df_boxes[seq_predictors, "xmax"],
               y = df_boxes[seq_predictors, "ymin"] + step_predictors,
               xend = df_boxes[which_y, "xmin"],
               yend = df_boxes[which_y, "ymin"] + step_y * rev(seq_predictors),
               Type = types_predictors,
               stringsAsFactors = FALSE)
  )

  # convert variable 'type' to a factor to ensure correct color allocation
  df_boxes$Type <- factor(df_boxes$Type, levels = types)
  df_arrows$Type <- factor(df_arrows$Type, levels = types)

  # order arrows so that the ones corresponding to control variables are drawn
  # first (then they are in the background relative to the other arrows)
  df_arrows <- df_arrows[order(df_arrows$Type, decreasing = TRUE), ]

  # set default text size
  if (model == "serial") text_size <- if (p_m == 2L) 3.5 else 3
  else text_size <- 4

  # return object
  out <- list(boxes = df_boxes,
              arrows = df_arrows,
              colors = c("black", "#808080"),
              size = text_size,
              expand = expansion(add = 0.1))
  class(out) <- "setup_model_diagram"
  out

}

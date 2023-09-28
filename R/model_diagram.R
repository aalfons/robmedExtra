# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


#' @importFrom grid arrow
#' @importFrom ggplot2 coord_fixed expansion geom_rect geom_text geom_segment
#' ggplot labs scale_color_manual scale_x_continuous scale_y_continuous
#' theme_void
#' @export

# x, y, m, covariates ... character strings/vectors of variable names
# model ................. type of mediation model
#                         (only relevant if two or more mediators are supplied)
# width, height ......... integers giving the width/height of the boxes
#                         (the same for all boxes)
# center_x,
# center_y,
# center_m,               matrices with two columns giving the coordinates of
# center_covariates ..... the centers of the boxes for the different variables

model_diagram <- function(x, y, m, covariates = NULL,
                          model = c("parallel", "serial"),
                          width = 8, height = 2, center_x = NULL,
                          center_y = NULL, center_m = NULL,
                          center_covariates = NULL) {

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
  types <- c("primary", "secondary")
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
  seq_x <- seq_len(p_x)
  seq_covariates <- seq_len(p_covariates)
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

  # if not supplied, define centers of boxes for independent variables
  if (is.null(center_x)) {
    center_x <- cbind(x = 0,
                      y = -(height + v_space$x) * (seq_x - 1))
  } else colnames(center_x) <- c("x", "y")
  # if not supplied, define centers of boxes for control variables
  if (p_covariates == 0L) center_covariates <- NULL
  else if (is.null(center_covariates)) {
    center_covariates <- cbind(
      x = max(center_x[, "x"]),
      y = min(center_x[, "y"]) - (height + v_space$x) * seq_covariates
    )
  } else colnames(center_covariates) <- c("x", "y")
  # combine centers for explanatory variables
  center_predictors <- rbind(center_x, center_covariates)
  # if not supplied, define centers of boxes for mediators
  if (is.null(center_m)) {
    # obtain origin for boxes for mediators
    origin_m <- c(max(center_predictors[, "x"]),
                  max(center_predictors[, "y"]))
    # define center of boxes for mediators
    if (model == "serial") {
      # currently only implemented for two or three hypothesized mediators
      center_m <- cbind(x = origin_m[1L] + width * seq_m + cumsum(h_space$m),
                        y = origin_m[2L] + height + v_space$m)
    } else {
      # all other mediation models
      center_m <- cbind(x = origin_m[1L] + width + h_space$m,
                        y = origin_m[2L] + (height + v_space$m) * rev(seq_m))
    }
  } else colnames(center_m) <- c("x", "y")
  # if not supplied, define center of boxes for dependent variable
  if (is.null(center_y)) {
    center_y <- cbind(x = max(center_m[, "x"]) + width + h_space$y,
                      y = max(center_x[, "y"]))
  } else colnames(center_y) <- c("x", "y")

  # construct data frame for boxes
  df_boxes <- rbind(
    # for explanatory variables
    data.frame(x = center_predictors[, "x"],
               y = center_predictors[, "y"],
               label = predictors,
               type = types_predictors,
               stringsAsFactors = FALSE),
    # for mediators
    data.frame(x = center_m[, "x"],
               y = center_m[, "y"],
               label = m,
               type = types[1L],
               stringsAsFactors = FALSE),
    # for dependent variable
    data.frame(x = center_y[, "x"],
               y = center_y[, "y"],
               label = y,
               type = types[1L],
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
                 type = types_predictors,
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
                   type = types[1L],
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
                   type = types[1L],
                   stringsAsFactors = FALSE),
        # from second mediator to third
        data.frame(x = df_boxes[which_m[2L], "xmax"],
                   y = df_boxes[which_m[2L], "ymax"] - step_m_out[2L],
                   xend = df_boxes[which_m[3L], "xmin"],
                   yend = df_boxes[which_m[3L], "ymax"] - step_m_in[3L],
                   type = types[1L],
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
                 type = types[1L],
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
                 type = types_predictors,
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
                 type = types[1L],
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
               type = types_predictors,
               stringsAsFactors = FALSE)
  )

  # order arrows so that the ones corresponding to control variables are drawn
  # first (then they are in the background relative to the other arrows)
  df_arrows <- df_arrows[order(df_arrows$type, decreasing = TRUE), ]

  # define style of arrow heads
  arrow_head <- arrow(angle = 15, length = unit(8, "pt"),
                      ends = "last", type = "closed")
  if (model == "serial") text_size <- if (p_m == 2L) 3.5 else 3
  else text_size <- 4
  # generate plot
  ggplot() +
    geom_segment(mapping = aes(x = x, y = y, xend = xend, yend = yend,
                               color = type),
                 data = df_arrows, linewidth = 0.5, arrow = arrow_head,
                 show.legend = FALSE) +
    geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                            color = type),
              data = df_boxes, fill = "white", linewidth = 0.5,
              show.legend = FALSE) +
    geom_text(mapping = aes(x = x, y = y, label = label, color = type),
              data = df_boxes, size = text_size, show.legend = FALSE) +
    coord_fixed() +
    scale_color_manual("", values = c("black", "#808080")) +
    scale_x_continuous(expand = expansion(add = 0.1)) +
    scale_y_continuous(expand = expansion(add = 0.1)) +
    theme_void()

}

# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


#' @importFrom grid arrow
#' @importFrom ggplot2 coord_fixed geom_rect geom_text geom_segment ggplot labs
#' scale_color_manual theme_void
#' @export

model_diagram <- function(x, y, m, covariates = NULL,
                          model = c("parallel", "serial"),
                          width = 8, height = 2,
                          spacing = c(4, 1)) {

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
  types_x <- rep.int(types, c(p_x, p_covariates))
  x <- c(x, covariates)
  p_x <- length(x)
  # in case of multiple mediators, check for parallel or serial model
  if (p_m == 1L) model <- if (p_x == 1L) "simple" else "multiple"
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
  seq_m <- seq_len(p_m)

  # define center of rectangles for independent variables and mediators
  if (model == "simple") {
    # simple mediation model
    center_x <- cbind(x = 0, y = 0)
    center_m <- cbind(x = width + spacing[1L], y = (height + spacing[2L]) * 2)
  } else if (model == "serial") {
    # serial multiple mediator model
    # (possibly with multiple independent variables)

  } else {
    # parallel multiple mediator model
    # (possibly with multiple independent variables)
    center_x <- cbind(x = 0,
                      y = -(height + spacing[2L]) * (seq_x-1))
    center_m <- cbind(x = width + spacing[1L],
                      y = (height + spacing[2L]) * rev(seq_m))
  }
  # define center of rectangles for dependent variable
  center_y <- cbind(x = 2 * (width + spacing[1L]), y = 0)

  # construct data frame for rectangles
  df_boxes <- rbind(
    # for independent variables
    data.frame(x = center_x[, "x"],
               y = center_x[, "y"],
               label = x,
               type = types_x),
    # for mediators
    data.frame(x = center_m[, "x"],
               y = center_m[, "y"],
               label = m,
               type = types[1L]),
    # for dependent variable
    data.frame(x = center_y[, "x"],
               y = center_y[, "y"],
               label = y,
               type = types[1L])
  )
  # add information on box size, as well as min and max coordinates
  df_boxes <- cbind(df_boxes,
                    width = width,
                    height = height,
                    xmin = df_boxes$x - width/2,
                    xmax = df_boxes$x + width/2,
                    ymin = df_boxes$y - height/2,
                    ymax = df_boxes$y + height/2)

  # find indices in data frame
  which_x <- seq_x
  which_m <- p_x + seq_m
  which_y <- p_x + p_m + 1L
  # construct data frames for arrows
  if (model == "serial") {

  } else {
    # define step sizes for connections
    step_x <- height / (p_m + 2)        # outbound
    step_m <- height / (p_x + 1)        # inbound
    step_y <- height / (p_x + p_m + 1)  # inbound
    # construct data frame for arrows from independent variables to mediators
    df_arrows <- lapply(seq_m, function(i) {
      data.frame(x = df_boxes[which_x, "xmax"],
                 y = df_boxes[which_x, "ymax"] - step_x * i,
                 xend = df_boxes[which_m[i], "xmin"],
                 yend = df_boxes[which_m[i], "ymin"] + step_m * rev(seq_x),
                 type = types_x)
    })
    df_arrows <- do.call(rbind, df_arrows)
    # add arrows from independent variables or mediators to dependent variable
    df_arrows <- rbind(
      # from independent variables to mediators
      df_arrows,
      # from mediators to dependent variable
      data.frame(x = df_boxes[which_m, "xmax"],
                 y = df_boxes[which_m, "y"],
                 xend = df_boxes[which_y, "xmin"],
                 yend = df_boxes[which_y, "ymax"] - step_y * seq_m,
                 type = types[1L]),
      # from independent variable to dependent variable
      data.frame(x = df_boxes[which_x, "xmax"],
                 y = df_boxes[which_x, "ymin"] + step_x,
                 xend = df_boxes[which_y, "xmin"],
                 yend = df_boxes[which_y, "ymin"] + step_y * rev(seq_x),
                 type = types_x)
    )
  }

  # order arrows so that the ones corresponding to control variables are drawn
  # first (then they are in the background relative to the other arrows)
  df_arrows <- df_arrows[order(df_arrows$type, decreasing = TRUE), ]

  # define style of arrow heads
  arrow_head <- arrow(angle = 15, length = unit(8, "pt"),
                      ends = "last", type = "closed")
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
              data = df_boxes, show.legend = FALSE) +
    coord_fixed() +
    scale_color_manual("", values = c("black", "#808080")) +
    theme_void()

}

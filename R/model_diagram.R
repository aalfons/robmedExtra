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
                          width = 8, height = 2) {

  # initializations
  p_x <- length(x)
  p_m <- length(m)
  p_covariates <- length(covariates)
  model <- match.arg(model)
  have_serial <- model == "serial"
  # covariates will be indicated to be of different relevance
  types <- c("primary", "secondary")
  # combine independent variables and covariates
  # (there is no difference in terms of the model)
  types_x <- rep.int(types, c(p_x, p_covariates))
  x <- c(x, covariates)
  p_x <- length(x)

  # define center of rectangles for independent variables
  if (p_x > 1L && p_m > 1L) {
    # multiple independent variables, multiple mediators
    if (have_serial) {
      # serial multiple mediator model

    } else {
      # parallel multiple mediator model

    }
  } else if (p_m > 1L) {
    # one independent variable, multiple mediators
    if (have_serial) {
      # serial multiple mediator model

    } else {
      # parallel multiple mediator model

    }
  } else if (p_x > 1L) {
    # multiple independent variables, one mediator

  } else {
    # simple mediation model
    center_x <- cbind(x = 0, y = 0)
    center_m <- cbind(x = 10, y = 5)
    center_y <- cbind(x = 20, y = 0)
  }

  # construct data frame for rectangles
  df_boxes <- rbind(
    data.frame(x = center_x[, "x"],
               y = center_x[, "y"],
               label = x,
               type = types_x),
    data.frame(x = center_m[, "x"],
               y = center_m[, "y"],
               label = m,
               type = types[1L]),
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

  # define data frames for arrows
  if (p_x > 1L && p_m > 1L) {
    # multiple independent variables, multiple mediators
    if (have_serial) {
      # serial multiple mediator model

    } else {
      # parallel multiple mediator model

    }
  } else if (p_m > 1L) {
    # one independent variable, multiple mediators
    if (have_serial) {
      # serial multiple mediator model

    } else {
      # parallel multiple mediator model

    }
  } else if (p_x > 1L) {
    # multiple independent variables, one mediator

  } else {
    # find indices in data frame
    which_x <- df_boxes$label == x
    which_m <- df_boxes$label == m
    which_y <- df_boxes$label == y
    # simple mediation model
    df_arrows <- rbind(
      data.frame(x = df_boxes[which_x, "xmax"],
                 y = df_boxes[which_x, "ymax"],
                 xend = df_boxes[which_m, "xmin"],
                 yend = df_boxes[which_m, "ymin"],
                 type = types_x),
      data.frame(x = df_boxes[which_m, "xmax"],
                 y = df_boxes[which_m, "ymin"],
                 xend = df_boxes[which_y, "xmin"],
                 yend = df_boxes[which_y, "ymax"],
                 type = types[1L]),
      data.frame(x = df_boxes[which_x, "xmax"],
                 y = df_boxes[which_x, "y"],
                 xend = df_boxes[which_y, "xmin"],
                 yend = df_boxes[which_y, "y"],
                 type = types[1L])
    )
  }

  # generate plot
  arrow_head <- arrow(angle = 15, length = unit(10, "pt"),
                 ends = "last", type = "closed")
  ggplot() +
    geom_segment(mapping = aes(x = x, y = y, xend = xend, yend = yend,
                               color = type),
                 data = df_arrows, size = 0.5, arrow = arrow_head,
                 show.legend = FALSE) +
    geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                            color = type),
              data = df_boxes, fill = "white", size = 0.5,
              show.legend = FALSE) +
    geom_text(mapping = aes(x = x, y = y, label = label, color = type),
              data = df_boxes, show.legend = FALSE) +
    coord_fixed() +
    scale_color_manual("", values = c("black", "darkgrey")) +
    theme_void()

}

# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# Load required packages -----
# we only load packages for which we don't use the :: operator to call functions
# (to keep the namespace clean)
library("shiny")
library("robmed")


# Server-side logic for GUI -----

#' @import shiny
#' @importFrom ggplot2 geom_vline labs scale_color_manual scale_fill_manual
#' theme
#' @importFrom graphics legend
#' @importFrom robmed density_plot test_mediation
#' @importFrom stats rnorm

shinyServer(function(input, output, session) {

  ## Define relevant objects and reactive values -----

  # define colors
  colors_data <- c(regular = "#00BFC4",
                   outlier = "#F8766D")
  labels_data <- c(regular = "Regular point",
                   outlier = "Outlier")
  colors_methods <- c(ols_boot = "#F8766D",
                      winsorized_boot = "#F564E3",
                      median_boot = "#619CFF",
                      ROBMED = "#00BFC4")
  labels_methods <- c(ols_boot = "OLS bootstrap",
                      winsorized_boot = "Winsorized bootstrap",
                      median_boot = "Median bootstrap",
                      ROBMED = "ROBMED")

  # initialize reactive values to be used for clean and contaminated data
  values <- reactiveValues(epsilon = 0.02)


  ## Render outputs for the 'Data' tab -----

  # observer to update clean data
  observe({
    # initializations
    a <- input$a
    b <- input$b
    c <- input$c
    n_observations <- input$n_observations
    seed <- input$seed
    # generate independent variable and error terms
    if (isTruthy(seed)) set.seed(seed)
    x <- rnorm(n_observations)
    e_m <- rnorm(n_observations)
    e_y <- rnorm(n_observations)
    # generate mediator and dependent variable
    m <- a * x + e_m
    y <- b * m + c * x + e_y
    # update reactive values
    values$clean_data <- data.frame(X = x, M = m, Y = y)
    values$sigma <- c(X = 1, M = sqrt(a^2 + 1),
                      Y = sqrt(b^2 * (a^2 + 1) + c^2 + 2*a*b*c + 1))
  })

  # show header for scatterplot matrix of clean data
  output$header_plot_clean_data <- renderUI({
    req(values$clean_data)
    h3("Simulated data")
  })

  # show scatterplot matrix of clean data
  output$plot_clean_data <- renderPlot({
    req(values$clean_data)
    plot(values$clean_data, pch = 16, cex = 2,
         col = colors_data["regular"],
         cex.axis = 1.5, cex.labels = 2, las = 1,
         oma = c(2.5, 2.5, 2.5, 12))
  })


  ## Render outputs for the 'Outliers' tab -----

  # create UI input for number of observations
  output$select_n_outliers <- renderUI({
    # adjust selected and maximum value according to number of observations
    selected_value <- ceiling(isolate(values$epsilon) * input$n_observations)
    max_value <- ceiling(0.1 * input$n_observations)
    # create UI input
    sliderInput("n_outliers", "Number of outliers", min = 0, max = max_value,
                value = selected_value, step = 1)
  })

  # observer to update contamination level
  observeEvent(input$n_outliers, {
    values$epsilon <- input$n_outliers / input$n_observations
  })

  # observer to update contaminated data
  observe({
    req(input$n_outliers)
    # initializations
    contaminated_data <- values$clean_data
    if (input$n_outliers == 0) {
      # add variable indicating that all observations are regular
      contaminated_data$Type <- rep.int("regular", input$n_observations)
    } else {
      # select observations to scale and shift
      replace <- seq_len(input$n_outliers)
      # combine scaling factors and additive shifts
      multiplier <- c(X = input$scaling_X,
                      M = input$scaling_M,
                      Y = input$scaling_Y)
      shift <- c(X = input$shift_X,
                 M = input$shift_M,
                 Y = input$shift_Y)
      # scale and shift observations
      contaminated_data[replace, ] <- mapply(function(v, m, s) m * v + s,
                                             v = contaminated_data[replace, ],
                                             m = multiplier,
                                             s = shift * values$sigma,
                                             SIMPLIFY = FALSE)
      # add variable indicating regular observations and outliers
      n_regular <- input$n_observations - input$n_outliers
      contaminated_data$Type <- rep.int(c("outlier", "regular"),
                                        c(input$n_outliers, n_regular))
    }
    # update reactive value
    values$contaminated_data <- contaminated_data
  })

  # show header for scatterplot matrix of contaminated data
  output$header_plot_contaminated_data <- renderUI({
    req(values$contaminated_data)
    h3("Simulated data with outliers")
  })

  # show scatterplot matrix of contaminated data
  output$plot_contaminated_data <- renderPlot({
    req(values$contaminated_data)
    plot(values$contaminated_data[, c("X", "M", "Y")], pch = 16, cex = 2,
         col = colors_data[values$contaminated_data$Type], cex.axis = 1.5,
         cex.labels = 2, las = 1, oma = c(2.5, 2.5, 2.5, 12))
    legend("right", inset = c(-0.03, 0), legend = labels_data, pch = 16,
           pt.cex = 1.4, col = colors_data, cex = 1.1, y.intersp = 0.75,
           bty = "n", xpd = TRUE)
  })


  ## Render outputs for the 'Methods' tab -----

  # show header for scatterplot matrix of contaminated data
  output$header_density_plot <- renderUI({
    req(isTruthy(values$clean_data) || isTruthy(values$contaminated_data),
        input$methods)
    h3("Bootstrap distribution")
  })

  # show density plot for selected methods
  output$density_plot <- renderPlot({
    # initializations
    simulated_data <- values$contaminated_data
    if (!isTruthy(simulated_data)) simulated_data <- values$clean_data
    req(simulated_data, input$methods)
    # check which methods are selected
    have_ols_boot <- "ols_boot" %in% input$methods
    have_winsorized_boot <- "winsorized_boot" %in% input$methods
    have_median_boot <- "median_boot" %in% input$methods
    have_ROBMED <- "ROBMED" %in% input$methods
    # With a smaller number of bootstrap replicates, there may be a warning
    # that extreme order statistics are used as endpoints of confidence
    # intervals.  Since this shiny app is just illustrative, such warnings
    # are ignored.
    suppressWarnings({
      if (have_ols_boot) {
        ols_boot <- test_mediation(simulated_data,
                                   x = "X", y = "Y", m = "M",
                                   test = "boot", R = input$R,
                                   method = "regression",
                                   robust = FALSE)
      } else ols_boot <- NULL
      if (have_winsorized_boot) {
        winsorized_boot <- test_mediation(simulated_data,
                                          x = "X", y = "Y", m = "M",
                                          test = "boot", R = input$R,
                                          method = "covariance",
                                          robust = TRUE)
      } else winsorized_boot <- NULL
      if (have_median_boot) {
        median_boot <- test_mediation(simulated_data,
                                      x = "X", y = "Y", m = "M",
                                      test = "boot", R = input$R,
                                      method = "regression",
                                      robust = "median")
      } else median_boot <- NULL
      if (have_ROBMED) {
        robust_boot <- test_mediation(simulated_data,
                                      x = "X", y = "Y", m = "M",
                                      test = "boot", R = input$R,
                                      method = "regression",
                                      robust = "MM")
      } else robust_boot <- NULL
    })
    # create object containing selected methods
    boot_list <- list(ols_boot, winsorized_boot, median_boot, robust_boot)
    names(boot_list) <- labels_methods
    # select colors
    selected_colors <- colors_methods[input$methods]
    names(selected_colors) <- labels_methods[input$methods]
    # plot the density of the bootstrap distribution
    density_plot(boot_list) +
      geom_vline(xintercept = input$a * input$b) +
      scale_color_manual(values = selected_colors) +
      scale_fill_manual(values = selected_colors) +
      labs(title = NULL) +
      theme(title = element_text(size = 15),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
  })

})

# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Server-side logic for GUI -----

#' @import shiny
#' @importFrom DT renderDataTable

shinyServer(function(input, output, session) {

  ## Define relevant objects and reactive expressions -----

  # create separate environment for safely loading RData files
  RData_env <- new.env()

  # define reactive expression to get selected data frame
  get_data <- reactive({
    # get data frame (empty if nothing is selected yet)
    if (is.null(input$df_name)) df <- data.frame()
    else {
      env <- if (input$data_source == "RData file") RData_env else .GlobalEnv
      df <- as.data.frame(get(input$df_name, envir = env))
    }
    # make sure that column names are unique
    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  # define reactive expression to get variable names
  get_variables <- reactive({
    df <- get_data()
    names(df)
  })

  # define reactive expression to get names of numeric variables
  get_numeric_variables <- reactive({
    df <- get_data()
    variables <- names(df)
    if (ncol(df) > 0L) {
      is_numeric <- sapply(df, is.numeric)
      variables <- variables[is_numeric]
    }
    variables
  })


  ## Render inputs for 'Data' tab -----

  # create UI input for selecting an RData file
  output$select_Rdata_file <- renderUI({
    if (input$data_source == "RData file") {
      fileInput("RData_file", "RData file", multiple = FALSE, accept = ".RData")
    }
  })

  # create UI input for selecting the data frame
  output$select_data_frame <- renderUI({
    # determine whether the UI input should list data frames from the R
    # environment, or from an RData file selected by the user
    if (input$data_source == "RData file") {
      # load selected RData file into separate environment
      req(input$RData_file)
      load(input$RData_file$datapath, envir = RData_env)
      # set environment and suffix for help text if there are no data frames
      env <- RData_env
      suffix <- "the selected RData file."
    } else {
      # set environment and suffix for help text if there are no data frames
      env <- .GlobalEnv
      suffix <- "your R environment."
    }
    # create UI input for selecting the data frame from the available list
    is_df <- sapply(env, is.data.frame, simplify = TRUE, USE.NAMES = TRUE)
    if (!any(is_df)) {
      # no data frame in the R environment or RData file
      helpText("There are no data frames in", suffix)
    } else {
      # at least one data frame in the R environment or RData file
      df_names <- names(is_df)[is_df]
      selectInput("df_name", "Data frame", choices = df_names, multiple = FALSE)
    }
  })


  ## Render outputs for 'Data' tab -----

  # show data frame in main panel
  output$data_table <-  DT::renderDataTable(get_data())


  ## Update inputs for 'Model' tab -----

  # observer to update variables that can be selected as response variable
  observe({
    remove <- c(input$explanatory, input$mediators, input$covariates)
    updateSelectInput(session, inputId = "response",
                      choices = setdiff(get_numeric_variables(), remove),
                      selected = isolate(input$response))
  })

  # observer to update variables that can be selected as explanatory variables
  observe({
    remove <- c(input$response, input$mediators, input$covariates)
    updateSelectInput(session, inputId = "explanatory",
                      choices = setdiff(get_variables(), remove),
                      selected = isolate(input$explanatory))
  })

  # observer to update variables that can be selected as mediators variables
  observe({
    remove <- c(input$response, input$explanatory, input$covariates)
    updateSelectInput(session, inputId = "mediators",
                      choices = setdiff(get_numeric_variables(), remove),
                      selected = isolate(input$mediators))
  })

  # observer to update variables that can be selected as control variables
  observe({
    remove <- c(input$response, input$explanatory, input$mediators)
    updateSelectInput(session, inputId = "covariates",
                      choices = setdiff(get_variables(), remove),
                      selected = isolate(input$covariates))
  })


  ## Render outputs for 'Model' tab -----
  output$test_y <- renderPrint(input$response)
  output$test_x <- renderPrint(input$explanatory)
  output$test_m <- renderPrint(input$mediators)
  output$test_covariates <- renderPrint(input$covariates)

})

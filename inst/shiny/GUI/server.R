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

  # create a separate environment for safely loading RData files
  RData_env <- new.env()

  # function to get the names of data frames in a given environment
  get_data_frames <- function(env = .GlobalEnv) {
    is_df <- sapply(env, is.data.frame, simplify = TRUE, USE.NAMES = TRUE)
    if (any(is_df)) names(is_df)[is_df]
    else character()
  }

  # check if there are any data frames in global environment
  df_global <- get_data_frames()
  n_df_global <- length(df_global)

  # reactive expression to get the selected data source
  get_data_source <- reactive({
    data_source <- input$data_source
    if (is.null(data_source)) data_source <- "RData file"
    data_source
  })

  # reactive expression to get the name of the selected data frame
  get_df_name <- reactive({
    if (get_data_source() == "RData file") input$df_name_RData
    else input$df_name_global
  })

  # reactive expression to get the environment of the selected data source
  get_env <- reactive({
    # this function is typically called with input$data_source as argument:
    # if there are no data frames in the global environment, this input is NULL
    if (get_data_source() == "RData file") RData_env
    else .GlobalEnv
  })

  # reactive expression to get the selected data frame
  get_data <- reactive({
    # get data frame (empty if nothing is selected yet)
    df_name <- get_df_name()
    if (is.null(df_name) || df_name == "") df <- data.frame()
    else df <- as.data.frame(get(df_name, envir = get_env()))
    # make sure that column names are unique
    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  # function to get the variable names of a data set
  # (by default the selected data frame)
  get_variables <- function(data = get_data()) names(data)

  # function to get the names of numeric variables of a data set
  # (by default the selected data frame)
  get_numeric_variables <- function(data = get_data()) {
    variables <- names(data)
    if (ncol(data) > 0L) {
      is_numeric <- sapply(data, is.numeric)
      variables <- variables[is_numeric]
    }
    variables
  }


  ## Render inputs for 'Data' tab -----

  # create UI input for selecting the data source
  # (global environment or RData file)
  output$select_data_source <- renderUI({
    # determine whether to show the UI input
    if (n_df_global == 0L) {
      # no data frames in the global environment
      helpText(#"There are no data frames in your R environment.",
               "If you have a data set that is not in RData format, you can",
               "import it into your R session before (re)starting the GUI.")
    } else {
      # show the UI input if there are data frames in the global environment
      selectInput("data_source", "Data source",
                  choices = c("R environment", "RData file"),
                  selected = "R environment", multiple = FALSE)
    }
  })

  # create UI input for selecting an RData file
  output$select_Rdata_file <- renderUI({
    # determine whether to show the UI input
    if (get_data_source() == "RData file") {
      fileInput("RData_file", "RData file", multiple = FALSE, accept = ".RData")
    }
  })

  # create UI input for selecting a data frame from the global environment
  output$select_df_global <- renderUI({
    # determine whether to show the UI input
    if (get_data_source() == "R environment") {
      # there is at least one data frame in the global environment
      if (n_df_global == 1L) {
        # if there is only one data set in the global environment, it is
        # selected automatically
        selectInput("df_name_global", "Data frame", choices = df_global,
                    selected = NULL, multiple = FALSE)
      } else {
        # if there are multiple data sets in the global environment, by default
        # the previously selected data frame is selected again (if it exists)
        previous <- isolate(input$df_name_global)
        selectInput("df_name_global", "Data frame", choices = c("", df_global),
                    selected = previous, multiple = FALSE)
      }
    }
  })

  # create UI input for selecting a data frame from the selected RData file
  output$select_df_RData <- renderUI({
    # determine whether to show the UI input
    if (get_data_source() == "RData file") {
      # make sure that an RData file is selected
      req(input$RData_file)
      # load the selected RData file into the separate environment
      # (make sure the environment is empty first)
      rm(list = ls(envir = RData_env, all.names = TRUE), envir = RData_env)
      load(input$RData_file$datapath, envir = RData_env)
      # get data frames in the selected RData file
      df_RData <- get_data_frames(RData_env)
      n_df_RData <- length(df_RData)
      # create UI input
      if (n_df_RData == 0L) {
        # let user know if there are no data frames in the selected RData file
        helpText("There are no data frames in the selected RData file.")
      } else if (n_df_RData == 1L) {
        # if there is only one data set in the selected RData file, it is
        # selected automatically
        selectInput("df_name_RData", "Data frame", choices = df_RData,
                    selected = NULL, multiple = FALSE)
      } else {
        # if there are multiple data sets in the selected RData file, there
        # is no default selection
        # TODO: Is there a way to check what change triggered the evaluation
        #       of this expression? If the user changes the data source back
        #       to "RData file", it would be nice to by select the previously
        #       selected data frame by default.  But if the user selects a new
        #       RData file, there should not be a default selection.
        selectInput("df_name_RData", "Data frame", choices = c("", df_RData),
                    selected = NULL, multiple = FALSE)
      }
    }
  })


  ## Render outputs for 'Data' tab -----

  # # for testing whether inputs are handled correctly
  # output$test_data_source <- renderPrint(get_data_source())
  # output$test_RData_file <- renderPrint(input$RData_file$name)
  # output$test_df_name <- renderPrint(get_df_name())

  # show data frame in main panel
  output$data_table <-  DT::renderDataTable(get_data())


  ## Update inputs for 'Model' tab -----

  # create UI element to show help text
  output$help_data <- renderUI({
    df_name <- get_df_name()
    if (is.null(df_name) || df_name == "") {
      # if applicable, show help text that data frame needs to be selected
      helpText("Select a data frame in the", em("Data"), "tab.")
    }
  })

  # observer to reset selected variables when data set is selected
  observe({
    # get data and variable names
    data <- get_data()
    variables <- get_variables(data)
    numeric_variables <- get_numeric_variables(data)
    # update UI inputs for selecting variables
    updateSelectInput(session, inputId = "y",
                      choices = c("", numeric_variables),
                      selected = NULL)
    updateSelectInput(session, inputId = "x",
                      choices = variables, selected = NULL)
    updateSelectInput(session, inputId = "m",
                      choices = numeric_variables, selected = NULL)
    updateSelectInput(session, inputId = "covariates",
                      choices = variables, selected = NULL)
  })

  # observer to update variables that can be selected as response variable
  observe({
    numeric_variables <- isolate(get_numeric_variables())
    remove <- c(input$x, input$m, input$covariates)
    updateSelectInput(session, inputId = "y",
                      choices = setdiff(numeric_variables, remove),
                      selected = isolate(input$y))
  })

  # observer to update variables that can be selected as explanatory variables
  observe({
    variables <- isolate(get_variables())
    remove <- c(input$y, input$m, input$covariates)
    updateSelectInput(session, inputId = "x",
                      choices = setdiff(variables, remove),
                      selected = isolate(input$x))
  })

  # observer to update variables that can be selected as mediators variables
  observe({
    numeric_variables <- isolate(get_numeric_variables())
    remove <- c(input$y, input$x, input$covariates)
    updateSelectInput(session, inputId = "m",
                      choices = setdiff(numeric_variables, remove),
                      selected = isolate(input$m))
  })

  # observer to update variables that can be selected as control variables
  observe({
    variables <- isolate(get_variables())
    remove <- c(input$y, input$x, input$m)
    updateSelectInput(session, inputId = "covariates",
                      choices = setdiff(variables, remove),
                      selected = isolate(input$covariates))
  })

  # create UI input for selecting the type of multiple mediator model
  output$select_model <- renderUI({
    if (length(input$m) > 1L) {
      selectInput("model", "Multiple mediator model:",
                  choices = c('parallel', 'serial'),
                  selected = isolate(input$model))
    }
  })


  ## Render outputs for 'Model' tab -----

  # # for testing whether inputs are handled correctly
  # output$test_y <- renderPrint(input$y)
  # output$test_x <- renderPrint(input$x)
  # output$test_m <- renderPrint(input$m)
  # output$test_covariates <- renderPrint(input$covariates)
  # output$test_model <- renderPrint(input$model)


  ## update inputs for 'ROBMED' tab

  # create UI button to perform ROBMED
  output$button_ROBMED <- renderUI({
    if (isTruthy(input$y) && isTruthy(input$x) && isTruthy(input$m)) {
      # if the necessary variables are selected, show a button to perform ROBMED
      actionButton("run_ROBMED", "Run")
    } else {
      # otherwise show help text that variables need to be selected
      helpText("Select a dependent variable, at least one independent",
               "variable, and at least one mediator in the", em("Model"),
               "tab.")
    }
  })

})

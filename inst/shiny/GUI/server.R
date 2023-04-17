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

  # create a separate environment for safely conducting analyses
  session_env <- new.env()

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

  # initialize reactive values
  values <- reactiveValues()

  # reactive expression to get the selected data source
  get_data_source <- reactive({
    # if there are no data frames in the global environment, this input is NULL
    # and the user can only select an RData file
    data_source <- input$data_source
    if (is.null(data_source)) data_source <- "RData file"
    data_source
  })

  # # reactive expression to get the name of the selected data frame
  # get_df_name <- reactive({
  #   if (get_data_source() == "RData file") input$df_name_RData
  #   else input$df_name_global
  # })
  #
  # # reactive expression to get the environment of the selected data source
  # get_env <- reactive({
  #   # this function is typically called with input$data_source as argument:
  #   # if there are no data frames in the global environment, this input is NULL
  #   if (get_data_source() == "RData file") RData_env
  #   else .GlobalEnv
  # })
  #
  # # reactive expression to get the selected data frame
  # get_data <- reactive({
  #   # get data frame (empty if nothing is selected yet)
  #   df_name <- get_df_name()
  #   if (is.null(df_name) || df_name == "") df <- data.frame()
  #   else df <- as.data.frame(get(df_name, envir = get_env()))
  #   # make sure that column names are unique
  #   names(df) <- make.names(names(df), unique = TRUE)
  #   df
  # })
  #
  # # function to get the variable names of a data set
  # # (by default the selected data frame)
  # get_variables <- function(data = get_data()) names(data)
  #
  # # function to get the names of numeric variables of a data set
  # # (by default the selected data frame)
  # get_numeric_variables <- function(data = get_data()) {
  #   variables <- names(data)
  #   if (ncol(data) > 0L) {
  #     is_numeric <- sapply(data, is.numeric)
  #     variables <- variables[is_numeric]
  #   }
  #   variables
  # }


  ## Render inputs for the 'Data' tab -----

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
        selectInput("df_name_global", "Data frame", choices = c("", df_global),
                    selected = isolate(input$df_name_global), multiple = FALSE)
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

  # observer to update data in separate environment for session
  observe({
    # obtain name of data frame and R environment corresponding to data source
    if (get_data_source() == "RData file") {
      df_name <- input$df_name_RData
      source_env <- RData_env
    } else {
      df_name <- input$df_name_global
      source_env <- .GlobalEnv
    }
    # make sure that the separate environment is empty
    rm(list = ls(envir = session_env, all.names = TRUE), envir = session_env)
    # check if a data frame is selected
    if (is.null(df_name) || df_name == "") {
      # reset relevant reactive values
      values$df_name <- NULL
      values$variables <- character()
      values$numeric_variables <- character()
    } else {
      # obtain data frame for the source environment
      df <- get(df_name, envir = source_env)
      # make sure that column names are unique
      names(df) <- make.names(names(df), unique = TRUE)
      # add the selected data frame to the separate environment
      assign(df_name, df, envir = session_env)
      # obtain variable names
      variables <- names(df)
      if (ncol(df) == 0L) numeric_variables <- variables
      else {
        is_numeric <- sapply(df, is.numeric)
        numeric_variables <- variables[is_numeric]
      }
      # update relevant reactive values
      values$df_name <- df_name
      values$variables <- variables
      values$numeric_variables <- numeric_variables
    }
  })


  ## Render outputs for the 'Data' tab -----

  # show data frame in main panel
  output$data_table <- DT::renderDataTable({
    df_name <- values$df_name
    if (is.null(df_name)) data.frame()
    else get(df_name, envir = session_env)
  })


  ## Update inputs for the 'Model' tab -----

  # create UI element to show help text
  output$help_data <- renderUI({
    if (is.null(values$df_name)) {
      # if applicable, show help text that data frame needs to be selected
      helpText("Select a data frame in the", em("Data"), "tab.")
    }
  })

  # observer to reset selected variables when data set is selected
  observe({
    # get variable names
    variables <- values$variables
    numeric_variables <- values$numeric_variables
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
    numeric_variables <- isolate(values$numeric_variables)
    remove <- c(input$x, input$m, input$covariates)
    updateSelectInput(session, inputId = "y",
                      choices = setdiff(numeric_variables, remove),
                      selected = isolate(input$y))
  })

  # observer to update variables that can be selected as explanatory variables
  observe({
    variables <- isolate(values$variables)
    remove <- c(input$y, input$m, input$covariates)
    updateSelectInput(session, inputId = "x",
                      choices = setdiff(variables, remove),
                      selected = isolate(input$x))
  })

  # observer to update variables that can be selected as mediators variables
  observe({
    numeric_variables <- isolate(values$numeric_variables)
    remove <- c(input$y, input$x, input$covariates)
    updateSelectInput(session, inputId = "m",
                      choices = setdiff(numeric_variables, remove),
                      selected = isolate(input$m))
  })

  # observer to update variables that can be selected as control variables
  observe({
    variables <- isolate(values$variables)
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


  ## update inputs for the 'ROBMED' tab

  # create UI button to perform ROBMED
  output$button_ROBMED <- renderUI({
    if (isTruthy(input$y) && isTruthy(input$x) && isTruthy(input$m)) {
      # if the necessary variables are selected, show the button
      actionButton("run_ROBMED", "Run")
    } else {
      # otherwise show help text that variables need to be selected
      helpText("Select a dependent variable, at least one independent",
               "variable, and at least one mediator in the", em("Model"),
               "tab.")
    }
  })

  # observer to ensure that confidence level is the same as for OLS bootstrap
  observeEvent(input$level_OLS_boot, {
    updateNumericInput(session, "level_ROBMED", value = input$level_OLS_boot)
  })

  # observer to ensure that number of bootstrap samples is the same as for
  # OLS bootstrap
  observeEvent(input$R_OLS_boot, {
    updateNumericInput(session, "R_ROBMED", value = input$R_OLS_boot)
  })

  # observer to ensure that seed of the random number generator is the same as
  # for OLS bootstrap
  observeEvent(input$seed_OLS_boot, {
    updateNumericInput(session, "seed_ROBMED", value = input$seed_OLS_boot)
  })

  # observer to ensure that version of the random number generator is the same
  # as for OLS bootstrap
  observeEvent(input$RNG_version_OLS_boot, {
    updateNumericInput(session, "RNG_version_ROBMED",
                       value = input$RNG_version_OLS_boot)
  })


  ## update inputs for the 'OLS Bootstrap' tab

  # create UI button to perform the OLS Bootstrap
  output$button_OLS_boot <- renderUI({
    if (isTruthy(input$y) && isTruthy(input$x) && isTruthy(input$m)) {
      # if the necessary variables are selected, show the button
      actionButton("run_OLS_boot", "Run")
    } else {
      # otherwise show help text that variables need to be selected
      helpText("Select a dependent variable, at least one independent",
               "variable, and at least one mediator in the", em("Model"),
               "tab.")
    }
  })

  # observer to ensure that confidence level is the same as for ROBMED
  observeEvent(input$level_ROBMED, {
    updateNumericInput(session, "level_OLS_boot", value = input$level_ROBMED)
  })

  # observer to ensure that number of bootstrap samples is the same as for
  # ROBMED
  observeEvent(input$R_ROBMED, {
    updateNumericInput(session, "R_OLS_boot", value = input$R_ROBMED)
  })

  # observer to ensure that seed of the random number generator is the same as
  # for ROBMED
  observeEvent(input$seed_ROBMED, {
    updateNumericInput(session, "seed_OLS_boot", value = input$seed_ROBMED)
  })

  # observer to ensure that version of the random number generator is the same
  # as for ROBMED
  observeEvent(input$RNG_version_ROBMED, {
    updateNumericInput(session, "RNG_version_OLS_boot",
                       value = input$RNG_version_ROBMED)
  })

})

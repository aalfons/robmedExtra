# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


#' @import shiny
#' @importFrom DT renderDataTable

shinyServer(function(input, output, session) {

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

  # create UI input for selecting an RData file
  output$Rdata_file <- renderUI({
    if (input$data_source == "RData file") {
      fileInput("RData_file", "RData file", multiple = FALSE, accept = ".RData")
    }
  })

  # create UI input for selecting the data frame
  output$data_frame <- renderUI({
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

  output$data_table <-  DT::renderDataTable(get_data())


})

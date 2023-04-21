# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Load required packages -----
library("shiny")
library("DT")
library("flextable")
library("robmed")
library("robmedExtra")


# Server-side logic for GUI -----

#' @import shiny
#' @importFrom DT renderDataTable
#' @importFrom flextable htmltools_value
#' @import robmed

shinyServer(function(input, output, session) {

  ## Define relevant objects and reactive expressions -----

  # function to get the names of data frames in a given environment
  get_data_frames <- function(env = .GlobalEnv) {
    is_df <- sapply(env, is.data.frame, simplify = TRUE, USE.NAMES = TRUE)
    if (any(is_df)) names(is_df)[is_df]
    else character()
  }

  # check if there are any data frames in global environment
  df_global <- get_data_frames()
  n_df_global <- length(df_global)

  # create a separate environment for safely loading RData files
  RData_env <- new.env()

  # create a separate environment for safely conducting analyses
  session_env <- new.env()

  # initialize reactive values to be used for various things
  values <- reactiveValues()

  # initialize reactive values for commands
  RNG_kind <- RNGkind()
  commands <- reactiveValues(
    packages = list(robmed = call("library", "robmed"),
                    robmedExtra = call("library", "robmedExtra")),
    RNG = call("RNGkind", RNG_kind[1L], RNG_kind[2L], RNG_kind[3L])
  )

  # reactive expression to get the selected data source
  get_data_source <- reactive({
    # if there are no data frames in the global environment, this input is NULL
    # and the user can only select an RData file
    data_source <- input$data_source
    if (is.null(data_source)) data_source <- "RData file"
    data_source
  })


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
    if (isTruthy(df_name)) {
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
      commands$data <- call("load", paste(df_name, "RData", sep = "."))
      values$df_name <- df_name
      values$variables <- variables
      values$numeric_variables <- numeric_variables
    } else {
      # reset relevant reactive values
      commands$data <- NULL
      values$df_name <- NULL
      values$variables <- character()
      values$numeric_variables <- character()
    }
    # clean up reactive values for commands and plot preview
    commands$ROBMED <- NULL
    commands$OLS_boot <- NULL
    commands$flextable <- NULL
    commands$plot <- NULL
    values$width <- NULL
    values$height <- NULL
  })


  ## Render outputs for the 'Data' tab -----

  # show data frame in main panel
  output$data_table <- DT::renderDataTable({
    req(values$df_name)
    get(values$df_name, envir = session_env)
  })


  ## Update inputs for the 'Model' tab -----

  # create UI element to show help text
  output$help_data <- renderUI({
    if (!isTruthy(values$df_name)) {
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
                      choices = variables,
                      selected = NULL)
    updateSelectInput(session, inputId = "m",
                      choices = numeric_variables,
                      selected = NULL)
    updateSelectInput(session, inputId = "covariates",
                      choices = variables,
                      selected = NULL)
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

  # observer to clean up reactive values when variables are selected
  observeEvent(c(input$y, input$x, input$m, input$covariates), {
    # clean up reactive values for commands
    commands$ROBMED <- NULL
    commands$OLS_boot <- NULL
    commands$flextable <- NULL
    commands$plot <- NULL
    # clean up reactive values for plot preview
    values$width <- NULL
    values$height <- NULL
  }, ignoreInit = TRUE)

  # create UI input for selecting the type of multiple mediator model
  output$select_model <- renderUI({
    if (length(input$m) > 1L) {
      selectInput("model", "Multiple mediator model:",
                  choices = c('parallel', 'serial'),
                  selected = isolate(input$model),
                  multiple = FALSE)
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

  # show help text if no random number seed is selected
  output$help_seed_ROBMED <- renderUI({
    if (!isTruthy(input$seed_ROBMED)) {
      helpText("The analysis is", strong("not reproducible"),
               "without setting a seed.")
    }
  })

  # show advanced options if selected
  output$MM_options <- renderUI({
    req(input$show_advanced_options)
    div(
      h2("MM-estimator"),
      selectInput("efficiency", "Efficiency at normal distribution",
                  choices = c(0.80, 0.85, 0.90, 0.95), selected = 0.85,
                  multiple = FALSE),
      numericInput("max_iterations", "Maximum number of iterations",
                   value = 10000, min = 1000, step = 1000)
    )
  })

  # observer to ensure that confidence level is the same as for OLS bootstrap
  observeEvent(input$level_OLS_boot, {
    updateNumericInput(session, inputId = "level_ROBMED",
                       value = input$level_OLS_boot)
  })

  # observer to ensure that number of bootstrap samples is the same as for
  # OLS bootstrap
  observeEvent(input$R_OLS_boot, {
    updateNumericInput(session, inputId = "R_ROBMED",
                       value = input$R_OLS_boot)
  })

  # observer to ensure that seed of the random number generator is the same as
  # for OLS bootstrap
  observeEvent(input$seed_OLS_boot, {
    updateNumericInput(session, inputId = "seed_ROBMED",
                       value = input$seed_OLS_boot)
  })

  # observer for button to run ROBMED
  observeEvent(input$run_ROBMED, {
    # construct command to set the seed of the random number generator
    if (isTruthy(input$seed_ROBMED)) {
      command_seed <- call("set.seed", input$seed_ROBMED)
      eval(command_seed, envir = session_env)
    } else command_seed <- NULL
    # construct command for control object for MM-estimator
    use_control <- isTruthy(input$efficiency) && isTruthy(input$max_iterations)
    if (use_control) {
      command_reg_control <- call("reg_control",
                                  efficiency = input$efficiency,
                                  max_iterations = input$max_iterations)
      command_ctrl <- call("<-", as.name("ctrl"), command_reg_control)
      eval(command_ctrl, envir = session_env)
    } else command_ctrl <- NULL
    # construct command to perform ROBMED
    command_test_mediation <- call("test_mediation",
                                   as.name(values$df_name),
                                   x = input$x,
                                   y = input$y,
                                   m = input$m)
    if (length(input$covariates) > 0L) {
      command_test_mediation$covariates <- input$covariates
    }
    command_test_mediation$R <- input$R_ROBMED
    command_test_mediation$level <- input$level_ROBMED
    command_test_mediation$robust <- TRUE
    if (length(input$m) > 1L) command_test_mediation$model = input$model
    if (use_control) command_test_mediation$control <- as.name("ctrl")
    command_robust_boot <- call("<-", as.name("robust_boot"),
                                command_test_mediation)
    eval(command_robust_boot, envir = session_env)
    # construct command to show summary
    command_summary <- call("summary", as.name("robust_boot"), plot = FALSE)
    # construct command to show diagnostic plot
    command_weight_plot <- call("weight_plot", as.name("robust_boot"))
    command_scale <- call("scale_color_manual", "",
                          values = c("black", "#00BFC4"))
    command_theme <- call("theme", legend.position = "top")
    command_plot <- call("+", call("+", command_weight_plot, command_scale),
                         command_theme)
    command_p <- call("<-", as.name("p"), command_plot)
    eval(command_p, envir = session_env)
    # update reactive value with list of commands to perform ROBMED
    commands_ROBMED <- list(seed = command_seed,
                            control = command_ctrl,
                            mediation = command_robust_boot,
                            summary = command_summary,
                            plot = command_p)
    attr(commands_ROBMED, "time_stamp") <- Sys.time()
    commands$ROBMED <- commands_ROBMED
    # clean up reactive values for relevant commands and plot preview
    commands$flextable <- NULL
    commands$plot <- NULL
    values$width <- NULL
    values$height <- NULL
  })


  ## Render outputs for the 'ROBMED' tab -----

  # show diagnostic plot for ROBMED in main panel
  output$plot_ROBMED_header <- renderUI({
    req(commands$ROBMED)
    h2("Diagnostic plot")
  })
  output$plot_ROBMED <- renderPlot({
    req(commands$ROBMED)
    get("p", envir = session_env)
  }, res = 100)

  # show summary for ROBMED in main panel
  output$summary_ROBMED_header <- renderUI({
    req(commands$ROBMED)
    h2("Model and test summaries")
  })
  output$summary_ROBMED <- renderPrint({
    req(commands$ROBMED)
    eval(commands$ROBMED$summary, envir = session_env)
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

  # show help text if no random number seed is selected
  output$help_seed_OLS_boot <- renderUI({
    if (!isTruthy(input$seed_OLS_boot)) {
      helpText("The analysis is", strong("not reproducible"),
               "without setting a seed.")
    }
  })

  # observer to ensure that confidence level is the same as for ROBMED
  observeEvent(input$level_ROBMED, {
    updateNumericInput(session, inputId = "level_OLS_boot",
                       value = input$level_ROBMED)
  })

  # observer to ensure that number of bootstrap samples is the same as for
  # ROBMED
  observeEvent(input$R_ROBMED, {
    updateNumericInput(session, inputId = "R_OLS_boot",
                       value = input$R_ROBMED)
  })

  # observer to ensure that seed of the random number generator is the same as
  # for ROBMED
  observeEvent(input$seed_ROBMED, {
    updateNumericInput(session, inputId = "seed_OLS_boot",
                       value = input$seed_ROBMED)
  })

  # observer for button to run the OLS bootstrap
  observeEvent(input$run_OLS_boot, {
    # construct command to set the seed of the random number generator
    if (isTruthy(input$seed_OLS_boot)) {
      command_seed <- call("set.seed", input$seed_OLS_boot)
      eval(command_seed, envir = session_env)
    } else command_seed <- NULL
    # construct command to perform the OLS bootstrap
    command_test_mediation <- call("test_mediation",
                                   as.name(values$df_name),
                                   x = input$x,
                                   y = input$y,
                                   m = input$m)
    if (length(input$covariates) > 0L) {
      command_test_mediation$covariates <- input$covariates
    }
    command_test_mediation$R <- input$R_OLS_boot
    command_test_mediation$level <- input$level_OLS_boot
    command_test_mediation$robust <- FALSE
    if (length(input$m) > 1L) command_test_mediation$model = input$model
    command_ols_boot <- call("<-", as.name("ols_boot"), command_test_mediation)
    eval(command_ols_boot, envir = session_env)
    # construct command to show summary
    command_summary <- call("summary", as.name("ols_boot"))
    # update reactive value with list of commands to perform the OLS bootstrap
    commands_OLS_boot <- list(seed = command_seed,
                              mediation = command_ols_boot,
                              summary = command_summary)
    attr(commands_OLS_boot, "time_stamp") <- Sys.time()
    commands$OLS_boot <- commands_OLS_boot
    # clean up reactive values for relevant commands
    commands$flextable <- NULL
  })


  ## Render outputs for the 'OLS Bootstrap' tab -----

  # show summary for the OLS bootstrap in main panel
  output$summary_OLS_boot_header <- renderUI({
    req(commands$OLS_boot)
    h2("Model and test summaries")
  })
  output$summary_OLS_boot <- renderPrint({
    req(commands$OLS_boot)
    eval(commands$OLS_boot$summary, envir = session_env)
  })


  ## update inputs for the 'Export' tab

  # create UI input for orientation of the table
  output$select_orientation <- renderUI({
    # show the input if both ROBMED and OLS bootstrap have been run
    req(commands$ROBMED, commands$OLS_boot)
    radioButtons("orientation", "Orientation",
                 choices = c("portrait", "landscape"),
                 selected = isolate(input$orientation))
  })

  # create UI button to preview the table
  output$button_table <- renderUI({
    if (isTruthy(commands$ROBMED) || isTruthy(commands$OLS_boot)) {
      # show the button if ROBMED or OLS bootstrap have been run
      actionButton("generate_table", "Generate")
    } else {
      # otherwise show help text that a method needs to be run
      helpText("Run ROBMED or the OLS bootstrap in the respective tabs.")
    }
  })

  # observer for button to preview the table
  observeEvent(input$generate_table, {
    # initializations
    have_ROBMED <- isTruthy(commands$ROBMED)
    have_OLS_boot <- isTruthy(commands$OLS_boot)
    # construct command to generate the flextable
    if (have_ROBMED && have_OLS_boot) {
      command_list <- call("list", as.name("robust_boot"), as.name("ols_boot"))
      command_to_flextable <- call("to_flextable", command_list,
                                   orientation = input$orientation,
                                   p_value = input$p_value,
                                   digits = input$digits)
    } else if (have_ROBMED) {
      command_to_flextable <- call("to_flextable", as.name("robust_boot"),
                                   p_value = input$p_value,
                                   digits = input$digits)
    } else if (have_OLS_boot) {
      command_to_flextable <- call("to_flextable", as.name("ols_boot"),
                                   p_value = input$p_value,
                                   digits = input$digits)
    }
    command_ft <- call("<-", as.name("ft"), command_to_flextable)
    eval(command_ft, envir = session_env)
    # construct command to export the flextable to Microsoft Word document
    command_export <- call("export_docx", as.name("ft"), file = "table.docx")
    # update reactive value with commands to generate and export the flextable
    commands_flextable <- list(generate = command_ft,
                               export = command_export)
    attr(commands_flextable, "time_stamp") <- Sys.time()
    commands$flextable <- commands_flextable
  })

  # create UI input for orientation of the table
  output$select_resolution <- renderUI({
    if ("png" %in% input$file_type) {
      numericInput("resolution", "Resolution (pixels per inch)",
                   value = 300, min = 0, step = 50)
    }
  })

  # create UI button to preview the diagnostic plot
  output$button_plot <- renderUI({
    if (isTruthy(commands$ROBMED)) {
      # ROBMED has been run
      if (isTruthy(input$file_type)) {
        # show button if at least one file type is selected
        actionButton("preview_plot", "Preview")
      } else {
        # otherwise show help text to select file type
        helpText("Select at least one file type.")
      }
    } else {
      # otherwise show help text that ROBMED needs to be run
      helpText("Run ROBMED in the respective tab.")
    }
  })

  # observer for switching the units for width and height
  observeEvent(input$units, {
    if (input$units == "inches") {
      width <- input$width / 2.54
      height <- input$height / 2.54
      step = 0.5
    } else {
      width <- input$width * 2.54
      height <- input$height * 2.54
      step = 1
    }
    updateNumericInput(session, inputId = "width", value = width, step = step)
    updateNumericInput(session, inputId = "height", value = height, step = step)
  }, ignoreInit = TRUE)

  # observer for button to preview the plot
  observeEvent(input$preview_plot, {
    # extract and convert some inputs
    width <- input$width
    height <- input$height
    units <- input$units
    if (units == "inches") units <- "in"
    else {
      # convert width and height to inches
      width <- width / 2.54
      height <- height / 2.54
    }
    # construct template for file name
    file_name <- "diagnostic_plot.%s"
    # construct command for opening the graphics device
    if ("pdf" %in% input$file_type) {
      # construct command
      command_pdf <- call("pdf", file = sprintf(file_name, "pdf"),
                          width = width, height = height)
    } else command_pdf <- NULL
    if ("png" %in% input$file_type) {
      # construct command
      command_png <- call("png", file = sprintf(file_name, "png"),
                          width = input$width, height = input$height,
                          units = units, res = input$resolution)
    } else command_png <- NULL
    # update reactive value with commands to create file containing plot
    commands_plot <- list(pdf = command_pdf,
                          png = command_png,
                          generate = call("print", as.name("p")),
                          close = call("dev.off"))
    attr(commands_plot, "time_stamp") <- Sys.time()
    commands$plot <- commands_plot
    # set reactive values for preview
    values$width <- width
    values$height <- height
  })

  # TODO: perhaps export button should first run the expressions for generating
  #       the table and the plot, create the replication script, and then save
  #       everything in one zip-archive (also including an RData file with the
  #       data set).


  ## Render outputs for the 'Export' tab -----

  # show preview of table in main panel
  output$table_preview_header <- renderUI({
    req(commands$flextable)
    h2("Table preview")
  })
  output$table_preview <- renderUI({
    req(commands$flextable)
    flextable::htmltools_value(get("ft", envir = session_env))
  })

  # show diagnostic plot for ROBMED in main panel
  output$plot_preview_header <- renderUI({
    req(input$file_type, values$width, values$height)
    div(
      h2("File preview for diagnostic plot"),
      helpText("The size shown here depends on the resolution of the browser",
               "and may differ from the size of the file to be generated.")
    )
  })
  output$plot_preview <- renderPlot({
    req(input$file_type)
    get("p", envir = session_env)
  }, width = function() {
    req(values$width)
    values$width * 125
  }, height = function() {
    req(values$height)
    values$height * 125
  }, res = 125)

})

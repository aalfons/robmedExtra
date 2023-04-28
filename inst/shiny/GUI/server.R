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


# Internal functions -----


# function to get the names of data frames in a given environment
get_data_frames <- function(env = .GlobalEnv) {
  is_df <- sapply(env, is.data.frame, simplify = TRUE, USE.NAMES = TRUE)
  if (any(is_df)) names(is_df)[is_df]
  else character()
}

# function to create a help text element
# This is defined to have more control over the style compared to the built-in
# function shiny::helpText(). In particular, we can make sure that the color is
# matched in function get_label() and that the the to and bottom margins are
# matched in function error_message().
help_text <- function(...) {
  # FIXME: this is hard-coded to be the same as CSS class "help-block" in
  #        bootstrap theme
  css <- "color: #737373; display: block; margin-top: 5px; margin-bottom: 10px;"
  span(style = css, ...)
}

# function to create an error text element
error_text <- function(...) {
  # FIXME: color is hard-coded to be the same as in error messages in
  #        bootstrap theme
  css <- "color: #a94442; display: block; margin-top: 5px; margin-bottom: 10px;"
  span(style = css, ...)
}

# function to create a warning text element
warning_text <- function(...) {
  # FIXME: color is hard-coded to be the same as background color in warning
  #        messages in bootstrap theme
  css <- "color: #ec971f; display: block; margin-top: 5px; margin-bottom: 10px;"
  span(style = css, ...)
}


# function to get the names of data frames in a given environment
get_data_frames <- function(env = .GlobalEnv) {
  is_df <- sapply(env, is.data.frame, simplify = TRUE, USE.NAMES = TRUE)
  if (any(is_df)) names(is_df)[is_df]
  else character()
}


# function to construct labels for variable selection inputs
get_label <- function(label, info) {
  # FIXME: color is hard-coded to be the same as in help text in bootstrap theme
  p(label, span(info, style = "color: #737373; font-weight:normal;"))
}


# generic function to show errors as (list of) text elements
show_errors <- function(errors) UseMethod("show_errors")

# the default method is intended for a character vector
show_errors.default <- function(errors) {
  # call error_text() with vector elements as different arguments
  if (length(errors) > 1L) do.call(error_text, as.list(errors))
  else error_text(errors)
}

# for a list of error messages, call the method for each list element
show_errors.list <- function(errors) {
  if (length(errors) == 1L) show_errors(errors[[1L]])
  else do.call(tagList, lapply(errors, show_errors))
}


# function to generate error message for missing variable selection
get_variable_selection_error <- function(have_y, have_x, have_m) {
  # define pieces of text for missing variable selection
  text_y <- if (!have_y) "a dependent variable"
  text_x <- if (!have_x) "at least one independent variable"
  text_m <- if (!have_m) "at least one mediator"
  # put text for missing variable selection together
  n_yx <- sum(have_y, have_x)
  if (n_yx == 2L) text_select <- text_m
  else {
    text_yx <- paste(c(text_y, text_x), collapse = ", ")
    sep <- if (n_yx == 1L) " and " else ", and"
    text_select <- paste(c(text_yx, text_m), collapse = sep)
  }
  # put everything into a nice message
  error <- paste("Select", text_select, "in the <em>Model</em> tab.")
  # mark error message as HTML
  HTML(error)
}

# # function to construct commands for flextable
# get_flextable_commands <- function(input, commands) {
#   # initializations
#   have_ROBMED <- isTruthy(commands$ROBMED)
#   have_OLS_boot <- isTruthy(commands$OLS_boot)
#   # construct command to generate the flextable
#   if (have_ROBMED && have_OLS_boot) {
#     command_list <- call("list", as.name("robust_boot"), as.name("ols_boot"))
#     command_to_flextable <- call("to_flextable", command_list,
#                                  orientation = input$orientation,
#                                  p_value = input$p_value,
#                                  digits = input$digits)
#   } else if (have_ROBMED) {
#     command_to_flextable <- call("to_flextable", as.name("robust_boot"),
#                                  p_value = input$p_value,
#                                  digits = input$digits)
#   } else if (have_OLS_boot) {
#     command_to_flextable <- call("to_flextable", as.name("ols_boot"),
#                                  p_value = input$p_value,
#                                  digits = input$digits)
#   }
#   command_ft <- call("<-", as.name("ft"), command_to_flextable)
#   # construct command to export the flextable to Microsoft Word document
#   command_export <- call("export_docx", as.name("ft"), file = "table.docx")
#   # return list of commands to generate and export the flextable
#   commands_flextable <- list(generate = command_ft,
#                              export = command_export)
#   attr(commands_flextable, "time_stamp") <- Sys.time()
#   commands_flextable
# }
#
#


# function to prepare export or preview of the table
# It returns a list with some of the following components:
# commands ... list of commands to generate the table
# values ..... list containing the options for the table
# errors ..... (list of) error messages to be displayed
prepare_table <- function(input, commands, values) {
  # initializations
  have_ROBMED <- isTruthy(commands$ROBMED) && isTruthy(values$ROBMED)
  have_OLS_boot <- isTruthy(commands$OLS_boot) && isTruthy(values$OLS_boot)
  have_both <- have_ROBMED && have_OLS_boot
  # check if we can generate the table
  if (!have_ROBMED && !have_OLS_boot) {
    # neither ROBMED nor the OLS bootstrap have been run
    commands_table <- NULL
    values_table <- NULL
    errors_table <- list(
      preview_table = "Run ROBMED or the OLS bootstrap in the respective tabs."
    )
  } else if (have_both && !identical(values$ROBMED, values$OLS_boot)) {
    # both ROBMED and the OLS bootstrap have been run but with different options
    commands_table <- NULL
    values_table <- NULL
    errors_table <- list(
      preview_table = c("ROBMED and the OLS bootstrap use different options.",
                        "Re-run the methods with the same options.")
    )
  } else {
    # construct command to generate the flextable
    if (have_both) {
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
    # construct command to export the flextable to Microsoft Word document
    command_export <- call("export_docx", as.name("ft"), file = "table.docx")
    # return list of commands to generate and export the flextable
    commands_table <- list(generate = command_ft,
                           export = command_export)
    attr(commands_table, "time_stamp") <- Sys.time()
    # construct list of options
    values_table <- list(digits = input$digits, p_value = input$p_value,
                         orientation = if (have_both) input$orientation)
    # indicate that there are no errors
    errors_table <- NULL
  }
  # return list
  list(commands = commands_table, values = values_table, errors = errors_table)
}


# # function to construct commands for diagnostic plot
# # (as well as values for plot dimensions)
# get_plot_commands <- function(input, commands) {
#   # extract and convert some inputs
#   width <- input$width
#   height <- input$height
#   units <- input$units
#   if (units == "inches") units <- "in"
#   else {
#     # convert width and height to inches
#     width <- width / 2.54
#     height <- height / 2.54
#   }
#   # construct template for file name
#   file_name <- "diagnostic_plot.%s"
#   # construct command for opening the graphics device
#   if ("pdf" %in% input$file_type) {
#     # construct command
#     command_pdf <- call("pdf", file = sprintf(file_name, "pdf"),
#                         width = width, height = height)
#   } else command_pdf <- NULL
#   if ("png" %in% input$file_type) {
#     # construct command
#     command_png <- call("png", file = sprintf(file_name, "png"),
#                         width = input$width, height = input$height,
#                         units = units, res = input$resolution)
#   } else command_png <- NULL
#   # construct list of commands to export the diagnostic plot
#   commands_plot <- list(pdf = command_pdf,
#                         png = command_png,
#                         generate = call("print", as.name("p")),
#                         close = call("dev.off"))
#   attr(commands_plot, "time_stamp") <- Sys.time()
#   # construct list with plot dimensions
#   values_plot <- list(width = width, height = height)
#   # return list with commands to export plot and values for plot dimensions
#   list(commands = commands_plot, values = values_plot)
# }


# function to prepare export or preview of the diagnostic plot
# (as specified by argument 'preview')
# It returns a list with some of the following components:
# commands ... list of commands to generate files containing the plot
#              (only if preview = FALSE)
# values ..... list containing the width and height of the plot in inches
# errors ..... (list of) error messages to be displayed
prepare_plot <- function(input, commands, values, preview = FALSE) {
  # check if file type has been selected and ROBMED has been run
  have_file_type <- isTruthy(input$file_type)
  have_ROBMED <- isTruthy(commands$ROBMED) && isTruthy(values$ROBMED)
  if (have_file_type && have_ROBMED) {
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
    # if relevant, construct list of commands to export the diagnostic plot
    if (preview) commands_plot <- NULL
    else {
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
      # construct list of commands to export the diagnostic plot
      commands_plot <- list(pdf = command_pdf,
                            png = command_png,
                            generate = call("print", as.name("p")),
                            close = call("dev.off"))
      attr(commands_plot, "time_stamp") <- Sys.time()
    }
    # construct list with plot dimensions
    values_plot <- list(width = width, height = height)
    # indicate that there are no errors
    errors_plot <- NULL
  } else {
    # construct error messages
    commands_plot <- NULL
    values_plot <- NULL
    errors_plot <- list(
      file_type = if (!have_file_type) "Select at least one file type",
      preview_plot = if (!have_ROBMED) "Run ROBMED in the respective tab."
    )
  }
  # return list
  list(commands = commands_plot, values = values_plot, errors = errors_plot)
}


# wrapper function to deparse command with certain arguments
deparse_command <- function(expr) {
  # TODO: replace this with something that produces nicer looking code
  #       (e.g., formatR::tidy_source() or something from package 'styler')
  deparse(expr, width.cutoff = 80L, backtick = FALSE, control = "niceNames")
}

# function to generate replication script from logged commands
generate_replication_script <- function(commands, file) {

  # initializations
  commands_ROBMED <- commands$ROBMED
  have_ROBMED <- !is.null(commands_ROBMED)
  commands_OLS_boot <- commands$OLS_boot
  have_OLS_boot <- !is.null(commands_OLS_boot)
  commands_table <- commands$table

  # construct vector containing lines to apply ROBMED and export diagnostic plot
  if (have_ROBMED) {
    # construct vector containing lines to apply ROBMED
    lines_ROBMED <- c(
      "",
      if (!is.null(commands_ROBMED$seed)) {
        c("# set the seed of the random number generator for reproducibility",
          deparse_command(commands_ROBMED$seed))
      },
      "# apply the robust bootstrap test ROBMED",
      if (!is.null(commands_ROBMED$control)) {
        deparse_command(commands_ROBMED$control)
      },
      deparse_command(commands_ROBMED$mediation),
      "# show a summary of the results",
      deparse_command(commands_ROBMED$summary),
      "# generate the diagnostic plot",
      deparse_command(commands_ROBMED$plot)
    )
    # construct vector containing lines to export diagnostic plot
    commands_plot <- commands$plot
    lines_plot <- c(
      if (!is.null(commands_plot$pdf)) {
        c("",
          "# generate a pdf file containing the diagnostic plot for ROBMED",
          deparse_command(commands_plot$pdf),
          deparse_command(commands_plot$generate),
          deparse_command(commands_plot$close))
      },
      if (!is.null(commands_plot$png)) {
        c("",
          "# generate a png image containing the diagnostic plot for ROBMED",
          deparse_command(commands_plot$png),
          deparse_command(commands_plot$generate),
          deparse_command(commands_plot$close))
      }
    )
  } else {
    lines_ROBMED <- NULL
    lines_plot <- NULL
  }

  # construct vector containing lines to apply the OLS bootstrap
  if (have_OLS_boot) {
    lines_OLS_boot <- c(
      "",
      if (!is.null(commands_OLS_boot$seed)) {
        c("# set the seed of the random number generator for reproducibility",
          deparse_command(commands_OLS_boot$seed))
      },
      "# apply the OLS bootstrap test",
      deparse_command(commands_OLS_boot$mediation),
      "# show a summary of the results",
      deparse_command(commands_OLS_boot$summary)
    )
  } else lines_OLS_boot <- NULL

  # combine lines for ROBMED and the OLS bootstrap
  # (depending on which was logged first)
  if (have_ROBMED && have_OLS_boot) {
    time_stamp_ROBMED <- attr(commands_ROBMED, "time_stamp")
    time_stamp_OLS_boot <- attr(commands_OLS_boot, "time_stamp")
    if (time_stamp_ROBMED < time_stamp_OLS_boot) {
      lines_methods <- c(lines_ROBMED, lines_OLS_boot)
    } else lines_methods <- c(lines_OLS_boot, lines_ROBMED)
  } else lines_methods <- c(lines_ROBMED, lines_OLS_boot)

  # construct vector containing lines of replication script
  # TODO: add sessionInfo() as a comment
  replication_code <- c(
    "# generated by the graphical user interface for (robust) mediation analysis",
    "# from package 'robmedExtra'",
    "",
    "# load required packages",
    sapply(commands$packages, deparse_command),
    "",
    "# set version of the random number generator to improve future reproducibility",
    deparse_command(commands$RNG),
    "",
    "# load data",
    deparse_command(commands$data$load),
    lines_methods,
    "",
    "# export a table of results to Microsoft Word",
    deparse_command(commands_table$generate),
    deparse_command(commands_table$export),
    lines_plot
  )

  # write lines of replication script to file
  writeLines(replication_code, con = file)

}


# Server-side logic for GUI -----

#' @import shiny
#' @importFrom DT renderDataTable
#' @importFrom flextable htmltools_value
#' @importFrom utils zip
#' @import robmed

shinyServer(function(input, output, session) {

  ## Define relevant objects and reactive expressions -----

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

  # initialize reactive values for error messages
  errors <- reactiveValues()

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
      help_text("If you have a data set that is not in RData format, you can",
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
        error_text("There are no data frames in the selected RData file.")
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
      # construct commands to save and load the data
      RData_file <- paste(df_name, "RData", sep = ".")
      command_save <- call("save", as.name(df_name), file = RData_file)
      command_load <- call("load", RData_file)
      # update relevant reactive values
      commands$data <- list(save = command_save, load = command_load)
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
    # (which is used to clear output)
    commands$ROBMED <- NULL
    commands$OLS_boot <- NULL
    commands$table <- NULL
    commands$plot <- NULL
    values$ROBMED <- NULL
    values$OLS_boot <- NULL
    values$plot <- NULL
    errors$ROBMED <- NULL
    errors$OLS_boot <- NULL
    errors$export <- NULL
  })


  ## Render outputs for the 'Data' tab -----

  # show data frame in main panel
  output$data_table <- DT::renderDataTable({
    req(values$df_name)
    get(values$df_name, envir = session_env)
  })


  ## Update inputs for the 'Model' tab -----

  # create UI element to show help text
  output$select_variables <- renderUI({
    # get variable names
    variables <- values$variables
    numeric_variables <- values$numeric_variables
    if (isTruthy(variables) && isTruthy(numeric_variables)) {
      # only show inputs if a data frame has been selected
      tagList(
        selectInput("y", label = get_label("Dependent variable", "(Numeric)"),
                    choices = c("", numeric_variables), selected = NULL,
                    multiple = FALSE),
        selectInput("x", label = "Independent variable(s)",
                    choices = variables, selected = NULL,
                    multiple = TRUE),
        selectInput("m", label = get_label("Mediator(s)", "(Numeric)"),
                    choices = numeric_variables, selected = NULL,
                    multiple = TRUE),
        selectInput("covariates", label = "Covariate(s)",
                    choices = variables, selected = NULL,
                    multiple = TRUE),
      )
    } else {
      # otherwise show error message that data frame needs to be selected
      error_text("Select a data frame in the", em("Data"), "tab.")
    }
  })

  # # observer to reset selected variables when data set is selected
  # observe({
  #   # get variable names
  #   variables <- values$variables
  #   numeric_variables <- values$numeric_variables
  #   # update UI inputs for selecting variables
  #   updateSelectInput(session, inputId = "y",
  #                     choices = c("", numeric_variables),
  #                     selected = NULL)
  #   updateSelectInput(session, inputId = "x",
  #                     choices = variables,
  #                     selected = NULL)
  #   updateSelectInput(session, inputId = "m",
  #                     choices = numeric_variables,
  #                     selected = NULL)
  #   updateSelectInput(session, inputId = "covariates",
  #                     choices = variables,
  #                     selected = NULL)
  # })

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
    if (isTruthy(input$m) && length(input$m) > 1L) {
      selectInput("model", "Multiple mediator model:",
                  choices = c('parallel', 'serial'),
                  selected = isolate(input$model),
                  multiple = FALSE)
    }
  })

  # observer to clean up reactive values when variables are selected
  # (which is used to clear output)
  observeEvent(c(input$y, input$x, input$m, input$covariates), {
    # clean up reactive values for commands
    commands$ROBMED <- NULL
    commands$OLS_boot <- NULL
    commands$table <- NULL
    commands$plot <- NULL
    # clean up reactive values for plot preview
    values$ROBMED <- NULL
    values$OLS_boot <- NULL
    values$plot <- NULL
    # clean up reactive values for error messages
    errors$ROBMED <- NULL
    errors$OLS_boot <- NULL
    errors$export <- NULL
  }, ignoreInit = TRUE)


  ## update inputs for the 'ROBMED' tab

  # observer for button to run ROBMED
  observeEvent(input$run_ROBMED, {

    # check if necessary inputs are selected
    have_y <- isTruthy(input$y)
    have_x <- isTruthy(input$x)
    have_m <- isTruthy(input$m)
    if (have_y && have_x && have_m) {

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
      # (which are evaluated to create output and written to replication script)
      commands_ROBMED <- list(seed = command_seed,
                              control = command_ctrl,
                              mediation = command_robust_boot,
                              summary = command_summary,
                              plot = command_p)
      attr(commands_ROBMED, "time_stamp") <- Sys.time()
      commands$ROBMED <- commands_ROBMED
      # update reactive values with list of options
      # (which are used to check if options are the same for ROBMED and OLS
      # bootstrap, or if inputs have changed since output was generated)
      values$ROBMED <- list(level = input$level_ROBMED, R = input$R_ROBMED,
                            seed = input$seed_ROBMED)
      # clean up reactive values for relevant commands and plot preview
      # (which is used to clear output)
      commands$table <- NULL
      commands$plot <- NULL
      values$plot <- NULL
      errors$export <- NULL

    } else {

      # construct error message that variables need to be selected
      errors$ROBMED <- get_variable_selection_error(have_y = have_y,
                                                    have_x = have_x,
                                                    have_m = have_m)

    }

  })

  # show error messages for ROBMED
  output$error_run_ROBMED <- renderUI({
    show_errors(errors$ROBMED)
  })

  # show help text if no random number seed is selected
  output$help_seed_ROBMED <- renderUI({
    if (!isTruthy(input$seed_ROBMED)) {
      warning_text("The analysis is", strong("not reproducible"),
                   "without setting a seed.")
    }
  })

  # show advanced options if selected
  output$MM_options <- renderUI({
    req(input$show_advanced_options)
    tagList(
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

  # observer for button to run the OLS bootstrap
  observeEvent(input$run_OLS_boot, {

    # check if necessary inputs are selected
    have_y <- isTruthy(input$y)
    have_x <- isTruthy(input$x)
    have_m <- isTruthy(input$m)
    if (have_y && have_x && have_m) {

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
      # (which are evaluated to create output and written to replication script)
      commands_OLS_boot <- list(seed = command_seed,
                                mediation = command_ols_boot,
                                summary = command_summary)
      attr(commands_OLS_boot, "time_stamp") <- Sys.time()
      commands$OLS_boot <- commands_OLS_boot
      # update reactive values with list of options
      # (which are used to check if options are the same for ROBMED and OLS
      # bootstrap, or if inputs have changed since output was generated)
      values$OLS_boot <- list(level = input$level_OLS_boot, R = input$R_OLS_boot,
                              seed = input$seed_OLS_boot)
      # clean up reactive values for relevant commands
      # (which is used to clear output)
      commands$table <- NULL
      errors$export <- NULL

    } else {

      # construct error message that variables need to be selected
      errors$OLS_boot <- get_variable_selection_error(have_y = have_y,
                                                      have_x = have_x,
                                                      have_m = have_m)

    }

  })

  # show error messages for the OLS Bootstrap
  output$error_run_OLS_boot <- renderUI({
    show_errors(errors$OLS_boot)
  })

  # show help text if no random number seed is selected
  output$help_seed_OLS_boot <- renderUI({
    if (!isTruthy(input$seed_OLS_boot)) {
      warning_text("The analysis is", strong("not reproducible"),
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

  # # observer to determine whether to show buttons or help text
  # observe({
  #   # initializations
  #   have_ROBMED <- isTruthy(commands$ROBMED) && isTruthy(values$ROBMED)
  #   have_OLS_boot <- isTruthy(commands$OLS_boot) && isTruthy(values$OLS_boot)
  #
  #
  #
  #   have_no_method <- !have_ROBMED && !have_OLS_boot
  #   have_different_options <- have_ROBMED && have_OLS_boot &&
  #     isolate(!identical(values$ROBMED, values$OLS_boot))
  #   have_no_file_type <- have_ROBMED && !isTruthy(input$file_type)
  #
  #
  #   # determine whether to show button to export files
  #   show_button_table <- !have_no_method && !have_different_options &&
  #     !have_no_file_type
  #   if (show_button_table) help_text_button_export <- NULL
  #   else {
  #     help_text_button_export <- c(
  #       if (have_no_method) "Run ROBMED or the OLS bootstrap in the respective tabs.",
  #       if (have_different_options) "",
  #       if (have_no_file_type) ""
  #     )
  #   }
  #   # determine whether to show preview button for table
  #   # (note that we don't need to show a help text if the button is not
  #   # shown, as there will be a help text instead of the export button)
  #   show_button_table <- !have_no_method && !have_different_options
  #   # determine whether to show preview button for diagnostic plot
  #   show_button_plot <- have_ROBMED
  #   if (!show_button_plot && have_OLS_boot) {
  #     # If no method has been run, there will be a help text instead of the
  #     # export button.  But if the OLS bootstrap has been run, the export
  #     # button is active, and we need to show a help text specific for the
  #     # preview button for the diagnostic plot.
  #     help_text_button_plot <- "Run ROBMED in the respective tab."
  #   } else help_text_button_plot <- NULL
  #
  # })
  #

  # # create UI button to export files
  # output$button_export <- renderUI({
  #   # initializations
  #   have_ROBMED <- isTruthy(commands$ROBMED) && isTruthy(values$ROBMED)
  #   have_OLS_boot <- isTruthy(commands$OLS_boot) && isTruthy(values$OLS_boot)
  #   ok <- TRUE
  #   # show help text if no method has been run
  #   if (have_ROBMED || have_OLS_boot) help_text_no_method <- NULL
  #   else {
  #     ok <- FALSE
  #     help_text_no_method <- helpText("Run ROBMED or the OLS bootstrap in the",
  #                                     "respective tabs.")
  #   }
  #   # show help text if ROBMED has been run but no file type for diagnostic
  #   # plot is selected
  #   if (have_ROBMED && !isTruthy(input$file_type)) {
  #     ok <- FALSE
  #     help_text_no_file_type <- helpText("Select at least one file type for",
  #                                        "the diagnostic plot.")
  #   } else help_text_no_file_type <- NULL
  #   # show help text if ROBMED and OLS bootstrap use different options
  #   if (have_ROBMED && have_OLS_boot &&
  #       isolate(!identical(values$ROBMED, values$OLS_boot))) {
  #     ok <- FALSE
  #     help_text_options <- helpText("ROBMED and the OLS bootstrap use",
  #                                   "different options. Re-run the methods",
  #                                   "with the same options.")
  #   } else help_text_options <- NULL
  #   # show button to export files if requirements are met
  #   if (ok) downloadButton("export_files", "Export files")
  #   else {
  #     tagList(help_text_no_method, help_text_no_file_type, help_text_options)
  #   }
  # })

  # show error messages for the file export
  output$error_export_files <- renderUI({
    show_errors(errors$export$export)
  })

  # create UI input for orientation of the table
  output$select_orientation <- renderUI({
    # show the input if both ROBMED and OLS bootstrap have been run
    req(commands$ROBMED, commands$OLS_boot)
    radioButtons("orientation", "Orientation",
                 choices = c("portrait", "landscape"),
                 selected = isolate(input$orientation))
  })

  # # create UI button to preview the table
  # output$button_table <- renderUI({
  #   if (isTruthy(commands$ROBMED) || isTruthy(commands$OLS_boot)) {
  #     # Show the button if ROBMED or OLS bootstrap have been run.  Note that
  #     # otherwise we don't need to show a help text, as there is already a
  #     # help text in place instead of the export button.
  #     actionButton("preview_table", "Preview")
  #   }
  # })

  # observer for button to preview the table
  observeEvent(input$preview_table, {
    # obtain list containing commands and options or errors
    table_preview <- prepare_table(input = input, commands = commands,
                                   values = values)
    # evaluate command to generate the flextable
    commands_table <- table_preview$commands
    if (isTruthy(commands_table)) {
      eval(commands_table$generate, envir = session_env)
    }
    # update reactive values to trigger table preview
    commands$table <- commands_table
    values$table <- table_preview$values
    errors$export <- table_preview$errors
  })

  # show error messages for the table preview
  output$error_preview_table <- renderUI({
    req(errors$export$preview_table)
    show_errors(errors$export$preview_table)
  })

  # show error messages for the file type for the diagnostic plot
  output$error_file_type <- renderUI({
    req(errors$export$file_type)
    show_errors(errors$export$file_type)
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

  # create UI input for resulution of the png image
  output$select_resolution <- renderUI({
    if ("png" %in% input$file_type) {
      numericInput("resolution", "Resolution (pixels per inch)",
                   value = 300, min = 0, step = 50)
    }
  })

  # # create UI button to preview the diagnostic plot
  # output$button_plot <- renderUI({
  #   if (isTruthy(commands$ROBMED)) {
  #     # show button if ROBMED has been run
  #     actionButton("preview_plot", "Preview")
  #   } else if (isTruthy(commands$OLS_boot)) {
  #     # If no method has been run, there is already a help text in place
  #     # instead of the export button.  But if the OLS bootstrap has been run,
  #     # the export button is active, so we need to show a help text specific
  #     # for the preview button.
  #     helpText("Run ROBMED in the respective tab.")
  #   }
  # })

  # observer for button to preview the diagnostic plot
  observeEvent(input$preview_plot, {
    # obtain list with values for plot dimensions or error messages
    plot_preview <- prepare_plot(input = input, commands = commands,
                                 values = values, preview = TRUE)
    # update reactive values to trigger file preview
    values$plot <- plot_preview$values
    errors$export <- plot_preview$error
  })

  # show error messages for the preview of the diagnostic plot
  output$error_preview_plot <- renderUI({
    req(errors$export$preview_plot)
    show_errors(errors$export$preview_plot)
  })


  ## Render outputs for the 'Export' tab -----

  # FIXME: This is currently a mess and interface and buttons do not work
  #        as they should.

  # download zip archive of all files
  output$export_files <- downloadHandler(
    filename = function() {
      date <- format(Sys.Date(), "%Y-%m-%d")
      sprintf("mediation_analysis_%s_%s.zip", values$df_name, date)
    },
    content = function(file) {
      # if user didn't preview the table, then commands need to be generated
      # FIXME: in addition to checking whether the commands exist, we also need
      #        to check whether any inputs have changed since the preview was
      #        generated
      if (!isTruthy(commands$table)) {
        # # obtain commands to generate and export flextable
        # commands_table <- get_flextable_commands(input, commands)
        # # evaluate command to generate the flextable
        # eval(commands_table$generate, envir = session_env)
        # # update reactive value with commands to trigger table preview
        # commands$table <- commands_table
        # obtain list containing commands and options or errors
        table_preview <- prepare_table(input = input, commands = commands,
                                       values = values)
        # evaluate command to generate the flextable
        commands_table <- table_preview$commands
        if (isTruthy(commands_table)) {
          eval(commands_table$generate, envir = session_env)
        } else {
          errors$export <- table_preview$errors
          stop("something happend")
        }
        # update reactive values to trigger table preview
        commands$table <- commands_table
        values$table <- table_preview$values
        errors$export <- table_preview$errors
      }
      # obtain list with commands to export plot and values for plot dimensions
      commands_plot <- get_plot_commands(input, commands)
      # update reactive values to trigger file preview
      commands$plot <- commands_plot$commands
      values$plot <- commands_plot$values
      # switch to temporary directory to save files
      working_directory <- setwd(tempdir())
      on.exit(setwd(working_directory))
      # save data set to RData file
      RData_file <- paste(values$df_name, "RData", sep = ".")
      command_save <- call("save", as.name(values$df_name), file = RData_file)
      eval(commands$data$save, envir = session_env)
      # save table to Microsoft Word document
      eval(commands$table$export, envir = session_env)
      # save diagnostic plot to file(s)
      commands_plot <- commands$plot
      if (isTruthy(commands_plot)) {
        # if requested, plot to pdf file
        command_pdf <- commands_plot$pdf
        have_pdf <- !is.null(command_pdf)
        if (have_pdf) {
          eval(command_pdf, envir = session_env)
          eval(commands_plot$generate, envir = session_env)
          eval(commands_plot$close, envir = session_env)
        }
        # if requested, plot to png image
        command_png <- commands_plot$png
        have_png <- !is.null(command_png)
        if (have_png) {
          eval(command_png, envir = session_env)
          eval(commands_plot$generate, envir = session_env)
          eval(commands_plot$close, envir = session_env)
        }
      } else {
        have_pdf <- FALSE
        have_png <- FALSE
      }
      # generate replication script and save to file
      script_file <- "replication_script.R"
      generate_replication_script(commands, file = script_file)
      # download zip file containing all the above files
      files_to_zip <- c(RData_file,
                        if (have_pdf) command_pdf$file,
                        if (have_png) command_png$file,
                        commands$table$export$file,
                        script_file)
      print(files_to_zip)
      zip(zipfile = file, files = files_to_zip)
    },
    contentType = "application/zip"
  )

  # show preview of table in main panel
  output$table_preview_header <- renderUI({
    req(commands$table, values$table)
    h2("Table preview")
  })
  output$table_preview <- renderUI({
    req(commands$table, values$table)
    flextable::htmltools_value(get("ft", envir = session_env))
  })

  # show diagnostic plot for ROBMED in main panel
  output$plot_preview_header <- renderUI({
    req(values$plot)
    tagList(
      h2("File preview for diagnostic plot"),
      help_text("The size shown here depends on the resolution of the browser",
                "and may differ from the size of the file to be generated.")
    )
  })
  output$plot_preview <- renderPlot({
    get("p", envir = session_env)
  }, width = function() {
    req(values$plot)
    values$plot$width * 125
  }, height = function() {
    req(values$plot)
    values$plot$height * 125
  }, res = 125)

})

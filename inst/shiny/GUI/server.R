# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Load required packages -----
# we only load packages for which we don't use the :: operator to call functions
# (to keep the namespace clean)
library("shiny")
library("robmed")
library("robmedExtra")


# Internal functions -----


# function to create a help text element
# This is defined to have more control over the style compared to the built-in
# function shiny::helpText(). In particular, we can make sure that the color is
# matched in function get_label() and that the the to and bottom margins are
# matched in function error_message().
help_text <- function(...) {
  # FIXME: this is hard-coded to be the same as CSS class "help-block" in
  #        shiny bootstrap theme
  css <- "color: #737373; display: block; margin-top: 5px; margin-bottom: 10px;"
  span(style = css, ...)
}

# function to create an error text element
error_text <- function(...) {
  # FIXME: color is hard-coded to be the same as color  in error messages in
  #        shiny bootstrap theme
  css <- "color: #a94442; display: block; margin-top: 5px; margin-bottom: 10px;"
  span(style = css, ...)
}

# function to create a warning text element
warning_text <- function(...) {
  # FIXME: color is hard-coded to be the same as background color in warning
  #        messages in shiny bootstrap theme
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


# function to generate error message for missing variable selection
get_variable_selection_error <- function(y, x, m) {
  # initializations
  have_y <- !is.null(y) && nchar(y) > 0L
  have_x <- length(x) > 0L
  have_m <- length(m) > 0L
  # define pieces of text for missing variable selection
  text_y <- if (!have_y) "a dependent variable"
  text_x <- if (!have_x) "at least one independent variable"
  text_m <- if (!have_m) "at least one mediator"
  # put text for missing variable selection together
  n_yx <- sum(have_y, have_x)
  if (n_yx == 2L) text_select <- text_m
  else {
    text_yx <- paste(c(text_y, text_x), collapse = ", ")
    sep <- if (n_yx == 1L) " and " else ", and "
    text_select <- paste(c(text_yx, text_m), collapse = sep)
  }
  # put everything into a nice message
  error <- paste("Select", text_select, "in the <em>Model</em> tab.")
  # mark error message as HTML
  HTML(error)
}


# wrapper function to deparse command with certain arguments
# split ... character string for splitting the commands. Currently only ", "
#           for splitting after arguments or " + " for splitting after the plus
#           operator are supported (note the spaces).
# limit ... integer giving desired maximum line width.  This is only used for
#           splitting after arguments (split = ", ").  If this is set to NA,
#           each argument is put on a new line.
deparse_command <- function(expr, split = ", ", limit = 80L) {
  # deparse the expression
  # Note: 500 is the maximum that argument 'width.cutoff' allows, and setting
  #       argument 'nlines' to a negative value implies no limit on the number
  #       of lines to be produced
  command <- deparse(expr, width.cutoff = 500L, backtick = FALSE,
                     control = "niceNames", nlines = -1L)
  # if deparse() returns multiple lines (as separate strings), remove any
  # indentation and put everything back into one string
  if (length(command) > 1L) {
    command <- paste(gsub("^ +", "", command), collapse = "")
  }
  # split and format the command as requested to make the code more readable
  if (split == ", ") {
    # split the commands into parts after each occurrence of ", "
    command <- gsub(", ", ",\n ", command, fixed = TRUE)
    command <- strsplit(command, "\n", fixed = TRUE)[[1L]]
    # align subsequent lines with the opening parenthesis of the function call
    n_spaces <- nchar(strsplit(command[1], "(", fixed = TRUE)[[1L]][1L])
    spaces <- strrep(" ", n_spaces)
    # format command
    if (is.na(limit)) command[-1L] <- paste0(spaces, command[-1L])
    else {
      # add parts to a line as long as there is space (given by 'limit'),
      # and start a new line when running out of space
      parts <- command
      command <- character()
      while(length(parts) > 0L) {
        if (length(parts) == 1L) {
          # only one part left, so add it as a new line and we're done
          add <- 1L
        } else {
          # determine how many predictors have space
          width <- nchar(parts)
          add <- which(cumsum(width) <= limit)
          # if the first part is too long it needs to be added anyway
          if (length(add) == 0) add <- 1
        }
        # add a new line
        command <- c(command, paste(parts[add], collapse = ""))
        # remove the parts that have just been written to the new line
        parts <- parts[-add]
        # indent next line
        if (length(parts) > 0L) parts[1L] <- paste0(spaces, parts[1L])
      }
    }
  } else if (split == " + ") {
    # put each part of the command after the '+' operator on a new line
    # (indented by two spaces)
    command <- gsub(" + ", " +\n  ", command, fixed = TRUE)
    command <- strsplit(command, "\n", fixed = TRUE)[[1L]]
  }
  # return formatted lines of code for the command
  command
}


# function to get references as HTML tags to be displayed in the GUI or as
# lines to be written into a file for import into into a reference manager
get_references <- function(format = "HTML") {
  # FIXME: author information is currently hardcoded
  # get information for package 'robmedExtra'
  package <- "robmedExtra"
  description <- packageDescription(package)
  version <- toString(packageVersion(package))
  year <- format(packageDate(package), "%Y")
  title <- gsub("(\\s|\\()([A-Z])", "\\1\\L\\2", description$Title,
                perl = TRUE, fixed = FALSE)
  note <- paste("package version", version)
  URL <- description$URL
  if (format == "HTML") {
    title <- paste0("<strong>", package, "</strong>: ", title)
    note <- paste("<strong>R</strong>", note)
  } else {
    title <- paste(package, title, sep = ": ")
    note <- paste("R", note)
  }
  # construct references in requested format
  if (format == "HTML") {
    # author information for package 'robmedExtra'
    author <- "Alfons, A., Archimbaud, A., & Drenth, V."
    # return list of HTML tags
    tagList(
      # HTML tag for ORM paper
      ROBMED = p(
        HTML("Alfons, A., Ate&scedil;, N. Y., & Groenen, P. J. F. (2022)."),
        "A robust bootstrap test for mediation analysis.",
        HTML("<em>Organizational Research Methods</em>, <em>25</em>(3),",
             "591&ndash;617."),
        a("https://doi.org/10.1177/1094428121999096",
          href = "https://doi.org/10.1177/1094428121999096")
      ),
      # HTML tag for package 'robmed'
      robmed = p(
        HTML("Alfons, A., Ate&scedil;, N. Y., & Groenen, P. J. F. (2022)."),
        HTML("Robust mediation analysis: The <strong>R</strong> package",
             "<strong>robmed</strong>."),
        HTML("<em>Journal of Statistical Software</em>, <em>103</em>(13),",
             "1&ndash;45."),
        a("https://doi.org/10.18637/jss.v103.i13",
          href = "https://doi.org/10.18637/jss.v103.i13")
      ),
      # HTML tag for package 'robmedExtra'
      robmedExtra = p(
        paste0(author, " (", year, ")."), HTML(paste0(title, ".")),
        HTML(paste0(note, ".")), a(URL, href = URL)
      )
    )
  } else if (format == "EndNote") {
    # return vector of lines to be written to .enw file
    c(
      # EndNote reference for ORM paper
      "%0 Journal Article",
      "%A Alfons, A.",
      "%A Ate\U015F, N. Y.",
      "%A Groenen, P. J. F.",
      "%D 2022",
      "%T A Robust Bootstrap Test for Mediation Analysis",
      "%J Organizational Research Methods",
      "%V 25",
      "%N 3",
      "%P 591-617",
      "%R 10.1177/1094428121999096",
      "%U https://doi.org/10.1177/1094428121999096",
      "",
      # EndNote reference for package 'robmed'
      "%0 Journal Article",
      "%A Alfons, A.",
      "%A Ate\U015F, N. Y.",
      "%A Groenen, P. J. F.",
      "%D 2022",
      "%T Robust mediation analysis: The R package robmed",
      "%J Journal of Statistical Software",
      "%V 103",
      "%N 13",
      "%P 1-45",
      "%R 10.18637/jss.v103.i13",
      "%U https://doi.org/10.18637/jss.v103.i13",
      "",
      # EndNote reference for package 'robmedExtra'
      "%0 Computer Program",
      "%A Alfons, A.",
      "%A Archimbaud, A.",
      "%A Drenth, V.",
      paste("%D", year),
      paste("%T", title),
      paste("%Z", note),
      paste("%U", URL)
    )
  } else if (format == "BibTeX") {
    # author information for package 'robmedExtra'
    author <- "Alfons, A. and Archimbaud, A. and Drenth, V."
    # return vector of lines to be written to .bib file
    c(
      # BibTeX entry for ORM paper
      "@article{alfons2022a,",
      "  author = {Alfons, A. and Ate\\c{s}, N. Y. and Groenen, P. J. F.},",
      "  year = {2022},",
      "  title = {A Robust Bootstrap Test for Mediation Analysis},",
      "  journal = {Organizational Research Methods},",
      "  volume = {25},",
      "  number = {3},",
      "  pages = {591--617},",
      "  doi = {10.1177/1094428121999096},",
      "  url = {https://doi.org/10.1177/1094428121999096}",
      "}",
      "",
      # BibTeX entry for package 'robmed'
      "@article{alfons2022b,",
      "  author = {Alfons, A. and Ate\\c{s}, N. Y. and Groenen, P. J. F.},",
      "  year = {2022},",
      "  title = {Robust mediation analysis: The R package robmed},",
      "  journal = {Journal of Statistical Software},",
      "  volume = {103},",
      "  number = {13},",
      "  pages = {1--45},",
      "  doi = {10.18637/jss.v103.i13},",
      "  url = {https://doi.org/10.18637/jss.v103.i13}",
      "}",
      "",
      # BibTeX entry for package 'robmedExtra'
      sprintf("@manual{alfons%s,", year),
      sprintf("  author = {%s}", author),
      sprintf("  year = {%s}", year),
      sprintf("  title = {%s}", title),
      sprintf("  note = {%s}", note),
      sprintf("  url = {%s}", URL),
      "}"
    )
  }
}


# Server-side logic for GUI -----

#' @import shiny
#' @importFrom DT renderDataTable
#' @importFrom flextable htmltools_value
#' @importFrom ggplot2 scale_color_manual theme
#' @importFrom grDevices dev.off pdf png
#' @importFrom robmed reg_control test_mediation weight_plot
#' @importFrom utils zip

shinyServer(function(input, output, session) {

  ## Define relevant objects and reactive values -----

  # check if there are any data frames in global environment
  df_global <- get_data_frames()
  n_df_global <- length(df_global)

  # create a separate environment for safely loading RData files
  RData_env <- new.env()

  # create a separate environment for safely conducting analyses
  session_env <- new.env()

  # initialize reactive values for sanitized versions of inputs
  values <- reactiveValues(data_source = "RData file",
                           df_name = "",
                           variables = character(),
                           numeric_variables = character(),
                           level = 0.95,
                           R = 5000,
                           show_advanced_options = FALSE,
                           seed = as.integer(format(Sys.Date(), "%Y%m%d")),
                           type = "boot",
                           file_type_table = c("docx", "pptx"),
                           file_type_plot = c("pdf", "png"),
                           units = "cm")

  # initialize reactive values for commands
  RNG_kind <- RNGkind()
  commands <- reactiveValues(
    packages = list(robmed = call("library", "robmed"),
                    robmedExtra = call("library", "robmedExtra")),
    RNG = call("RNGkind", RNG_kind[1L], RNG_kind[2L], RNG_kind[3L])
  )

  # initialize reactive values for snapshots of inputs when buttons are pressed
  used_inputs <- reactiveValues()

  # information on R and relevant packages
  R_version <- paste(R.Version()[c("major", "minor")], collapse = ".")
  robmed_version <- toString(packageVersion("robmed"))
  robmedExtra_version <- toString(packageVersion("robmedExtra"))
  references <- get_references("HTML")


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

  # observer to make sure that the data source is properly defined
  # If there are no data frames in the global environment, the input for the
  # data source is not shown, and the user can only select an RData file.
  # This is why we use the sanitized version 'values$data_source' (which is
  # properly initialized) rather than the input 'input$data_source'.
  observeEvent(input$data_source, {
    values$data_source <- input$data_source
  })

  # create UI input for selecting a data frame from the global environment
  output$select_df_global <- renderUI({
    # show UI input only if the data source is set accordingly
    req(values$data_source == "R environment")
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
  })

  # create UI input for selecting an RData file
  output$select_Rdata_file <- renderUI({
    # show UI input only if the data source is set accordingly
    req(values$data_source == "RData file")
    fileInput("RData_file", "RData file", multiple = FALSE, accept = ".RData")
  })

  # create UI input for selecting a data frame from the selected RData file
  output$select_df_RData <- renderUI({
    # show UI input only if the data source is set accordingly and an RData
    # file is selected
    req(values$data_source == "RData file", input$RData_file)
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
  })

  # observer to update data in separate environment for session
  observe({
    # obtain name of data frame and R environment corresponding to data source
    if (values$data_source == "RData file") {
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
      values$df_name <- ""
      values$variables <- character()
      values$numeric_variables <- character()
    }
    # clean up reactive values to clear output from other tabs
    commands$ROBMED <- NULL
    commands$OLS_boot <- NULL
    commands$table <- NULL
    commands$plot <- NULL
    used_inputs$ROBMED <- NULL
    used_inputs$OLS_boot <- NULL
    used_inputs$table <- NULL
    used_inputs$plot <- NULL
    values$download <- NULL
  })

  # if relevant, show UI element with error messages for the selected data frame
  output$error_data <- renderUI({
    req(values$df_name)
    if (length(values$variables) < 3L) {
      error_text("The selected data frame must contain",
                 HTML("<strong>at least 3 variables</strong>."))
    } else if (length(values$numeric_variables) < 2L) {
      error_text("The selected data frame must contain",
                 strong("at least 2 numeric variables"),
                 "(as currently only a numeric dependent variable",
                 "and numeric mediators are supported).")
    }
  })


  ## Render outputs for the 'Data' tab -----

  # show data frame in main panel
  output$data_table <- DT::renderDataTable({
    req(values$df_name)
    get(values$df_name, envir = session_env)
  })


  ## Update inputs for the 'Model' tab -----

  # create UI element with inputs for variable selection
  output$select_variables <- renderUI({
    # get variable names
    variables <- values$variables
    numeric_variables <- values$numeric_variables
    if (length(variables) >= 3L && length(numeric_variables) >= 2L) {
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
                    multiple = TRUE)
      )
    } else {
      # otherwise show error message that data frame needs to be selected
      error_text("Select a data frame in the", em("Data"), "tab.")
    }
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
    req(length(values$variables) >= 3L,
        length(values$numeric_variables) >= 2L,
        length(input$m) > 1L)
    selectInput("model", "Multiple mediator model:",
                choices = c("parallel", "serial"),
                selected = isolate(input$model),
                multiple = FALSE)
  })

  # observer to clean up reactive values when variables are selected
  # (which is used to clear output)
  observeEvent(c(input$y, input$x, input$m, input$covariates), {
    # clean up reactive values for commands
    commands$ROBMED <- NULL
    commands$OLS_boot <- NULL
    commands$table <- NULL
    commands$plot <- NULL
    # clean up reactive values for used inputs
    used_inputs$ROBMED <- NULL
    used_inputs$OLS_boot <- NULL
    used_inputs$table <- NULL
    used_inputs$plot <- NULL
    # clean up reactive value for files to export
    values$download <- NULL
  }, ignoreInit = TRUE)


  ## update inputs for the 'ROBMED' tab

  # create UI element with inputs for ROBMED options
  output$options_ROBMED <- renderUI({
    if (isTruthy(input$y) && isTruthy(input$x) && isTruthy(input$m)) {
      # only show inputs if the required variables have been selected
      tagList(
        # button to perform ROBMED
        actionButton("run_ROBMED", "Run"),
        # options for the bootstrap confidence intervals
        h3("Options"),
        numericInput("level_ROBMED", "Confidence level",
                     value = isolate(values$level),
                     min = 0.9, max = 0.999, step = 0.01),
        numericInput("R_ROBMED", "Number of bootstrap samples",
                     value = isolate(values$R), min = 1000,
                     step = 1000),
        # checkbox whether to show advanced options
        checkboxInput("show_advanced_options_ROBMED", "Show advanced options",
                      value = isolate(values$show_advanced_options))
      )
    } else {
      # otherwise show an error message on which variables need to be selected
      error_text(get_variable_selection_error(input$y, input$x, input$m))
    }
  })

  # show advanced options if selected
  output$advanced_options_ROBMED <- renderUI({
    req(input$y, input$x, input$m, values$show_advanced_options)
    # for MM-estimator use previous values as defaults if they exist
    # (other inputs are synchronized with OLS bootstrap via 'values')
    default_efficiency <- isolate(input$efficiency)
    if (is.null(default_efficiency)) default_efficiency <- 0.85
    default_max_iterations <- isolate(input$max_iterations)
    if (is.null(default_max_iterations)) default_max_iterations <- 10000
    # create inputs
    tagList(
      numericInput("seed_ROBMED", "Seed of the random number generator",
                   value = isolate(values$seed)),
      uiOutput("help_seed_ROBMED"),
      selectInput("type_ROBMED", "Inference for total and direct effects",
                  choices = c("Normal theory t tests" = "data",
                              "Bootstrap z tests" = "boot"),
                  selected = isolate(values$type), multiple = FALSE),
      h3("MM-estimator"),
      selectInput("efficiency", "Efficiency at normal distribution",
                  choices = c(0.80, 0.85, 0.90, 0.95),
                  selected = default_efficiency,
                  multiple = FALSE),
      numericInput("max_iterations", "Maximum number of iterations",
                   value = default_max_iterations, min = 1000,
                   step = 1000)
    )
  })

  # show warning text if no random number seed is selected
  output$help_seed_ROBMED <- renderUI({
    if (!isTruthy(values$seed)) {
      warning_text("The analysis is", strong("not reproducible"),
                   "without setting a seed.")
    }
  })

  # observer for button to run ROBMED
  observeEvent(input$run_ROBMED, {
    # clean up reactive values to clear output
    commands$ROBMED <- NULL
    commands$table <- NULL
    commands$plot <- NULL
    used_inputs$ROBMED <- NULL
    used_inputs$table <- NULL
    used_inputs$plot <- NULL
    values$download <- NULL
    # construct command to set the seed of the random number generator
    if (isTruthy(values$seed)) {
      command_seed <- call("set.seed", values$seed)
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
    command_test_mediation$R <- values$R
    command_test_mediation$level <- values$level
    command_test_mediation$robust <- TRUE
    if (length(input$m) > 1L) command_test_mediation$model = input$model
    if (use_control) command_test_mediation$control <- as.name("ctrl")
    command_robust_boot <- call("<-", as.name("robust_boot"),
                                command_test_mediation)
    eval(command_robust_boot, envir = session_env)
    # construct command to show summary
    command_summary <- call("summary", as.name("robust_boot"),
                            type = values$type, plot = FALSE)
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
                            generate_plot = command_plot,
                            assign_plot = command_p)
    attr(commands_ROBMED, "time_stamp") <- Sys.time()
    commands$ROBMED <- commands_ROBMED
    # update reactive values with list of used inputs
    # (which are used to check if options are the same for ROBMED and OLS
    # bootstrap, or if inputs have changed since output was generated)
    used_inputs$ROBMED <- list(level = values$level, R = values$R,
                               seed = values$seed, type = values$type)
  })

  # observer to ensure that confidence level is the same as for OLS bootstrap
  observeEvent(input$level_OLS_boot, {
    values$level <- input$level_OLS_boot
    updateNumericInput(session, inputId = "level_ROBMED",
                       value = values$level)
  })

  # observer to ensure that number of bootstrap samples is the same as for
  # OLS bootstrap
  observeEvent(input$R_OLS_boot, {
    values$R <- input$R_OLS_boot
    updateNumericInput(session, inputId = "R_ROBMED",
                       value = values$R)
  })

  # observer to ensure advanced options are shown the same as for OLS bootstrap
  observeEvent(input$show_advanced_options_OLS_boot, {
    values$show_advanced_options <- input$show_advanced_options_OLS_boot
    updateCheckboxInput(session, inputId = "show_advanced_options_ROBMED",
                        value = values$show_advanced_options)
  })

  # observer to ensure that seed of the random number generator is the same as
  # for OLS bootstrap
  observeEvent(input$seed_OLS_boot, {
    values$seed <- input$seed_OLS_boot
    updateNumericInput(session, inputId = "seed_ROBMED",
                       value = values$seed)
  })

  # observer to ensure that inference type is the same as for OLS bootstrap
  observeEvent(input$type_OLS_boot, {
    values$type <- input$type_OLS_boot
    updateNumericInput(session, inputId = "type_ROBMED",
                       value = values$type)
  })


  ## Render outputs for the 'ROBMED' tab -----

  # show diagnostic plot for ROBMED in main panel
  output$plot_ROBMED_header <- renderUI({
    req(commands$ROBMED)
    h3("Diagnostic plot")
  })
  output$plot_ROBMED <- renderPlot({
    req(commands$ROBMED)
    get("p", envir = session_env)
  }, res = 100)

  # show summary for ROBMED in main panel
  output$summary_ROBMED_header <- renderUI({
    req(commands$ROBMED)
    h3("Model and test summaries")
  })
  output$summary_ROBMED <- renderPrint({
    req(commands$ROBMED)
    eval(commands$ROBMED$summary, envir = session_env)
  })


  ## update inputs for the 'OLS Bootstrap' tab

  # create UI element with inputs for OLS bootstrap options
  output$options_OLS_boot <- renderUI({
    if (isTruthy(input$y) && isTruthy(input$x) && isTruthy(input$m)) {
      # only show inputs if the required variables have been selected
      tagList(
        # button to perform the OLS bootstrap
        actionButton("run_OLS_boot", "Run"),
        # options for the bootstrap confidence intervals
        h3("Options"),
        numericInput("level_OLS_boot", "Confidence level",
                     value = isolate(values$level),
                     min = 0.9, max = 0.999, step = 0.01),
        numericInput("R_OLS_boot", "Number of bootstrap samples",
                     value = isolate(values$R), min = 1000,
                     step = 1000),
        # checkbox whether to show advanced options
        checkboxInput("show_advanced_options_OLS_boot", "Show advanced options",
                      value = isolate(values$show_advanced_options))
      )
    } else {
      # otherwise show an error message on which variables need to be selected
      error_text(get_variable_selection_error(input$y, input$x, input$m))
    }
  })

  # show advanced options if selected
  output$advanced_options_OLS_boot <- renderUI({
    req(input$y, input$x, input$m, values$show_advanced_options)
    # create inputs
    tagList(
      numericInput("seed_OLS_boot", "Seed of the random number generator",
                   value = isolate(values$seed)),
      uiOutput("help_seed_OLS_boot"),
      selectInput("type_OLS_boot", "Inference for total and direct effects",
                  choices = c("Normal theory t tests" = "data",
                              "Bootstrap z tests" = "boot"),
                  selected = isolate(values$type), multiple = FALSE)
    )
  })

  # show warning text if no random number seed is selected
  output$help_seed_OLS_boot <- renderUI({
    if (!isTruthy(values$seed)) {
      warning_text("The analysis is", strong("not reproducible"),
                   "without setting a seed.")
    }
  })

  # observer for button to run the OLS bootstrap
  observeEvent(input$run_OLS_boot, {
    # clean up reactive values to clear output
    commands$OLS_boot <- NULL
    commands$table <- NULL
    commands$plot <- NULL
    used_inputs$OLS_boot <- NULL
    used_inputs$table <- NULL
    used_inputs$plot <- NULL
    values$download <- NULL
    # construct command to set the seed of the random number generator
    if (isTruthy(values$seed)) {
      command_seed <- call("set.seed", values$seed)
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
    command_test_mediation$R <- values$R
    command_test_mediation$level <- values$level
    command_test_mediation$robust <- FALSE
    if (length(input$m) > 1L) command_test_mediation$model = input$model
    command_ols_boot <- call("<-", as.name("ols_boot"), command_test_mediation)
    eval(command_ols_boot, envir = session_env)
    # construct command to show summary
    command_summary <- call("summary", as.name("ols_boot"), type = values$type)
    # update reactive value with list of commands to perform the OLS bootstrap
    # (which are evaluated to create output and written to replication script)
    commands_OLS_boot <- list(seed = command_seed,
                              mediation = command_ols_boot,
                              summary = command_summary)
    attr(commands_OLS_boot, "time_stamp") <- Sys.time()
    commands$OLS_boot <- commands_OLS_boot
    # update reactive values with list of used inputs
    # (which are used to check if options are the same for ROBMED and OLS
    # bootstrap, or if inputs have changed since output was generated)
    used_inputs$OLS_boot <- list(level = values$level, R = values$R,
                                 seed = values$seed, type = values$type)
  })

  # observer to ensure that confidence level is the same as for ROBMED
  observeEvent(input$level_ROBMED, {
    values$level <- input$level_ROBMED
    updateNumericInput(session, inputId = "level_OLS_boot",
                       value = values$level)
  })

  # observer to ensure that number of bootstrap samples is the same as for
  # ROBMED
  observeEvent(input$R_ROBMED, {
    values$R <- input$R_ROBMED
    updateNumericInput(session, inputId = "R_OLS_boot",
                       value = values$R)
  })

  # observer to ensure advanced options are shown the same as for ROBMED
  observeEvent(input$show_advanced_options_ROBMED, {
    values$show_advanced_options <- input$show_advanced_options_ROBMED
    updateCheckboxInput(session, inputId = "show_advanced_options_OLS_boot",
                        value = values$show_advanced_options)
  })

  # observer to ensure that seed of the random number generator is the same as
  # for ROBMED
  observeEvent(input$seed_ROBMED, {
    values$seed <- input$seed_ROBMED
    updateNumericInput(session, inputId = "seed_OLS_boot",
                       value = values$seed)
  })

  # observer to ensure that inference type is the same as for ROBMED
  observeEvent(input$type_ROBMED, {
    values$type <- input$type_ROBMED
    updateNumericInput(session, inputId = "type_OLS_boot",
                       value = values$type)
  })


  ## Render outputs for the 'OLS Bootstrap' tab -----

  # show summary for the OLS bootstrap in main panel
  output$summary_OLS_boot_header <- renderUI({
    req(commands$OLS_boot)
    h3("Model and test summaries")
  })
  output$summary_OLS_boot <- renderPrint({
    req(commands$OLS_boot)
    eval(commands$OLS_boot$summary, envir = session_env)
  })


  ## update inputs for the 'Export' tab

  # create UI element with buttons to generate/download files and input for
  # the file type for the table
  output$select_file_type_table <- renderUI({
    # check which methods have been run
    have_ROBMED <- isTruthy(commands$ROBMED)
    have_OLS_boot <- isTruthy(commands$OLS_boot)
    # check whether to show inputs for button and table
    if (!have_ROBMED && !have_OLS_boot) {
      error_text("Run ROBMED or the OLS bootstrap in the respective tabs.")
    } else if (have_ROBMED && have_OLS_boot &&
               !identical(used_inputs$ROBMED, used_inputs$OLS_boot)) {
      error_text("ROBMED and the OLS bootstrap use different options.",
                 "Re-run the methods with the same options.")
    } else {
      # create inputs
      tagList(
        # buttons to generate and download files
        actionButton("generate_files", "Generate and preview files"),
        uiOutput("button_download_files"),
        # inputs for table options
        h3("Table"),
        checkboxGroupInput("file_type_table", "File type",
                           choices = c("Microsoft Word (docx)" = "docx",
                                       "Microsoft Powerpoint (pptx)" = "pptx"),
                           selected = isolate(values$file_type_table))
      )
    }
  })

  # create UI button to download files
  output$button_download_files <- renderUI({
    # show the button only if files have been successfully
    req(values$download)
    downloadButton("download_files", "Download files")
  })

  # observer to update reactive value for file type for the diagnostic plot
  # Note that 'values$file_type_plot' is necessary to initialize the default
  # values in the checkbox group.  Checking whether 'input$file_type_plot' is
  # NULL (as we do for other inputs) doesn't work for checkbox groups since the
  # value is NULL when no checkbox is selected.  Hence it is important that the
  # observer is ignored when the value is initialized, but not when it is NULL.
  observeEvent(input$file_type_table, {
    values$file_type_table <- input$file_type_table
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # create UI element with inputs for table options
  output$options_table <- renderUI({
    # check which methods have been run
    have_ROBMED <- isTruthy(commands$ROBMED)
    have_OLS_boot <- isTruthy(commands$OLS_boot)
    have_both <- have_ROBMED && have_OLS_boot
    # show the inputs if ROBMED or the OLS bootstrap have been run (with the
    # same options) and a file type has been selected
    req(((have_ROBMED || have_OLS_boot) && !have_both) ||
          (have_both && identical(used_inputs$ROBMED, used_inputs$OLS_boot)),
        values$file_type_table)
    # use previous values as defaults (if they exist)
    default_digits <- isolate(input$digits)
    if (is.null(default_digits)) default_digits <- 3
    default_p_value <- isolate(input$p_value)
    if (is.null(default_p_value)) default_p_value <- FALSE
    # create inputs
    tagList(
      sliderInput("digits", "Number of digits after decimal point",
                  min = 2, max = 6, value = default_digits,
                  step = 1, round = TRUE, ticks = FALSE),
      checkboxInput("p_value",
                    get_label(HTML("Include <em>p</em> values for indirect effects"),
                              "(may take time to compute)"),
                    value = default_p_value)
    )
  })

  # # create UI input for orientation of the table
  # output$select_orientation <- renderUI({
  #   # show the input if both ROBMED and OLS bootstrap have been run
  #   # (with the same options)
  #   req(commands$ROBMED, commands$OLS_boot,
  #       identical(used_inputs$ROBMED, used_inputs$OLS_boot))
  #   selectInput("orientation", "Page orientation",
  #               choices = c("landscape", "portrait"),
  #               selected = isolate(input$orientation),
  #               multiple = FALSE)
  # })

  # create UI input for file type for the diagnostic plot
  output$select_file_type_plot <- renderUI({
    # show the input if ROBMED has been run, and if the OLS bootstrap has been
    # run as well the methods need to have the same options
    req(commands$ROBMED,
        !isTruthy(commands$OLS_boot) ||
          identical(used_inputs$ROBMED, used_inputs$OLS_boot))
    # header and input for file type
    tagList(
      h3("Diagnostic Plot"),
      checkboxGroupInput("file_type_plot", "File type",
                         choices = c("pdf", "png"),
                         selected = isolate(values$file_type_plot))
    )
  })

  # observer to update reactive value for file type for the diagnostic plot
  # Note that 'values$file_type_plot' is necessary to initialize the default
  # values in the checkbox group.  Checking whether 'input$file_type_plot' is
  # NULL (as we do for other inputs) doesn't work for checkbox groups since the
  # value is NULL when no checkbox is selected.  Hence it is important that the
  # observer is ignored when the value is initialized, but not when it is NULL.
  observeEvent(input$file_type_plot, {
    values$file_type_plot <- input$file_type_plot
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # create UI inputs with inputs for options for the diagnostic plot
  output$options_plot <- renderUI({
    # show the inputs if ROBMED has been run and a file type has been selected,
    # and if the OLS bootstrap has been run as well the methods need to have
    # the same options
    req(commands$ROBMED,
        !isTruthy(commands$OLS_boot) ||
          identical(used_inputs$ROBMED, used_inputs$OLS_boot),
        values$file_type_plot)
    # use previous values as defaults (if they exist)
    default_units <- isolate(values$units)
    default_width <- isolate(input$width)
    if (is.null(default_width)) default_width <- 13
    # TODO: allow default height to scale with the number of regressions
    default_height <- isolate(input$height)
    if (is.null(default_height)) default_height <- 11.5
    # create inputs
    tagList(
      fluidRow(
        column(width = 7, style = "padding-right: 5px",
               numericInput("width", "Width", value = default_width,
                            min = 0, step = 1)),
        column(width = 5, style = "padding-left: 5px",
               selectInput("unit_width", "Unit",
                           choices = c("cm", "inches" = "in"),
                           selected = default_units,
                           multiple = FALSE))
      ),
      fluidRow(
        column( width = 7, style = "padding-right: 5px",
                numericInput("height", "Height", value = default_height,
                             min = 0, step = 1)),
        column(width = 5, style = "padding-left: 5px",
               selectInput("unit_height", "Unit",
                           choices = c("cm", "inches" = "in"),
                           selected = default_units,
                           multiple = FALSE))
      )
    )
  })

  # observer to ensure that unit of height is the same as unit of width
  observeEvent(input$unit_width, {
    values$units <- input$unit_width
    updateSelectInput(session, inputId = "unit_height",
                      selected = values$units)
  })

  # observer to ensure that unit of width is the same as unit of height
  observeEvent(input$unit_height, {
    values$units <- input$unit_height
    updateSelectInput(session, inputId = "unit_width",
                      selected = values$units)
  })

  # observer for switching the units for width and height
  # (argument 'ignoreInit = TRUE' prevents that the observer is executed when
  # the reactive value is initialized so that the default values are correct)
  observeEvent(values$units, {
    if (values$units == "in") {
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

  # create UI input for resolution of the png image for the diagnostic plot
  output$select_resolution <- renderUI({
    # show the input if ROBMED has been run and a png image has been selected,
    # and if the OLS bootstrap has been run as well the methods need to have
    # the same options
    req(commands$ROBMED,
        !isTruthy(commands$OLS_boot) ||
          identical(used_inputs$ROBMED, used_inputs$OLS_boot),
        "png" %in% values$file_type_plot)
    # use previous value as default (if it exists)
    default_resolution <- isolate(input$resolution)
    if (is.null(default_resolution)) default_resolution <- 300
    # create input
    numericInput("resolution", "Resolution (pixels per inch)",
                 value = default_resolution, min = 0, step = 50)
  })

  # observer for button to generate and preview files
  observeEvent(input$generate_files, {
    ## clean up reactive values to clear output and download button
    commands$table <- NULL
    used_inputs$table <- NULL
    commands$plot <- NULL
    used_inputs$plot <- NULL
    values$download <- NULL
    ## initializations
    commands_ROBMED <- commands$ROBMED
    have_ROBMED <- isTruthy(commands_ROBMED)
    commands_OLS_boot <- commands$OLS_boot
    have_OLS_boot <- isTruthy(commands_OLS_boot)
    have_table <- isTruthy(values$file_type_table)
    have_plot <- isTruthy(values$file_type_plot)
    ## construct commands to generate and export flextable
    if (have_table) {
      # construct command to generate the flextable
      if (have_ROBMED && have_OLS_boot) {
        # orientation <- input$orientation
        command_list <- call("list", as.name("robust_boot"), as.name("ols_boot"))
        command_to_flextable <- call("to_flextable", command_list,
                                     type = used_inputs$ROBMED$type,
                                     p_value = input$p_value,
                                     # orientation = orientation,
                                     digits = input$digits)
      } else {
        # orientation <- NULL
        if (have_ROBMED) {
          object_name <- "robust_boot"
          type <- used_inputs$ROBMED$type
        } else {
          object_name <- "ols_boot"
          type <- used_inputs$OLS_boot$type
        }
        command_to_flextable <- call("to_flextable", as.name(object_name),
                                     type = type, p_value = input$p_value,
                                     digits = input$digits)
      }
      command_ft <- call("<-", as.name("ft"), command_to_flextable)
      # define file names for table
      file_table <- "table.%s"
      # construct command to export the flextable to Microsoft Word
      have_docx <- "docx" %in% values$file_type_table
      if (have_docx) {
        command_docx <- call("export_docx", as.name("ft"),
                               file = sprintf(file_table, "docx"))
      } else command_docx <- NULL
      # construct command to export the flextable to Microsoft Powerpoint
      have_pptx <- "pptx" %in% values$file_type_table
      if (have_pptx) {
        command_pptx <- call("export_pptx", as.name("ft"),
                             file = sprintf(file_table, "pptx"))
      } else command_pptx <- NULL
      # construct list of commands to generate and export the flextable
      commands_table <- list(generate = command_ft,
                             docx = command_docx,
                             pptx = command_pptx)
      attr(commands_table, "time_stamp") <- Sys.time()
      # construct list of inputs used to generate the table
      # used_inputs_table <- list(digits = input$digits,
      #                           p_value = input$p_value,
      #                           orientation = orientation)
      used_inputs_table <- list(digits = input$digits,
                                p_value = input$p_value)
      # evaluate command to generate flextable
      eval(commands_table$generate, envir = session_env)
    } else {
      have_docx <- FALSE
      have_pptx <- FALSE
      commands_table <- NULL
      used_inputs_table <- NULL
    }
    ## construct commands to generate and export diagnostic plot
    if (have_ROBMED && have_plot) {
      # extract and convert some inputs
      width <- input$width
      height <- input$height
      units <- values$units
      if (units == "cm") {
        # pdf() required width and height in inches: command looks nicer with
        # a function call for the conversion instead of the converted values
        width <- call("/", width, 2.54)
        height <- call("/", height, 2.54)
      }
      # define file names for diagnostic plot
      file_plot <- "diagnostic_plot.%s"
      # construct command for opening pdf file
      have_pdf <- "pdf" %in% values$file_type_plot
      if (have_pdf) {
        # construct command
        command_pdf <- call("pdf", file = sprintf(file_plot, "pdf"),
                            width = width, height = height)
      } else command_pdf <- NULL
      # construct command for opening png image
      have_png <- "png" %in% values$file_type_plot
      if (have_png) {
        # construct command
        resolution <- input$resolution
        command_png <- call("png", file = sprintf(file_plot, "png"),
                            width = input$width, height = input$height,
                            units = units, res = resolution)
      } else {
        resolution <- NULL
        command_png <- NULL
      }
      # construct list of commands to export the diagnostic plot
      commands_plot <- list(pdf = command_pdf,
                            png = command_png,
                            generate = call("print", as.name("p")),
                            close = call("dev.off"))
      attr(commands_plot, "time_stamp") <- Sys.time()
      # construct list with used inputs and plot dimensions for preview
      used_inputs_plot <- list(file_type = values$file_type_plot,
                               units = values$units,
                               width = input$width,
                               height = input$height,
                               resolution = resolution)
    } else {
      have_pdf <- FALSE
      have_png <- FALSE
      commands_plot <- NULL
      used_inputs_plot <- NULL
    }
    ## generate files for output
    # switch to temporary directory to save files
    temporary_directory <- tempdir()
    working_directory <- setwd(temporary_directory)
    on.exit(setwd(working_directory))
    # save data set to RData file
    eval(commands$data$save, envir = session_env)
    # if requested, save table to Microsoft Word document
    if (have_docx) eval(commands_table$docx, envir = session_env)
    # if requested, save table to Microsoft Powerpoint document
    if (have_pptx) eval(commands_table$pptx, envir = session_env)
    # if requested, save diagnostic plot to pdf file
    if (have_pdf) {
      eval(commands_plot$pdf, envir = session_env)
      eval(commands_plot$generate, envir = session_env)
      eval(commands_plot$close, envir = session_env)
    }
    # if requested, save diagnostic plot to png image
    if (have_png) {
      eval(commands_plot$png, envir = session_env)
      eval(commands_plot$generate, envir = session_env)
      eval(commands_plot$close, envir = session_env)
    }
    ## generate replication script
    # construct initial lines to load packages and data
    # TODO: add time stamp and sessionInfo() in the initial comment
    lines_initial <- c(
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
      deparse_command(commands$data$load)
    )
    # construct lines to apply ROBMED and to show or export diagnostic plot
    if (have_ROBMED) {
      # construct lines to apply ROBMED
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
        deparse_command(commands_ROBMED$mediation, limit = NA),
        "# show a summary of the results",
        deparse_command(commands_ROBMED$summary),
        "# generate the diagnostic plot",
        if (have_plot) deparse_command(commands_ROBMED$assign_plot, split = " + ")
        else deparse_command(commands_ROBMED$generate_plot, split = " + ")
      )
      # construct lines to export diagnostic plot
      lines_plot <- c(
        if (have_pdf) {
          c("",
            "# generate a pdf file containing the diagnostic plot for ROBMED",
            deparse_command(commands_plot$pdf),
            deparse_command(commands_plot$generate),
            deparse_command(commands_plot$close))
        },
        if (have_png) {
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
        deparse_command(commands_OLS_boot$mediation, limit = NA),
        "# show a summary of the results",
        deparse_command(commands_OLS_boot$summary)
      )
    } else lines_OLS_boot <- NULL
    # combine lines for ROBMED and the OLS bootstrap
    # (depending on which was logged first)
    if (have_ROBMED && have_OLS_boot) {
      # both methods have been run
      time_stamp_ROBMED <- attr(commands_ROBMED, "time_stamp")
      time_stamp_OLS_boot <- attr(commands_OLS_boot, "time_stamp")
      if (time_stamp_ROBMED < time_stamp_OLS_boot) {
        lines_methods <- c(lines_ROBMED, lines_OLS_boot)
      } else lines_methods <- c(lines_OLS_boot, lines_ROBMED)
    } else {
      # at least one of lines_ROBMED and lines_OLS_boot is NULL
      lines_methods <- c(lines_ROBMED, lines_OLS_boot)
    }
    # construct vector containing lines to export table
    if (have_docx && have_pptx) {
      lines_table <- c(
        "",
        "# export a table of results to Microsoft Word and Powerpoint",
        deparse_command(commands_table$generate),
        deparse_command(commands_table$docx),
        deparse_command(commands_table$pptx)
      )
    } else if (have_docx) {
      lines_table <- c("",
                       "# export a table of results to Microsoft Word",
                       deparse_command(commands_table$generate),
                       deparse_command(commands_table$docx))
    } else if (have_pptx) {
      lines_table <- c("",
                       "# export a table of results to Microsoft Powerpoint",
                       deparse_command(commands_table$generate),
                       deparse_command(commands_table$pptx))
    } else lines_table <- NULL
    # construct vector containing lines of replication script
    replication_code <- c(lines_initial, lines_methods, lines_table, lines_plot)
    # write lines of replication script to file
    script_file <- "replication_script.R"
    writeLines(replication_code, con = script_file)
    ## finalize everything
    # switch back to working directory
    setwd(working_directory)
    # update reactive values to trigger preview
    commands$table <- commands_table
    used_inputs$table <- used_inputs_table
    commands$plot <- commands_plot
    used_inputs$plot <- used_inputs_plot
    # update reactive value to enable download button
    values$download <- list(path = temporary_directory,
                            files = c(commands$data$save$file,
                                      if (have_docx) commands_table$docx$file,
                                      if (have_pptx) commands_table$pptx$file,
                                      if (have_pdf) commands_plot$pdf$file,
                                      if (have_png) commands_plot$png$file,
                                      script_file))
  })


  ## Render outputs for the 'Export' tab -----

  # download zip archive of all files (note that the download button is only
  # displayed if the reactive value 'values$download' exists)
  output$download_files <- downloadHandler(
    filename = function() {
      date <- format(Sys.Date(), "%Y-%m-%d")
      sprintf("mediation_analysis_%s_%s.zip", values$df_name, date)
    },
    content = function(file) {
      # switch to temporary directory where files are saved
      working_directory <- setwd(values$download$path)
      on.exit(setwd(working_directory))
      # download zip file containing the files
      zip(zipfile = file, files = values$download$files)
    },
    contentType = "application/zip"
  )

  # show preview of table in main panel
  output$table_preview_header <- renderUI({
    req(commands$table)
    h3("File preview of table")
  })
  output$table_preview <- renderUI({
    req(commands$table)
    flextable::htmltools_value(get("ft", envir = session_env))
  })

  # show diagnostic plot for ROBMED in main panel
  output$plot_preview_header <- renderUI({
    req(used_inputs$plot)
    tagList(
      h3("File preview of diagnostic plot"),
      help_text("The size shown here depends on the resolution of the browser",
                "and may differ from the size of the file to be generated.")
    )
  })
  output$plot_preview <- renderPlot({
    get("p", envir = session_env)
  }, width = function() {
    req(used_inputs$plot)
    width <- used_inputs$plot$width
    if (used_inputs$plot$units == "cm") width <- width / 2.54
    width * 125
  }, height = function() {
    req(used_inputs$plot)
    height <- used_inputs$plot$height
    if (used_inputs$plot$units == "cm") height <- height / 2.54
    height * 125
  }, res = 125)


  ## Render outputs for the 'Info' tab -----

  # render information on software versions
  output$version_info <- renderUI({
    p("You are using", strong("R"), paste0("version ", R_version, ", package"),
      strong("robmed"), paste0( "version ", robmed_version, ", and package"),
      strong("robmedExtra"), paste0("version ", robmedExtra_version, "."))
  })

  # render citation information
  output$citation_info <- renderUI({
    tagList(
      p("To cite the robust bootstrap test ROBMED, please use:"),
      references$ROBMED,
      p("To cite our software, please use:"),
      references$robmed,
      references$robmedExtra
    )
  })

  # download references in selected format
  output$download_references <- downloadHandler(
    filename = function() {
      extension <- switch(input$citation_format,
                          "EndNote" = "enw",
                          "BibTeX" = "bib")
      paste("ROBMED", extension, sep = ".")
    },
    content = function(file) {
      # write lines to file of selected reference format
      references <- get_references(input$citation_format)
      writeLines(references, con = file)
    },
    contentType = "text/plain"
  )

})

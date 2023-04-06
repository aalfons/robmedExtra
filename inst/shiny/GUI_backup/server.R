#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(robmed)
library(DT)
library(officer)
library(flextable)
library(xtable)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

      session$onSessionEnded(function() { stopApp() })
      vals <- reactiveValues()

      # Initializing the to be generated script
      vals$script <- c()

      # A list of dataframes used from the existing environment for the script
      vals$used_data <- c()

      # Assigning alternative environment for safely loading RData files
      alt_env <- new.env()

      # Locally initialize robust_boot, ols_boot which will be updated later
      # using <<- . This initialization ensures that the variables are not
      # written to the user's Global Environment

      ols_boot <- NULL
      robust_boot <- NULL


      # Reactive expression to get data in dataframe
      get_data <- reactive({
        if (input$datatype == "R environment"){
            if (is.null(input$dfname)) {
              final_df <- data.frame()
            } else {
              final_df <- as.data.frame(get(df_name(), envir = .GlobalEnv))
            }
          } else if (input$datatype == "RData file") {
            if (is.null(input$rdata_dfname)) {
              # return an empty dataframe
              final_df <- data.frame()
            } else {
              # Find the correct dataframe in the environment where it has been
              # loaded
              final_df <- as.data.frame(get(df_name(), envir = alt_env))
            }
          }

        colnames(final_df) <- make.names(colnames(final_df), unique = TRUE)
        final_df
      })

      # Filters only numeric values in the selected dataframe
      numeric_data <- reactive({
        df <- get_data()
        Filter(is.numeric, df)
      })

      # Observers ensuring that the rng version for ols and robmed is the same
      observe({
        updateTextInput(session, inputId = "rng_version_ols",
                        value = input$rng_version_robust)
        RNGversion(input$rng_version_robust)
      })

      observe({
        updateTextInput(session, inputId = "rng_version_robust",
                        value = input$rng_version_ols)
        RNGversion(input$rng_version_ols)
      })

      # reactive value for the name of the dataframe
      df_name <- reactive({
        ifelse(input$datatype == "RData file", input$rdata_dfname,
                        input$dfname)
      })

      # Creates boot_test_mediation object
      robust_bootstrap_test <- eventReactive(input$runRobust,{
        if (length(input$Mediators) > 1) {
          validate(need(input$Modeltype,
          "Please select a type of mediation model in the MODEL tab."))
        }

        vals$script <- c(vals$script, "",
                           "# Perform robust bootstrap test")

        # Save the name and dimension of the dataframe in the list of dataframes
        if(input$datatype == "R environment") {
          vals$used_data <- c(vals$used_data,
                              paste0(df_name(),"(",
                                     paste(dim(get_data()), collapse = ","),
                                     ")")
                              )
        } else {
          # write the line that loads the rdatafile in the script
          vals$script <- c(vals$script, paste0("load(",input$rdatafile$name, ")"))
        }

        # Command to create the control that sets options
        command_control <- call("reg_control", efficiency = input$MM_eff,
                                max_iterations = input$max_iter)

        # call to assign the dataframe to a name object
        command_df_name <- call("<-", as.name(df_name()), get_data())

        # Command to create the test_mediation object
        command_robust_test <- call("test_mediation",
                                    object = as.name(df_name()),
                                    x = input$Explanatory,
                                    y = input$Response,
                                    m = input$Mediators,
                                    robust = TRUE,
                                    level = input$ConfidenceOLS,
                                    R = input$boot_samplesROBMED,
                                    control = as.name("ctrl")
                                    )
        # Command for setting seed (may not be used)
        command_seed <- call("set.seed", input$seedROBMED)


        if (!is.null(input$Covariates)) {
          command_robust_test$covariates <- input$Covariates
        }

        # Check if mediation model is specified.
        # might not be specified in case of simple model

        if (!is.null(input$Modeltype)) {
          command_robust_test$model <- input$Modeltype
        }

        if (!is.na(input$seedROBMED)) {
          eval(command_seed)
          vals$script <- c(vals$script, as.character(as.expression(command_seed)))
        }

        # Chaining together the commands and assigning the resulting object to a
        # name object called robust_boot
        command_complete <- call("<-", as.name("robust_boot"), command_robust_test)

        # assigning the control object to the name ctrl
        command_control_complete <- call("<-", as.name("ctrl"), command_control)

        # Adding all to script
        vals$script <- c(vals$script,
                         as.character(as.expression(command_control_complete)),
                         as.character(as.expression(command_complete)),
                         "summary(robust_boot)")

        # evaluate the command to assign the dataframe to a name
        #eval(command_df_name)
        # Evaluating the command to assign the control variable to the name "ctrl"
        eval(command_control_complete)

        # Evaluating to create the test_mediation object with all the necessary
        # input variables as defined before
        eval(command_complete)
      })

      ols_bootstrap_test <- eventReactive(input$runOLS, {
        # Check if mediation model has been specified if needed
        if (length(input$Mediators) > 1) {
          validate(need(input$Modeltype,
                        "Please select a type of mediation model in the MODEL tab."))
         }

         vals$script <- c(vals$script, "", "# Perform OLS bootstrap test")

         # takes care of entering the dataframes in the script
         if(input$datatype == "R environment") {
           vals$used_data <- c(vals$used_data,
                               paste0(df_name(),"(",
                                     paste(dim(get_data()), collapse = ","),
                                     ")")
                               )

         } else {
           vals$script <- c(vals$script, paste0("load(", df_name(), ")"))
         }

         command_seed <- call("set.seed", input$seedOLS)
         command_ols_test <- call("test_mediation",
                                  as.name(df_name()),
                                  x = input$Explanatory,
                                  y = input$Response,
                                  m = input$Mediators,
                                  robust = FALSE,
                                  level = input$ConfidenceOLS,
                                  R = input$boot_samplesOLS)

         if (!is.null(input$Covariates)) {
           command_robust_test$covariates <- input$Covariates
         }

         if (!is.null(input$Modeltype)) {
           command_ols_test$model <- input$Modeltype
         }

         if (!is.na(input$seedOLS)) {
           eval(command_seed)
           vals$script <- c(vals$script,
                            as.character(as.expression(command_seed)))
         }

         command_complete <- call("<-", as.name("ols_boot"), command_ols_test)

         vals$script <- c(vals$script,
                                as.character(as.expression(command_complete)),
                          "summary(ols_boot)")

         eval(command_complete)
       })

      # Renders plot of expected vs empirical weights
      create_plot <- reactive({
          robust_boot_simple <- robust_bootstrap_test()
          summary_simple <- summary(robust_boot_simple)
          summary_simple$plot
      })

      output$plot_weights <- renderPlot({create_plot()})

      output$downloadPlot <- downloadHandler(
        filename = function() {
          if (input$plot_format == "png") {
            ext <- "png"
          } else if (input$plot_format == "pdf") {
            ext <- "pdf"
          }
          paste0(Sys.Date(),"_",df_name(),"_","diagnostic_plot.", ext)
        },
        content = function(file) {
          # Gives popup with 'loading' while downloading
          showModal(modalDialog("Loading", footer = NULL))
          on.exit(removeModal())

          if (input$plot_format == "png") {
            command_plot <- call("png",file = file, width = input$width_plot,
                                 height = input$height_plot,
                                 units = input$plot_units,
                                 res = input$plot_resolution)
          } else if (input$plot_format == "pdf") {

            if (input$plot_units == "cm") {

              # Convert to inch for pdf which only takes inch
              command_plot <- call("pdf",file = file,
                                   width = input$width_plot/2.54,
                                   height = input$height_plot/2.54)
            } else if (input$plot_units == "in") {
              command_plot <- call("pdf", file = file, width = input$width_plot,
                                   height = input$height_plot)
            }
          }
          robust_boot <- robust_bootstrap_test()
          command_plot1 <- call("summary", as.name("robust_boot"))
          command_plot2 <- call("$", command_plot1, as.name("plot"))
          command_assign_plot <- call("<-", as.name("diagnostic_plot"),
                                      command_plot2)

          command_print <- call("print", as.name("diagnostic_plot"))

          vals$script <- c(vals$script,
                           "",
                           as.character(as.expression(command_plot)),
                           as.character(as.expression(command_assign_plot)),
                           as.character(as.expression(command_print)))

          eval(command_plot)
          eval(command_assign_plot)
          eval(command_print)
          dev.off()
        },
        contentType = "image/png"
      )


    # Renders the summary text for ROBMED
    output$summary <- renderPrint({
      robust_boot_simple <- robust_bootstrap_test()
      summary(robust_boot_simple)
      })

    # Renders the summary text for OLS bootstrap
    output$summaryOLS <- renderPrint({
      robust_boot_simple <- ols_bootstrap_test()
      summary(robust_boot_simple)
    })


    # Below observers are used to exclude any already chosen variables from the
    # displayed options

    observe({
      isolate(selectedInput <- input$Explanatory)
      updateSelectInput(session, inputId = "Explanatory",
                        choices = setdiff(colnames(get_data()),
                                          c(input$Mediators,
                                            input$Response,
                                            input$Covariates)),
                        selected = selectedInput)

    })

    observe({
      isolate(selectedInput <- input$Mediators)

      updateSelectInput(session, inputId = "Mediators",
                        choices = setdiff(colnames(numeric_data()),
                                          c(input$Explanatory,
                                            input$Response,
                                            input$Covariates)),
                        selected = selectedInput)
    })

    observe({
      isolate(selectedInput <- input$Response)

      updateSelectInput(session, inputId = "Response",
                        choices = setdiff(colnames(numeric_data()),
                                          c(input$Explanatory,
                                            input$Mediators,
                                            input$Covariates)),
                        selected = selectedInput)
    })

    observe({
      isolate(selectedInput <- input$Covariates)

      updateSelectInput(session, inputId = "Covariates",
                        choices = setdiff(colnames(get_data()),
                                          c(input$Explanatory,
                                            input$Response,
                                            input$Mediators)),
                        selected = selectedInput)
    })

    # Generates the UI that allows users to select variables of the data
    output$selectExplanatory <- renderUI({
      choices <- colnames(get_data())
      selectInput(inputId="Explanatory", label = "Independent variable(s)",
                  choices = choices, multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      # Mediator only allows for numeric values
      choices <- colnames(numeric_data())
      selectInput(inputId="Mediators",
                  label= p("Mediator(s)",
                           span("(Numeric)", style = "color: 	#A0A0A0")),
                  choices = choices, multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      # Response only allows for numeric values
      choices <- colnames(numeric_data())
      selectInput(inputId = "Response",
                  label = p("Dependent variable",
                            span("(Numeric)", style = "color: 	#A0A0A0")),
                  choices = c("",choices), multiple = FALSE, selected = NULL)
    })

    output$selectControls <- renderUI({
      isolate(self <- input$Covariates)

      choices <- colnames(get_data())
      selectInput(inputId="Covariates",
                  label = p("Covariate(s)",
                            span("(Optional)", style = "color: 	#A0A0A0")),
                  choices = choices, multiple = TRUE)
    })

    # Creates input UI for type of data
    output$dataframechoice <- renderUI({

      if (input$datatype == "R environment") {
        if (!any(as.logical(lapply(.GlobalEnv, is.data.frame)))){
          helpText("There are no data frames in your R environment.")
        } else {
          # There is at least one dataframe in the environment
          mydataframes <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
          selectInput("dfname", "DataFrame from R environment",
                      choices = mydataframes, multiple = FALSE)
        }
      }
      else if (input$datatype == "RData file") {
        fileInput("rdatafile", "RData File",
                  accept = c(".RData"))
      }
    })

    # Creates input UI for the dataframe in an RData file
    output$rdatafile_dataframes <- renderUI({
      if (input$datatype == "RData file") {
        req(input$rdatafile)

        # loading the RData file into the alternative environment
        load(input$rdatafile$datapath, envir = alt_env)

        # Select names of all dataframes in the environment after loading the
        # selected RData file
        dataframe_names <- names(which(unlist(eapply(env = alt_env,
                                                FUN = is.data.frame))))

        selectInput("rdata_dfname", "DataFrame",
                    choices = dataframe_names)
      }
    })

    output$data_table <-  DT::renderDataTable({get_data()})

    output$robmedversion <- renderText({
      paste("robmed Package version: ", toString(packageVersion("robmed")))})

    output$downloadbuttonplot <- renderUI({
      req(input$runRobust)
      downloadButton("downloadPlot", "Download Plot")
    })

    output$downloadbuttonscript <- renderUI({
      req(input$runRobust)
      downloadButton("downloadScript", "Generate R script")
    })

    # Two below observers are used to maintain equal confidence level for both
    # OLS and ROBMED tabs
    observeEvent(input$ConfidenceOLS, {
      updateSliderInput(session, "ConfidenceROBMED",
                        value = input$ConfidenceOLS)
    })

    observeEvent(input$ConfidenceROBMED, {
      updateSliderInput(session, "ConfidenceOLS",
                        value = input$ConfidenceROBMED)
    })

    # Two below observers are used to maintain equal seeds for both OLS and
    # robmed tabs
    observe({
      updateNumericInput(session, "seedOLS", value = input$seedROBMED)
      })

    observe({
      updateNumericInput(session, "seedROBMED", value = input$seedOLS)
    })

    output$downloadScript <- downloadHandler(

      filename = function() {
        paste(Sys.Date(), df_name(), "script.R", sep = "_")
      },
      content = function(file) {
        file.create(file)

        # Text that tells the user to load data
        comment_text <- paste0("# Enter the command to load the following ",
                          "datasets into the environment:")

        # Adding the used dataframes to this comment
        output_data <- paste(comment_text, paste(unique(vals$used_data),
                                                 collapse = ", "))

        # Adding example loading data
        output_data <- paste0(output_data, "\n",
                             paste("# Example: if rdata file is called rdata",
                             "# and the name of the dataframe is data the right command is:",
                             "# 'load(~/rdata.RData)'", sep = "\n"))
        # Top of the script
        output_top <- paste0("# Script generated by robmedExtra on ", Sys.Date(),
        "\n\n", "# load package", "\n", 'library("robmed")')

        writeLines(c(output_top, output_data, vals$script), file)
      }
    )

    output$ui_model_type <- renderUI({
      if (length(input$Mediators) > 1) {
      selectInput("Modeltype", "Multiple mediator model:",
                  choices = c('parallel', 'serial'), selected = "parallel")
      }
    })

    output$ui_runbutton_robmed <- renderUI({
      req(input$Mediators, input$Response, input$Explanatory)
      actionButton("runRobust", "Run")
    })

    output$ui_runbutton_ols <- renderUI({
      req(input$Mediators, input$Response, input$Explanatory)
      actionButton('runOLS', 'Run')
    })

    observeEvent(input$plot_units, {
      # Set reasonable values depending on inch or cm
      if (input$plot_units == "in") {
        min_val <- 1
        max_val <- 40
        standard <- 7
      } else if (input$plot_units == "cm") {
        min_val <- 2.54
        max_val <- 101.6
        standard <- 17.78
      }

      updateSliderInput(session, inputId = "width_plot",
                        label = "Width plot",
                        min = min_val, max = max_val, value = standard)
      updateSliderInput(session, inputId = "height_plot",
                        label = "Height plot",
                        min = min_val, max = max_val, value = standard)
    })

    output$download_tables <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_", df_name(), "_", "table", "_",
               input$table_orientation, ".docx")
      },
      content = function(file) {
        showModal(modalDialog("Loading", footer = NULL))
        on.exit(removeModal())

        mediation_list <- list()

        if (isTruthy(input$models_tables)) {
          # If the checkbox for selecting models exists check which models to do
          contains_robust <- "ROBMED" %in% input$models_tables
          contains_ols <- "OLS" %in% input$models_tables
        } else {
          contains_robust <- TRUE
          contains_ols <- TRUE
        }

        # If a model is included, check whether it has been run and include name
        if (contains_robust) {
          if(isTruthy(input$runRobust)) {
            robust_boot <<- robust_bootstrap_test()
            mediation_list$robust <- as.name("robust_boot")
          }
        }

        if (contains_ols) {
          if(isTruthy(input$runOLS)) {
            ols_boot <<- ols_bootstrap_test()
            mediation_list$ols <- as.name("ols_boot")
          }
        }

        # mediation_list now contains the name(s) of the method that have been run
        # when evaluated this call assigns the list to the name models
        command_model_list <- call("<-", as.name("models"), mediation_list)

        # call to create the word table, will not be evaluated directly but
        # is included in the next call
        command_doc <- call("export_table_MSWord",
                            test_model = as.name("models"),
                            orientation = input$table_orientation,
                            p_values = as.logical(input$include_pval))

        # creates the word table and assigns this to the name document
        assign_doc <- call("<-", as.name("document"), command_doc)

        # prints the object called document in the file
        print_doc <- call("print", as.name("document"), file)

        # reflect the actions in the generated script
        vals$script <- c(vals$script,
                         "",
                         "# Saving tables in Word doc",
                         as.character(as.expression(command_model_list)),
                         as.character(as.expression(assign_doc)),
                         as.character(as.expression(print_doc)))

        # evaluate the calls in order to actually produce the output
        eval(command_model_list)
        eval(assign_doc)

        #Print the document to the proper file location
        eval(print_doc)
      }
    )

    #UI to select which models to include in the table
    # TODO: Replace ROBMED capitalized with robmed

    output$ui_checkbox_table <- renderUI({
      if (isTruthy(robust_bootstrap_test()) && isTruthy(ols_bootstrap_test())) {
        checkboxGroupInput(inputId = "models_tables",
                           label = "Models to include:",
                           choices = c("ROBMED", "OLS"),
                           selected = c("ROBMED", "OLS"))
      }
    })

    output$ui_resolution <- renderUI({
      if (input$plot_format == "png") {
        numericInput(inputId = "plot_resolution",
                     label = "Resolution of the plot (ppi)",
                     value = 300)
      }

    })

    latex_ols <- eventReactive(input$show_latex, {
      to_latex(ols_bootstrap_test())
    })

    latex_robust <- eventReactive(input$show_latex, {
      to_latex(robust_bootstrap_test())
    })

    # Copy to clipboard buttons for latex
    observeEvent(input$copy_latex_robust, {
      clipr::write_clip(content = latex_robust())

      # Add the action to the script
      command_latex_robust <- call("to_latex", robust_bootstrap_test())
      vals$script <- c(vals$script, "",
                       "# Print LaTeX code for table robust bootstrap test",
                       deparse(command_latex_robust,
                               control = c("niceNames", "useSource")))

      eval(command_latex_robust)
    })

    observeEvent(input$copy_latex_ols, {
      clipr::write_clip(content = latex_ols())
    })

    # Observers to show 'display latex' button if the model actually exists
    observe({
      if (isTruthy(robust_bootstrap_test())) {
        output$checkbox_latex_table <- renderUI({actionButton("show_latex",
                                                               "Display LaTeX table")})
      }
    })

    observe({
      if (isTruthy(ols_bootstrap_test())) {
        output$checkbox_latex_table <- renderUI({actionButton("show_latex",
                                                              "Display LaTeX table")})
      }
    })

    output$text_latex_robust <- renderText({latex_robust()})
    output$text_latex_ols <- renderText({latex_ols()})

    # Reacts to display latex button
    observeEvent(latex_ols(), {
      # Display copy button only if the corresponding test object exists

      if (isTruthy(latex_ols())) {
        output$button_latex_ols <- renderUI({actionButton("copy_latex_ols",
                                                          "Copy OLS table")})
      }
    })

    # Reacts to display latex button
    observeEvent(latex_robust(), {
      # Display copy button only if the corresponding test object exists

      if (isTruthy(latex_robust())) {
        output$button_latex_robust <- renderUI({actionButton("copy_latex_robust",
                                                             "Copy robmed table")})
      }
    })

    # Create action button for saving the dataframe to RData file
    observe({
      if (input$datatype == "R environment") {
        output$save_rdata_ui <- renderUI({downloadButton("download_data",
                                                       "Save selected dataframe",
                                                       class = "dlButton")})
      }
    })

    output$download_data <- downloadHandler(
      filename <- function(){
        paste0(Sys.Date(),"_",df_name(), ".RData")
      },
      content <- function(file) {
        df <- get_data()
        save(df, file = file)
      }
    )


    # Setting citation text
    vals$citation_normal <- paste(capture.output(citation("robmed"))[3:9],
                                  collapse = "\n")
    vals$citation_bib <- paste(capture.output(citation("robmed"))[10:20],
                                  collapse = "\n")


    output$citation_normal <- renderText({
      return(vals$citation_normal)
    })

    output$citation_bib <- renderText({
      return(vals$citation_bib)
    })

    observeEvent(input$copy_citation_normal, {
      clipr::write_clip(content = vals$citation_normal)
    })

    observeEvent(input$copy_citation_bib, {
      clipr::write_clip(content = vals$citation_bib)
    })




#' Export result table to Word
#'
#' Export the table containing results of mediation analysis to a Microsoft Word
#' document.
#'
#' @param test_model an object inheriting from class
#' \code{"\link{test_mediation}"} or a list of objects of that class.
#' of object
#'
#' @param digits a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#'
#' @param orientation the  orientation in which the table is displayed in the
#' document. When set to portrait, the tables are displayed
#' underneath each other. When set to landscape, the tables are displayed side
#' by side.
#'
#'
#' @return An object of class \code{"\link{rdocx}"}, containing a table of the
#' results of the provided objects.
#'
#'
#' @author Vincent Drenth
#'
#' @examples
#' data("BSG2014")
#'
#' boot_simple <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014)
#'
#' table <- export_table_MSWord(boot_simple)
#' print(table, target = "filename.docx")
#'
#' @export
#'
export_table_MSWord <- function(test_model, ...) {
  UseMethod("export_table_MSWord")
}

# function for name object to avoid bugs in the ols_bootstrap_test and
# robust_bootstrap_test which calls this function on name objects
export_table_MSWord.name <- function(test_model, digits = 4,
                                     orientation = c("landscape", "portrait"),
                                     filename = NULL, p_values = T,
                                     ...) {
  tryCatch({model <- get(x = test_model)},
           error = function(cond) {
             message(paste("Error in export_table_MSWord.name",
                           "No object with name", test_model))
             return(NULL)
           }
  )
  return(export_table_MSWord(model, digits = digits, orientation = orientation,
                             filename = filename, p_values = p_values))
}

# Takes two test objects and merges them into 1 flextable, expanding over columns
merged_flextable <- function(test1, test2, digits = 4, p_values = T) {
  data1 <- prep_data_table(test1, digits = digits, p_values = p_values)
  data2 <- prep_data_table(test2, digits = digits, p_values = p_values)

  df1 <- data1[[1]]
  df2 <- data2[[1]]
  hline_paths <- data1[[2]] - 1

  merged_df <- data.frame(df1, df2[,-1],
                          check.names = FALSE)

  # Work around to prevent duplicate col keys which is not normally allowed
  for(i in c(6:9)) {
    colnames(merged_df)[i] <- paste0(colnames(merged_df)[i], "\r")
  }

  # Row index of the start of the indirect effects
  start_merge <- which(merged_df[,1] == "Indirect Effects")

  # All indices of indirect effects over which we should merge
  indirect_range <- c(start_merge : nrow(df1))

  if (p_values) {
    # Columns that should be merged
    j_range_left <- c(3:4)
    j_range_right <- c(7:8)
  } else {
    j_range_left <- c(3:5)
    j_range_right <- c(7:9)

    # Set p-values to empty string
    merged_df[start_merge, c(5, 9)] <- ""
  }

  merged_ft <- flextable(merged_df)

  for (index in indirect_range) {
    merged_ft <- merge_at(merged_ft, i = index, j = j_range_left)
    merged_ft <- merge_at(merged_ft, i = index, j = j_range_right)
  }

  # Changing cosmetics

  merged_ft <- autofit(merged_ft)
  merged_ft <- flextable::align(merged_ft, align = "center", part = "all")
  merged_ft <- theme_booktabs(merged_ft, bold_header = TRUE)
  merged_ft <- bold(merged_ft, i = start_merge, bold = TRUE)

  merged_ft <- add_header_row(merged_ft,
                              top = TRUE,
                              values = c("",get_method_robmed(test1),
                                         get_method_robmed(test2)),
                       colwidths = c(1,4,4))

  merged_ft <- hline(merged_ft, border = NULL, part = "body")
  merged_ft <- flextable::align(merged_ft, part = "body", align = "center")
  merged_ft <- flextable::align(merged_ft,i = 1, part = "body", align = "left")

  return(merged_ft)
}

#' @export
#'
export_table_MSWord.list <- function(test_model, digits = 4,
                                     orientation = c("landscape", "portrait"),
                                     filename = NULL, p_values = TRUE,
                                     ...) {
  doc <- officer::read_docx()

  if (orientation == "landscape") {
    tables <- to_flextable(test_model = test_model, digits = digits,
                            merged = TRUE, p_values = p_values)
    doc <- officer::body_end_section_landscape(doc)

    for(table in tables) {
      doc <- flextable::body_add_flextable(doc, table)
    }
    doc <- officer::body_end_section_landscape(doc)

  } else if (orientation == "portrait") {
      count_page <- 0
      tables <- to_flextable(test_model = test_model, digits = digits,
                              merged = FALSE, p_values = p_values)

      for (table in tables) {
        doc <- flextable::body_add_flextable(doc, table)
        doc <- body_add_par(doc, value = "")

        count_page <- count_page + 1

        if (count_page %% 2 == 0) {
        doc <- officer::body_add_break(doc, "after")
        }
      }
  }

  if (!is.null(filename)) {
    print(doc, filename)
  } else {
    return(doc)
  }
}

#'@export
export_table_MSWord.test_mediation <- function(test_model, digits = 4,
                                               filename = NULL, p_values = T,
                                               ...) {
  table <- to_flextable(test_model = test_model, digits = digits,
                        p_values = p_values)

  doc <- read_docx()
  doc <- body_add_flextable(doc, table)

  if (!is.null(filename)) {
    print(doc, filename)
  } else {
    return(doc)
  }
}

#' Generate LaTeX code for table
#'
#' Generates LaTeX code that displays a table containing results of mediation
#' analyis.
#'
#' @param test_model an object inheriting from class
#' \code{"\link{test_mediation}"} or a list of objects of that class.
#' @param digits a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#' @param data dataframe containing the data of the direct and indirect effects
#' as would be produced by prep_data_table
#'
#' @return A character object containing the latex code that produces a table of
#' the results of the test_mediation object.
#'
#' @example
#' data("BSG2014")
#'
#' boot_robust <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014)
#' to_latex(boot_robust)
#'
#' @importFrom xtable xtable print.xtable
#'
#' @export
to_latex <- function(test_model, digits = 4, data = NULL, ...) {

  # Check if data is passed and if so, use that data
  if(is.null(data)) {
    table <- to_flextable(test_model = test_model, digits = digits)
    dataset <- table$body$data
  } else {
    dataset <- data
  }

  # row number where indirect effects start in the dataframe
  indirect_start <- which(dataset[,1] == "Indirect Effects")

  full_table <- xtable(dataset, align = "llcccc")

  final_table_character <- capture.output(print(full_table, booktabs = T,
                                                hline.after = (-1:nrow(dataset)),
                                                include.rownames = F,
                                                type = "latex"))

  final_table_character <- paste(final_table_character, collapse = "\n")


  # Adding multicol for column title of confidence interval in indirect effects
  final_table_character <- gsub(pattern = "(\\(-[^)]*\\))",
                                replacement = paste0("\\\\multicolumn{2}{c}{",
                                                     "\\1", "}"),
                                x = final_table_character)

  # Adding multicol to insert the confidence interval
  final_table_character <- gsub(pattern = "(Confidence Interval &  )",
                                replacement = "\\\\multicolumn{2}{c}{Confidence Interval}",
                                x = final_table_character)

  # Remove trailing empty cell caused by 'merging' the two columns
  final_table_character <- gsub(pattern = "&  &",
                                replacement = "&",
                                x = final_table_character)

  return(final_table_character)
}


#' Create flextable from mediation test
#'
#' Creates a flextable from a test_mediation object
#'
#' @param test_model an object inheriting from class
#' \code{"\link{test_mediation}"} or a list of objects of that class.
#' of object
#'
#' @param digits a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#'
#' @param merged boolean that determines whether the flextables are merged to show
#' the results of two tests in one table (except possibly the last one) or
#' seperate. Only used when test_model
#' is a list.
#'
#' @param p_values boolean that indicates whether the p-values for indirect
#' effects should be included in the flextable or not.
#' Default is to include the p-values
#'
#' @return An object of class \code{"\link{flextable}"} or a list of objects of
#' this class.
#'
#' @importFrom flextable theme_booktabs align bold add_header_row autofit
#' merge_at flextable add_footer_row hline
#'
#' @author Vincent Drenth
#'
#' @examples
#' data("BSG2014")
#'
#' boot_robust <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014)
#'
#' ft <- to_flextable(boot_robust)
#'
#' boot_ols <- test_mediation(TeamCommitment ~
#'                                 m(TaskConflict) +
#'                                   ValueDiversity,
#'                               data = BSG2014,
#'                               robust = FALSE)
#'
#' flextables_list <- to_flextable(list(boot_robust, boot_ols))
#'
#'
#' @export

to_flextable <- function(test_model, ...) {
  UseMethod("to_flextable")
}


#' @export
#' @method to_flextable test_mediation

to_flextable.test_mediation <- function(test_model, digits = 4, p_values = T) {

  tables_paths <- prep_data_table(test_model = test_model, digits = digits,
                                  p_values = p_values)

  df_stacked <- tables_paths[[1]]
  ft <- flextable(df_stacked)

  start_merge <- which(df_stacked[,1] == "Indirect Effects")

  # Row indices where the table should have a horizontal line
  path_values <- c(tables_paths[[2]] - 1, start_merge, start_merge - 1)

  # Merge the third and fourth column for the indirect effects to create one
  # column for the confidence interval
  indirect_range <- c(start_merge:(nrow(df_stacked)))

  if (p_values) {
    merge_range <- c(3:4)
  } else {
    merge_range <- c(3:5)
  }

  for (index in indirect_range) {
    ft <- merge_at(ft, i = index, j = merge_range)
  }

  # Changing cosmetics
  ft <- flextable::theme_booktabs(ft, bold_header = TRUE)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, align = "left", j = 1, part = "all")
  ft <- flextable::bold(ft, i = start_merge, bold = T)
  ft <- flextable::add_header_row(ft, top = TRUE,
                                  values = c(get_method_robmed(test_model)),
                                  colwidths = c(5))
  ft <- hline(ft, i = path_values, border = NULL, part = "body")


  for (row in c(1:(start_merge - 1))) {
    # Adding subscript to the effect's letter

    orig_text <- df_stacked[row,1]
    text_list <- strsplit(orig_text, " ")[[1]]

    effect <- paste(text_list[1:3], collapse = " ")
    letter_number <- strsplit(gsub("[()]", "", text_list[4]), "")[[1]]

    if (letter_number[2] == "'") {
      letter = paste0(letter_number[1:2], collapse = "")
      number = paste0(letter_number[3:length(letter_number)], collapse = "")
    } else {
      letter = letter_number[1]
      number = paste0(letter_number[2:length(letter_number)], collapse = "")
    }

    effect_arrow <- sub(pattern = "->", replacement = "\U2192", x = effect)

    new_text <- as_paragraph(as_chunk(effect_arrow), as_chunk(" ("),
                             as_chunk(letter),
                             as_sub(number),
                             as_chunk(")"))

    # Add text with subscript
    ft <- flextable::compose(ft, i = row, j = 1, value = new_text)
  }

  # Adding arrows for the indirect effects as well:
  for (row in c((start_merge + 1):nrow(df_stacked))) {
    original_text <- df_stacked[row, 1]
    new_text <- sub(pattern = "->", replacement = "\U2192", x = original_text)

    # Set character in chunk so that it can be put into flextable
    new_text <- as_paragraph(as_chunk(new_text))
    ft <- flextable::compose(ft, i = row, j = 1, value = new_text)
  }

  ft <- autofit(ft)

  return(ft)
}

# Helper function used so that flextable can be used on name objects
# Needed because the calls in robust_bootstrap_test and ols_bootstrap_test are
# on name objects
to_flextable.name <- function(test_model, digits = 4,
                              p_values = T, ...) {
  tryCatch({model <- get(x = test_model)},
           error = function(cond) {
             message(paste("No object with name", test_model))
             return(NA)
           }
  )
  return(to_flextable(model, digits = digits, p_values = p_values))
}

#' @export
#' @method to_flextable list
to_flextable.list <- function(test_model, digits = 4, merged = F,
                              p_values = TRUE, ...) {
  result <- list()
  if(merged) {
    for (index in seq(1, ceiling((length(test_model) + 1)/2), 2)) {
      test_model_left <- test_model[[index]]
      test_model_right <- test_model[[index + 1]]

      ft <- merged_flextable(test_model_left, test_model_right)
      result <- append(result, list(ft))
    }

    if (length(test_model) %% 2 == 1) {
      ft <- to_flextable(test_model[[length(test_model)]], p_values = p_values)
      result <- append(result, list(ft))
    }

  } else {
    # Seperate tables for each method
    for(test in test_model) {
      ft <- to_flextable(test, digits = digits, p_values = p_values)
      result <- append(result, list(ft))
    }
  }

  return(result)
}

# Creates a single dataframe that contains the data needed to create the table
# The 4th column contains NA for all rows corresponding to the indirect effects
# as the 3rd and 4th column will be later merged to create the column for
# the Confidence Interval

prep_data_table <- function(test_model, digits = 4, p_values = T) {
  sm <- summary(test_model)$summary

  if (test_model$fit$model == "serial") {
    if (length(sm$m) == 1) {
      directrows <- 3 * (length(sm$x)) + 1
      indirectrows <- length(sm$x)

    } else if (length(sm$m) == 2) {
      # to generate table for 2 serial mediators
      directrows <- 4 * length(sm$x) + 3
      indirectrows <- 3 * length(sm$x)
    } else if (length(sm$m) == 3) {
      # Table for 3 serial mediators
      directrows <- 4 * length(sm$x) + 5 + 2
      indirectrows <- (3 + 3 + 1) * length(sm$x)
    }
  } else {
    #Model is parallel
    rows = 2 * (length(sm$x) * length(sm$m)) + 2 * length(sm$x) + length(sm$m)
    indirectrows = length(sm$x) * length(sm$m)
    directrows = rows - indirectrows
  }

  # Create dataframe for the direct effects
  df_dir <- data.frame(matrix(NA, nrow = directrows, ncol = 5))

  row <- 1
  for (med in sm$m) {
    if (length(sm$m) > 1){
      coefs_a <- sm$fit_mx[med][[1]]$coefficients
    } else {
      coefs_a <- sm$fit_mx$coefficients
    }

    #Add a paths
    for (reg in sm$x) {
      df_dir[row, 1] <- paste(reg,"->", med, paste0("(a", row, ")"))
      df_dir[row, 2:5] <- coefs_a[reg, 2:5]
      row <- row + 1
    }
  }
  a_paths <- row

  coefs_b <- sm$fit_ymx$coefficients
  for (med in sm$m) {
    #Add b paths
    df_dir[row, 1] <- paste(med ,"->" , sm$y,
                            paste0("(b", row - a_paths + 1, ")"))
    df_dir[row, 2:5] <- coefs_b[med, 2:5]
    row <- row + 1
  }
  b_paths <- row

  # Add c path (Direct effect)
  for (reg in sm$x){
    df_dir[row, 1] <- paste(reg,'->', sm$y,
                            paste0("(c", row - b_paths + 1, ")"))
    df_dir[row, 2:5] <- sm$direct[reg, 2:5]
    row <- row + 1
  }
  c_paths <- row

  # Add c' path (Total effect)
  for (reg in sm$x) {
    df_dir[row, 1] <- paste(reg, "->", sm$y,
                            paste0("(c'", row - c_paths + 1, ")"))
    df_dir[row, 2:5] <- sm$total[reg, 2:5]
    row <- row + 1
  }

  if (test_model$fit$model == "serial" && length(sm$m) > 1) {

    if (length(sm$m) == 2) {
    df_dir[row, 1] <- paste(paste(sm$m, collapse = "->"), "(d)")
    df_dir[row, 2:5] <- as.numeric(sm$fit_mx[sm$m[2]][[1]]$coefficients[sm$m[1],2:5])
    row <- row + 1

    } else if (length(sm$m) == 3) {
      d_paths <- names(test_model$d)

      for (path in d_paths) {
        meds <- strsplit(path, split = "->")[[1]]
        med1 <- meds[1]
        med2 <- meds[2]

        coefs <- sm$fit_mx[med2][[1]]$coefficients[med1, 2:5]
        df_dir[row, 1] <- paste(path, "(d)")
        df_dir[row, 2:5] <- as.numeric(coefs)
        row <- row + 1
      }
    }
  }

  if (p_values) {
    pvals <- p_value(test_model, parm = "indirect")
  }
  # changing number of columns from 4 to 5. Last column should contain p-vals
  # 4th column should be empty

  #Add indirect effects (a (d) b paths)
  df_ind <- data.frame(matrix(0, nrow = indirectrows, ncol = 4))

  if (test_model$fit$model == "serial" ){
    row <- 1
    for (reg in sm$x) {
        # The effectname in this case is "reg -> med_1 -> ... -> med_n" with
        # n for 1 up to 3. The order of mediators remains the same but can skip
        # a value.

      # indirect_effects will contain the different permutations of mediators

      indirect_effects <- list()

      if (length(sm$m) >= 1) {
        # add 1 to the list
        indirect_effects <- append(indirect_effects, list(1))
      }
      if (length(sm$m) >= 2) {
        # add 2 and possible permutations of 1 and 2
        indirect_effects <- append(indirect_effects, list(c(2)))
        indirect_effects <- append(indirect_effects, list(c(1:2)))
      }
      if (length(sm$m) == 3) {
        # add 3 and possible choices from 1,2,3 where the numbers are ordered

        indirect_effects <- append(indirect_effects, list(c(3)))
        indirect_effects <- append(indirect_effects, list(c(1,3)))
        indirect_effects <- append(indirect_effects, list(c(2:3)))
        indirect_effects <- append(indirect_effects, list(c(1:3)))
      }

      for (effect in indirect_effects) {
        effectname <- paste(reg, paste0(sm$m[effect], collapse = "->"),
                            sep = "->")

        if (length(sm$x) == 1) {
          effectname <- gsub(pattern = paste0(reg,"->"),
                             replacement = "",
                             x = effectname)
        }

        df_ind[row,1] <- paste(effectname, "(Indirect)")

        if (length(sm$m) > 1) {
            df_ind[row, 2] <- test_model$indirect[effectname][[1]]

            lower <- round(test_model$ci[effectname, 1], digits)
            upper <- round(test_model$ci[effectname, 2], digits)

        } else {
          df_ind[row,2] <- test_model$indirect[reg][[1]]


          lower <- round(test_model$ci[1], digits)
          upper <- round(test_model$ci[2], digits)
        }

        df_ind[row, 3] <- paste0("("(), lower, ",",upper,")")

        if (p_values) {
          df_ind[row, 4] <- pvals[paste("Indirect", effectname, sep = "_")][[1]]
        }

        row <- row + 1
      }
    }

  } else {
    # Parallel model
    row <- 1
    for (reg in sm$x) {
      for (med in sm$m) {
        effectname <- paste(reg, "->", med, sep = "")
        df_ind[row,1] <- paste(effectname, "(Indirect)")

        if (length(sm$m) > 1) {
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[effectname, 1], digits)
            upper <- round(test_model$ci[effectname, 2], digits)
            df_ind[row, 2] <- test_model$indirect[effectname][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", effectname,
                                          sep = "_")][[1]]

          } else {
            # Only 1 explanatory variable, multiple mediators
            lower <- round(test_model$ci[med, 1], digits)
            upper <- round(test_model$ci[med, 2], digits)
            df_ind[row, 2] <- test_model$indirect[med][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", med, sep = "_")][[1]]

          }
        } else {
          # Only 1 mediator
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[reg, 1], digits)
            upper <- round(test_model$ci[reg, 2], digits)
            df_ind[row,2] <- test_model$indirect[reg][[1]]

            if (p_values) {
              df_ind[row, 4] <- pvals[paste("Indirect", reg, sep = "_")][[1]]
            }
          } else {
            # only 1 indirect effect
            df_ind[row,2] <- test_model$indirect
            lower <- round(test_model$ci[1], digits)
            upper <- round(test_model$ci[2], digits)

            if (p_values) {
              df_ind[row, 4] <- pvals["Indirect"][[1]]
            }
          }
        }
        df_ind[row, 3] <- paste("(", lower, ", ", upper,")", sep = "")
        row <- row + 1
      }
    }
  }

  # Create the table from the dataframes and round columns
  df_rounded <- data.frame(lapply(df_dir, function(y) {
    if(is.numeric(y)) round(y, digits) else y}))
  df_ind_rounded <- data.frame(lapply(df_ind, function(y) {
    if(is.numeric(y)) round(y, digits) else y}))

  colnames(df_ind_rounded) <- c("Indirect Effects", "Estimate",
                                "Confidence Interval", "p-value")

  colnames(df_rounded) <- c("Direct Effects", "Estimate", "Std. Error",
                            "z statistic", "p-value")

  dim_ind <- dim(df_ind_rounded)
  dim_dir <- dim(df_rounded)

  # Copying data from matrix with 4 columns to one with 5 columns to get the
  # same dimensions for direct and indirect effects
  df_ind_merge <- as.data.frame(matrix(NA, nrow = dim_ind[1], ncol = dim_ind[2]))
  df_ind_merge[,1:3] <- df_ind_rounded[,1:3]
  df_ind_merge[,5] <- df_ind_rounded[,4]

  df_ind_merge <- rbind(c("Indirect Effects", "Estimate",
                          "Confidence Interval","", "p-value"), df_ind_merge)

  df_dir_merge <- df_rounded

  # Set colnames equal so that rbind can be applied
  colnames(df_dir_merge) <- colnames(df_ind_merge)
  df_dir_merge <- data.frame(apply(df_dir_merge, FUN = as.character,
                                   MARGIN = 2))

  df_stacked <- rbind(df_dir_merge, df_ind_merge)
  colnames(df_stacked) <- c("Direct Effects", "Estimate", "Std. Error",
                            "z statistic", "p-value")
  return(list(df_stacked, c(a_paths, b_paths, c_paths)))
}


get_method_robmed <- function(test_model) {
  # TODO implement this function to return the proper method type for different
  # methods included in robmed based on robust, method, and family attributes
  if (test_model$fit$robust %in% c("MM", TRUE)) {
    return("robmed")
  } else {
    return("OLS Bootstrap")
  }
}
})

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
library(vroom)
library(DT)
library(officer)
library(flextable)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

      session$onSessionEnded(function() { stopApp() })

      vals <- reactiveValues()
      vals$script <- c()
      vals$usedDF <- c()

      counter <<- 1

      # Updates the script to include the current analysis that is being run
      observeEvent(input$runRobust, {
        df <- get_data()

        # Load the RData file in the R Script
        if (input$datatype != "csv") {
          if (input$datatype == "RData"){
            env <- new_env
            df_name <- input$rdata_dfname
          } else{
            env <- .GlobalEnv
            df_name <- input$dfname
          }

        } else {
          # Data is from csv file
          df_name <- input$file$name
        }

        if (!df_name %in% vals$usedDF) {
          filename <- as.character(paste(getwd(),"/",  df_name, ".Rdata", sep = ""))
          save(df, file = filename)
          vals$script <- c(vals$script, paste("load('",filename, "')", sep = ""))
          vals$usedDF <- c(vals$usedDF, df_name)
        }

        #Write test_mediation(formula, data)
        txt_controlvars <- paste("control_var <- reg_control(efficiency = ",
                                 input$MM_eff, ", max_iterations = ",
                                 input$max_iter,")")
        if (!is.na(input$seedROBMED)) {
          txt_seed <- paste("set.seed(", input$seedROBMED, ")", sep = "")
        } else {
          txt_seed <- ""
        }

        txt_formulamodel <- paste("formula_model_", counter, " <- ",
                                  paste(deparse(formula(), width.cutoff = 500),
                                        collapse=""), sep = "")

        txt_test <- paste("bootstrap_test_",counter,
                          " <- test_mediationrormula_model",counter,
                          ", data = ", df_name,", ", "robust = TRUE,",
                          "level = ", input$ConfidenceROBMED,
                          ", control = control_var)", sep = "")

        vals$script <- c(vals$script, txt_controlvars, txt_seed,
                         txt_formulamodel, txt_test, "\n")
        counter <<- counter + 1
      })

      observeEvent(input$runOLS, {
        df <- get_data()

        # Load the RData file in the R Script
        if (input$datatype != "csv") {
          if (input$datatype == "RData"){
            env = new_env
            df_name <- input$rdata_dfname
          } else{
            env = .GlobalEnv
            df_name <- input$dfname
          }

        } else {
          # Data is from csv file
          df_name <- input$file$name
        }

        if (!df_name %in% vals$usedDF) {
          filename <- paste(getwd(),"/",  df_name, ".Rdata", sep = '')
          save(df, file = filename)
          vals$script <- c(vals$script, paste("load('",filename, "')", sep = ''))
          vals$usedDF <- c(vals$usedDF, df_name)
        }

        #Write test_mediation(formula, data)
        if (!is.na(input$seedOLS)) {
          txt_seed <- paste("set.seed(", input$seedOLS, ")", sep = '')
        } else {
          txt_seed <- ""
        }

        txt_formulamodel <- paste("formula_model_", counter, "<- ",
                                  paste(deparse(formula(), width.cutoff = 500),
                                        collapse=""), sep = '')
        txt_test <- paste("bootstrap_test_",counter,
                          "<- test_mediation(formula_model", counter,
                          ", data = ", df_name,", ", "robust = FALSE,",
                          "level = ", input$ConfidenceROBMED, ")", sep = '')

        vals$script <- c(vals$script, txt_seed, txt_formulamodel, txt_test, "\n")
        counter <<- counter + 1
      })


      # Reactive expression to get data; only supports csv for now
      get_data <- reactive({

        if (input$datatype == "csv"){
          if (is.null(input$file)) {
            final_df <- data.frame()
          } else {
            ext <- tools::file_ext(input$file$name)
            final_df <- switch(ext, csv = read.csv(input$file$datapath,
                                                   check.names = TRUE),
                               tsv = read.delim(input$file$datapath, sep = "\t"),
                               shiny::validate(sprintf("Invalid file:
                                        Please upload a .csv file.\n
                                        You uploaded a %s file", ext)))
            }
          } else if (input$datatype == "Existing DataFrame"){
            if (is.null(input$dfname)) {
              final_df <- data.frame()
            } else {
              final_df <- as.data.frame(get(input$dfname, .GlobalEnv))
            }
          } else if (input$datatype == "RData") {
            if (is.null(input$rdata_dfname)) {
              final_df <- data.frame()
            } else {
              final_df <- as.data.frame(get(input$rdata_dfname, envir = new_env ))
            }
          }

        colnames(final_df) <- make.names(colnames(final_df), unique = TRUE)
        final_df
      })

      numeric_data <- reactive({
        df <- get_data()
        Filter(is.numeric, df)
      })

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


      formula <- reactive({
        req(input$Explanatory, input$Response, input$Mediators)

        explanatory <- paste(input$Explanatory, collapse = "+")

        # Choose parallel as default when no mediator type is chosen
        if (length(input$Mediators) == 1 || input$Modeltype == "" ) {
          model_type <- "parallel"
        } else {
          model_type <- input$Modeltype
        }

        mediators <- paste("m(", paste(input$Mediators, collapse = ","),
                           ', .model = "',model_type,'")', sep = '')

        controls <- ''
        if(length(input$Covariates) != 0){
          controls <- paste("+","covariates(",
                            paste(input$Covariates, collapse = ","), ")")
        }
        formula_string <- paste(input$Response, "~", mediators,
                                      "+", explanatory, controls)


        form_out <- as.formula(formula_string)

      })


      # Creates boot_test_mediation object
      robust_bootstrap_test <- eventReactive(input$runRobust,
                                              {
      df <- get_data()
      f_test <- formula()
      control_var <- reg_control(efficiency = input$MM_eff,
                                 max_iterations = input$max_iter)

      if (!is.na(input$seedROBMED)) {set.seed(input$seedROBMED)}
      robust_boot_test <- test_mediation(f_test, data = df, robust = TRUE,
                                         level = input$ConfidenceROBMED,
                                         R = input$boot_samplesROBMED,
                                         control = control_var)
      })

      ols_bootstrap_test <- eventReactive(input$runOLS,
       {
         if (!is.na(input$seedOLS)) {set.seed(input$seedOLS)}
         df <- get_data()
         f_test <- formula()
         ols_bootstrap <- test_mediation(f_test, data = df, robust = FALSE,
                                         level = input$ConfidenceOLS,
                                         R = input$boot_samplesOLS)
         ols_bootstrap
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
          paste(Sys.Date() ,"diagnosticplot.", ext, sep = '')
        },
        content = function(file) {
          if (input$plot_format == "png") {

            png(file, width = input$width_plot, height = input$height_plot,
                units = input$plot_units, res = input$plot_resolution)
          } else if (input$plot_format == "pdf") {
            if (input$plot_units == "cm") {
              # Convert to inch for pdf
              pdf(file, width = input$width_plot/2.54,
                  height = input$height_plot/2.54)
            } else if (input$plot_units == 'in') {
              pdf(file, width = input$width_plot, height = input$height_plot)
            }
          }
          print(create_plot())
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

    # Generates the UI that allows users to select variables of the uploaded data
    output$selectExplanatory <- renderUI({
      choices <- colnames(get_data())
      selectInput(inputId="Explanatory", label="Independent variable(s)",
                  choices = choices, multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      choices <- colnames(numeric_data())
      selectInput(inputId="Mediators", label="Mediator(s) (Numeric)",
                  choices = choices, multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      choices <- colnames(numeric_data())
      selectInput(inputId="Response", label= "Dependent variable (Numeric)",
                  choices = c("",choices), multiple = FALSE, selected = NULL)
    })

    output$selectControls <- renderUI({
      isolate(self <- input$Covariates)
      choices <- colnames(get_data())
      selectInput(inputId="Covariates", label = "Covariate(s) (Optional)",
                  choices = choices, multiple = TRUE)
    })

    # Creates input UI for type of data
    output$dataframechoice <- renderUI({

      if (input$datatype == "Existing DataFrame") {
        if (is.null(unlist(eapply(.GlobalEnv,is.data.frame)))){
          helpText("The Global Environment is empty")
        } else {
          mydataframes <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
          selectInput("dfname", "DataFrame from Global Env",
                      choices = mydataframes, multiple = FALSE)
        }
      } else if (input$datatype == "csv") {
        fileInput("file", "CSV File:",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                                                       ".csv"))

      } else if (input$datatype == "RData") {
        fileInput("rdatafile", "RData File",
                  accept = c(".RData"))
      }
    })

    # Creates input UI for the dataframe in an RData file
    output$rdatafile_dataframes <- renderUI({
      if (input$datatype == "RData") {
        req(input$rdatafile)
        new_env <<- new.env()
        load(input$rdatafile$datapath, new_env)

        dataframes <- names(which(unlist(eapply(new_env,is.data.frame))))

        selectInput("rdata_dfname", "DataFrame",
                    choices = dataframes)
      }
    })

    output$data_table <-  DT::renderDataTable({get_data()})

    output$robmedversion <- renderText({
      paste("ROBMED Package version: ", toString(packageVersion("robmed")))})

    output$downloadbuttonplot <- renderUI({
      req(input$runRobust)
      downloadButton("downloadPlot", "Download Plot")
    })

    output$downloadbuttonscript <- renderUI({
      req(input$runRobust)
      downloadButton("downloadScript", "Generate R script")
    })

    observeEvent(input$ConfidenceOLS, {
      updateSliderInput(session, "ConfidenceROBMED", value = input$ConfidenceOLS)
    })

    observeEvent(input$ConfidenceROBMED, {
      updateSliderInput(session, "ConfidenceOLS", value = input$ConfidenceROBMED)
    })

    observe({
      updateNumericInput(session, "seedOLS", value = input$seedROBMED)
      })

    observe({
      updateNumericInput(session, "seedROBMED", value = input$seedOLS)
    })

    output$downloadScript <- downloadHandler(

      filename = function() {
        paste(Sys.Date(), "script.R", sep = '')
      },
      content = function(file) {
        file.create(file)
        writeLines(vals$script, file)
      }
    )

    output$downloadbuttontableRobust <- renderUI({
      downloadButton("downloadTableRobust", "Download Table Robust")
    })

    output$downloadTableRobust <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), "tableROBMED.docx", sep = "")
      },
      content = function(file) {
        tabledoc <- export_table_MSWord(robust_bootstrap_test())
        print(tabledoc, file)
      }
    )

    output$downloadbuttontableOLS <- renderUI({
      downloadButton("downloadTableOLS", "Download Table OLS")
    })

    output$downloadTableOLS <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), "tableOLS.docx", sep = "")
      },
      content = function(file) {
        tabledoc <- export_table_MSWord(ols_bootstrap_test())
        print(tabledoc, file)
      }
    )

    output$ui_model_type <- renderUI({
      if (length(input$Mediators) > 1) {
      selectInput("Modeltype", "Multiple mediator model:",
                  choices = c("", 'parallel', 'serial'), selected = NULL)
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


    observeEvent(input$plot_format, {
      if (input$plot_format == "pdf") {
        updateRadioButtons(inputId = "plot_units",
                     label = "Units of height/width",
                     choices = c("in", "cm"),
                     selected = "in")
      } else if (input$plot_format == "png") {
        updateRadioButtons(inputId = "plot_units",
                           label = "Units of height/width",
                           choices = c("in", "cm", 'px'),
                           selected = "in")
      }
    })
    observeEvent(input$plot_units, {
      if (input$plot_units == "in") {
        min_val = 1
        max_val = 40
        standard = 7
      } else if (input$plot_units == "cm") {
        min_val = 2.54
        max_val = 101.6
        standard = 17.78
      } else if (input$plot_units == 'px') {
        min_val = 480
        max_val = 1200
        standard = 600
      }

      updateSliderInput(session, inputId = "width_plot",
                        label = "Width plot",
                        min = min_val, max = max_val, value = standard)
      updateSliderInput(session, inputId = "height_plot",
                        label = "Height plot",
                        min = min_val, max = max_val, value = standard)

    })


# This function takes the summary output of ROBMED
# and turns it into a nicely formatted table

#' Export result table to Word
#'
#' Export the table containing results of mediation analysis to a Microsoft Word
#' document.
#'
#' @param test_model an object inheriting from class
#' \code{"\link{test_mediation}"} or a list of objects of that class.
#' of object
#'
#' @param rounding a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#'
#' @param \dots For the generic function, additional arguments that need to be
#' passed to the methods.
#'
#'
#' @return An object of class \code{"\link{rdocx}"}, containing a table of the
#' results.
#'
#' @author Vincent Drenth
#'
#'
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

export_table_MSWord <- function(test_model, ...) {
  UseMethod("export_table_MSWord")
}


#' @export
#'
export_table_MSWord.list <- function(test_model,
                                     orientation = c("landscape", "portrait"),
                                     ...) {
  if (length(test_model) == 1) {
    return(export_table_MSWord(test_model[[1]]))
  }

  model_robust = Filter(function(x) x$fit$robust != FALSE, test_model)[[1]]
  model_ols = Filter(function(x) (x$fit$robust == FALSE), test_model)[[1]]

  tables_robust <- create_tables(model_robust)
  tables_ols <- create_tables(model_ols)

  doc <- officer::read_docx()

  if (orientation == "landscape") {
    direct_data_robust <- tables_robust$direct$body$dataset
    direct_data_ols <- tables_ols$direct$body$dataset

    indirect_data_robust <- tables_robust$indirect$body$dataset
    indirect_data_ols <- tables_ols$indirect$body$dataset

    colnames(direct_data_ols) <- unlist(lapply(colnames(direct_data_ols),
                                               FUN = function(x) paste0(x, "\r")))
    colnames(indirect_data_ols) <- unlist(lapply(colnames(indirect_data_ols),
                                                 FUN = function(x) paste0(x, "\r")))

    direct_data_ols <- direct_data_ols[,2:ncol(direct_data_ols)]
    direct_data <- data.frame(direct_data_robust, direct_data_ols,
                              check.names = FALSE)

    indirect_data_ols <- indirect_data_ols[,2:ncol(indirect_data_ols)]
    indirect_data <- data.frame(indirect_data_robust, indirect_data_ols,
                                check.names = FALSE)

    ft_direct <- flextable(direct_data)
    ft_direct <- flextable::width(ft_direct, j = 1, width = 2, unit = "in")
    ft_direct <- add_header_row(ft_direct,
                                values = c(" ", "ROBMED", "OLS Bootstrap"),
                                top = TRUE, colwidths = c(1, 4, 4))
    ft_direct <- align(ft_direct, align = "center", part = "all")

    ft_indirect <- flextable::flextable(indirect_data)
    ft_indirect <- flextable::width(ft_indirect, j = 1, width = 2,
                                    unit = "in")
    ft_indirect <- flextable::width(ft_indirect, j = 3, width = 1.5,
                                    unit = "in")
    ft_indirect <- flextable::width(ft_indirect, j = 6, width = 1.5,
                                    unit = "in")
    ft_indirect <- align(ft_indirect, align = "center", part = "all")

    doc <- officer::body_end_section_landscape(doc)
    doc <- flextable::body_add_flextable(doc, ft_direct)
    doc <- flextable::body_add_flextable(doc, ft_indirect)
    doc <- officer::body_end_section_landscape(doc)
  } else if (orientation == 'portrait') {
    doc <- officer::body_add_fpar(doc, value = fpar(ftext("ROBMED")))
    doc <- flextable::body_add_flextable(doc, tables_robust$direct)
    doc <- flextable::body_add_flextable(doc, tables_robust$indirect)
    doc <- officer::body_add_fpar(doc, value = fpar(ftext("OLS Bootstrap")))
    doc <- flextable::body_add_flextable(doc, tables_ols$direct)
    doc <- flextable::body_add_flextable(doc, tables_ols$indirect)
  }
  return(doc)
}

#'@export
export_table_MSWord.test_mediation <- function(test_model, rounding = 4, ...) {
  tables <- create_tables(test_model = test_model, rounding = rounding)
  ft_direct <- tables$direct
  ft_indirect <- tables$indirect

  doc <- read_docx()
  doc <- body_add_flextable(doc, ft_direct)
  doc <- body_add_flextable(doc, ft_indirect)
  return(doc)
}

create_tables <- function(test_model, rounding = 4) {

  sm <- summary(test_model)$summary

  if (test_model$fit$model == "serial") {
    if (length(sm$m) == 1) {
      directrows <- 2 * (length(sm$x)) + 1
      indirectrows <- length(sm$x)

    } else if (length(sm$m) == 2) {
      #Generate table for 2 serial mediators
      directrows <- 3 * (1 + length(sm$x))
      indirectrows <- 3 * length(sm$x)
    }
  } else {
    #Model is parallel
    rows = 2 * (length(sm$x) * length(sm$m)) + 2 * length(sm$x) + length(sm$m)
    indirectrows = length(sm$x) * length(sm$m)
    directrows = rows - indirectrows
  }

  df_dir <- as.data.frame(matrix(NA, nrow = directrows, ncol = 5))
  colnames(df_dir) <- c("Direct Effects", 'Estimate', 'Std. Error', 'z statistic', 'p-value')

  row <- 1
  for (med in sm$m) {
    if (length(sm$m) > 1){
      coefs_a <- sm$fit_mx[med][[1]]$coefficients
    } else {
      coefs_a <- sm$fit_mx$coefficients
    }

    #Add a paths
    for (reg in sm$x) {
      df_dir[row, 1] <- paste(reg, med, sep = "->")
      df_dir[row, 2:5] <- coefs_a[reg, 2:5]
      row <- row + 1
    }
  }
  a_paths <- row

  coefs_b <- sm$fit_ymx$coefficients
  for (med in sm$m) {
    #Add b paths
    df_dir[row, 1] <- paste('(X),',med ,'->' , sm$y)
    df_dir[row, 2:5] <- coefs_b[med, 2:5]
    row <- row + 1
  }
  b_paths <- row

  # Add c path (Direct effect)
  for (reg in sm$x){
    df_dir[row, 1] <- paste(reg,'->', sm$y, '(direct)')
    df_dir[row, 2:5] <- sm$direct[reg, 2:5]
    row <- row + 1
  }
  c_paths <- row

  # Add c' path (Total effect)
  for (reg in sm$x) {
    df_dir[row, 1] <- paste(reg, '->', sm$y, '(total)')
    df_dir[row, 2:5] <- sm$total[reg, 2:5]
    row <- row + 1
  }

  pvals <- p_value(test_model, parm = 'indirect')

  #Add indirect effects (a (d) b paths)
  df_ind <- data.frame(matrix(0, nrow = indirectrows, ncol = 4))
  colnames(df_ind) <- c("Indirect Effects", 'Estimate', 'Confidence Interval', 'p-value')
  if (test_model$fit$model == "serial" ){
    row <- 1
    for (reg in sm$x) {
      # Through only first or only second mediator
      for (med in sm$m) {
        effectname <- paste(reg, '->', med, sep ='')
        df_ind[row,1] <- paste(effectname, '(Indirect)')

        if (length(sm$m) > 1) {
          df_ind[row, 2] <- test_model$indirect[effectname][[1]]
          lower <- round(test_model$ci[effectname, 1], rounding)
          upper <- round(test_model$ci[effectname, 2], rounding)
        } else {
          df_ind[row,2] <- test_model$indirect[reg][[1]]
          lower <- round(test_model$ci[1], rounding)
          upper <- round(test_model$ci[2], rounding)
        }

        df_ind[row, 3] <- paste('(', lower, ',',upper,')', sep = '')

        row <- row + 1
      }

      # Path through both mediators
      effectname <- paste(reg, '->', sm$m[1], '->', sm$m[2], sep = '')

      df_ind[row,1] <- paste(effectname, '(Indirect)', sep = '')
      df_ind[row, 2] <- test_model$indirect[effectname][[1]]

      lower <- round(test_model$ci[effectname, 1], rounding)
      upper <- round(test_model$ci[effectname, 2], rounding)

      df_ind[row, 3] <- paste('(', lower, ',', upper,')', sep = '')
      row <- row + 1
    }
  } else {
    # Parallel model
    row <- 1
    for (reg in sm$x) {
      for (med in sm$m) {
        effectname <- paste(reg, '->', med, sep ='')
        df_ind[row,1] <- paste(effectname, '(Indirect)')

        if (length(sm$m) > 1) {
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[effectname, 1], rounding)
            upper <- round(test_model$ci[effectname, 2], rounding)
            df_ind[row, 2] <- test_model$indirect[effectname][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", effectname, sep = '_')][[1]]

          } else {
            # Only 1 explanatory variable, multiple mediators
            lower <- round(test_model$ci[med, 1], rounding)
            upper <- round(test_model$ci[med, 2], rounding)
            df_ind[row, 2] <- test_model$indirect[med][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", med, sep = '_')][[1]]

          }
        } else {
          # Only 1 mediator
          if (length(sm$x) > 1) {
            lower <- round(test_model$ci[reg, 1], rounding)
            upper <- round(test_model$ci[reg, 2], rounding)
            df_ind[row,2] <- test_model$indirect[reg][[1]]
            df_ind[row, 4] <- pvals[paste("Indirect", reg, sep = '_')][[1]]

          } else {
            # only 1 indirect effect
            df_ind[row,2] <- test_model$indirect
            lower <- round(test_model$ci[1], rounding)
            upper <- round(test_model$ci[2], rounding)
            df_ind[row, 4] <- pvals["Indirect"][[1]]

          }
        }
        df_ind[row, 3] <- paste('(', lower, ',', upper,')', sep = '')
        row <- row + 1
      }
    }
  }

  # Create the table from the dataframes
  df_rounded <- data.frame(lapply(df_dir, function(y) if(is.numeric(y)) round(y, rounding) else y))
  df_ind_rounded <- data.frame(lapply(df_ind, function(y) if(is.numeric(y)) round(y, rounding) else y))

  set_flextable_defaults(
    font.size = 10,
    padding = 2,
    background.color = 'white')

  ft_direct <- flextable(df_rounded)
  ft_direct <- width(ft_direct, j = 1, width = 2.5, unit = "in")
  ft_direct <- width(ft_direct, j = 2:5, width = 1, unit = "in")
  ft_direct <- align(ft_direct, i = 1:directrows, j = 2:5, align = "center", part = "body")
  ft_direct <- align(ft_direct, j = 2:5, align = "center", part = "header")

  # Add spacing and a line between different kinds of paths
  ft_direct <- padding(ft_direct, i = a_paths, padding.bottom =  5, part = "body")
  ft_direct <- padding(ft_direct, i = b_paths, padding.bottom =  5, part = "body")
  ft_direct <- padding(ft_direct, i = c_paths, padding.bottom =  5, part = "body")
  ft_direct <- padding(ft_direct, i = directrows, padding.bottom =  10, part = "body")

  ft_direct <- hline(ft_direct, i = a_paths - 1, border = fp_border("gray"), part = "body")
  ft_direct <- hline(ft_direct, i = b_paths - 1, border = fp_border("gray"), part = "body")
  ft_direct <- hline(ft_direct, i = c_paths - 1, border = fp_border("gray"), part = "body")

  ft_indirect <- flextable(df_ind_rounded)
  ft_indirect <- width(ft_indirect, j = 1, width = 2.5, unit = "in")
  ft_indirect <- width(ft_indirect, j = 3, width = 2, unit = "in")
  ft_indirect <- width(ft_indirect, j = c(2,4), width = 1, unit = "in")
  ft_indirect <- align(ft_indirect, i = 1:indirectrows, j = 2:4, align = "center", part = "body")
  ft_indirect <- align(ft_indirect, j = 2:4, align = "center", part = "header")
  ft_indirect

  ft_indirect <- add_footer_lines(ft_indirect, paste('Sample size = ', nrow(test_model$fit$data),
                                                     '. Number of bootstrap samples = ', test_model$R , '.\n',
                                                     '†p < .1. *p < .05. **p < .01. ***p < .001.'))
  result = list()
  result$direct <- ft_direct
  result$indirect <- ft_indirect

  return(result)
}
})

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
library(kableExtra)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


      session$onSessionEnded(function() { stopApp() })
      vals <- reactiveValues()
      vals$script <- c('library("robmed")')

      # Reactive expression to get data in dataframe
      get_data <- reactive({
        if (input$datatype == "Existing data frame"){
            if (is.null(input$dfname)) {
              final_df <- data.frame()
            } else {
              final_df <- as.data.frame(get(input$dfname, .GlobalEnv))
            }
          } else if (input$datatype == "RData") {
            if (is.null(input$rdata_dfname)) {
              final_df <- data.frame()
            } else {
              final_df <- as.data.frame(get(input$rdata_dfname, envir = new_env))
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

      df_name <- reactive(
        ifelse(input$datatype == "RData", input$rdata_dfname,
                        input$dfname)
      )

      # Creates boot_test_mediation object
      robust_bootstrap_test <- eventReactive(input$runRobust,{
        vals$script <- c("", vals$script)

      df_name <- ifelse(input$datatype == "RData", input$rdata_dfname,
                        input$dfname)

      command_control <- call("reg_control", efficiency = input$MM_eff,
                              max_iterations = input$max_iter)

      command_robust_test <- call("test_mediation",
                                  as.name(df_name),
                                  x = input$Explanatory,
                                  y = input$Response,
                                  m = input$Mediators,
                                  robust = TRUE,
                                  level = input$ConfidenceOLS,
                                  R = input$boot_samplesROBMED,
                                  control = as.name("ctrl")
                                  )

      command_seed <- call("set.seed", input$seedROBMED)

      if (!is.null(input$Covariates)) {
        command_robust_test$covariates <- input$Covariates
      }

      if (!is.null(input$Modeltype)) {
        command_robust_test$model <- input$Modeltype
      }

      if (!is.na(input$seedROBMED)) {
        eval(command_seed)
        vals$script <- c(vals$script, as.character(as.expression(command_seed)))
      }

      vals$script <- c(vals$script,
                       paste("ctrl <-",
                             as.character(as.expression(command_control))),
                       as.character(as.expression(command_robust_test)))

      assign("ctrl", eval(command_control))
      eval(command_robust_test)
      })

      ols_bootstrap_test <- eventReactive(input$runOLS,
       {
         vals$script <- c("", vals$script)

         df_name <- ifelse(input$datatype == "RData", input$rdata_dfname,
                           input$dfname)

         command_seed <- call("set.seed", input$seedOLS)
         command_ols_test <- call("test_mediation",
                                  as.name(df_name),
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

         vals$script <- c(vals$script,
                          as.character(as.expression(command_ols_test)))

         eval(command_ols_test)
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
          paste(Sys.Date(),"_",df_name(),"_","diagnostic_plot.", ext, sep = '')
        },
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())

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

    # Generates the UI that allows users to select variables of the data
    output$selectExplanatory <- renderUI({
      choices <- colnames(get_data())
      selectInput(inputId="Explanatory", label = "Independent variable(s)",
                  choices = choices, multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      choices <- colnames(numeric_data())
      selectInput(inputId="Mediators",
                  label= p("Mediator(s)",
                           span("(Numeric)", style = "color: 	#A0A0A0")),
                  choices = choices, multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      choices <- colnames(numeric_data())
      selectInput(inputId="Response",
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

      if (input$datatype == "Existing data frame") {
        if (!any(as.logical(lapply(.GlobalEnv, is.data.frame)))){
          helpText("There are no data frames in your Global Environment.")
        } else {
          mydataframes <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
          selectInput("dfname", "DataFrame from Global Env",
                      choices = mydataframes, multiple = FALSE)
        }
      }
      else if (input$datatype == "RData") {
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
      updateSliderInput(session, "ConfidenceROBMED",
                        value = input$ConfidenceOLS)
    })

    observeEvent(input$ConfidenceROBMED, {
      updateSliderInput(session, "ConfidenceOLS",
                        value = input$ConfidenceROBMED)
    })

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
        writeLines(vals$script, file)
      }
    )

    output$downloadbuttontableRobust <- renderUI({
      downloadButton("downloadTableRobust", "Download Table ROBMED")
    })

    output$downloadTableRobust <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), df_name(), "table.docx", sep = "_")
      },
      content = function(file) {
        showModal(modalDialog("Loading", footer=NULL))
        on.exit(removeModal())

        tabledoc <- export_table_MSWord(robust_bootstrap_test())
        print(tabledoc, file)
      }
    )

    output$downloadbuttontableOLS <- renderUI({
      downloadButton("downloadTableOLS", "Download Table OLS")
    })

    output$downloadTableOLS <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), df_name(), "table.docx", sep = "_")
      },
      content = function(file) {
        showModal(modalDialog("Loading", footer=NULL))
        on.exit(removeModal())

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


    observeEvent(input$plot_units, {
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
        paste(Sys.Date(), "_", df_name(), "_", "table", "_",input$table_orientation, ".docx")
      },
      content = function(file) {
        showModal(modalDialog("Loading", footer = NULL))
        on.exit(removeModal())

        mediation_list <- list()

        if (isTruthy(input$models_tables)) {
          contains_robust <- "ROBMED" %in% input$models_tables
          contains_ols <- "OLS" %in% input$models_tables
        } else {
          contains_robust <- TRUE
          contains_ols <- TRUE
        }

        if (contains_robust) {
          if(isTruthy(input$runRobust)) {
            mediation_list$robust <- robust_bootstrap_test()
          }
        }

        if (contains_ols) {
          if(isTruthy(input$runOLS)) {
            mediation_list$ols <- ols_bootstrap_test()
          }
        }

        document <- export_table_MSWord(mediation_list,
                                        orientation = input$table_orientation)
        print(document, file)
      }
    )

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
#' @param digits a positive integer, which determines the number of decimals
#' that should be displayed in the table. The default is to display 4 decimals.
#'
#' @param orientation the  orientation in which the table is displayed in the
#' document. When set to portrait, the tables are displayed
#' underneath each other. When set to landscape, the tables are displayed side
#' by side.
#'
#'
#'
#' @return An object of class \code{"\link{rdocx}"}, containing a table of the
#' results of the provided objects.
#'
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
    doc <- officer::body_end_section_landscape(doc)
    for (index in seq(1, ceiling((length(test_model) + 1)/2), 2)) {
      tables_left <- create_tables(test_model[[index]])
      direct_data_left <- tables_left$direct$body$dataset
      indirect_data_left <- tables_left$indirect$body$dataset

      tables_right <- create_tables(test_model[[index + 1]])
      direct_data_right <- tables_right$direct$body$dataset
      indirect_data_right <- tables_right$indirect$body$dataset

      colnames(direct_data_right) <- unlist(lapply(colnames(direct_data_right),
                                                 FUN = function(x) paste0(x, "\r")))
      colnames(indirect_data_right) <- unlist(lapply(colnames(indirect_data_right),
                                                   FUN = function(x) paste0(x, "\r")))

      direct_data_right <- direct_data_right[,2:ncol(direct_data_right)]
      indirect_data_right <- indirect_data_right[,2:ncol(indirect_data_right)]

      direct_data <- data.frame(direct_data_left, direct_data_right)
      indirect_data <- data.frame(indirect_data_left, indirect_data_right)

      ft_direct <- flextable::flextable(direct_data)
      ft_direct <- flextable::width(ft_direct, j = 1, width = 2, unit = "in")
      ft_direct <- flextable::add_header_row(ft_direct,
                                  values = c(" ", "METHOD", "METHOD"),
                                  top = TRUE, colwidths = c(1, 4, 4))
      ft_direct <- align(ft_direct, align = "center", part = "all")

      ft_indirect <- flextable::flextable(indirect_data)
      ft_indirect <- flextable::width(ft_indirect, j = 1, width = 2,
                                      unit = "in")
      ft_indirect <- flextable::width(ft_indirect, j = 3, width = 1.5,
                                      unit = "in")
      ft_indirect <- flextable::width(ft_indirect, j = 6, width = 1.5,
                                      unit = "in")
      ft_indirect <- flextable::align(ft_indirect, align = "center",
                                      part = "all")

      doc <- flextable::body_add_flextable(doc, ft_direct)
      doc <- flextable::body_add_flextable(doc, ft_indirect)
      doc <- officer::body_add_break(doc, "after")
    }

    if (length(test_model) %% 2 == 1) {
      tables <- create_tables(test_model[[length(test_model)]])
      #direct_table <- flextable::add_header_row(tables$direct,
      #                                          values = c("METHOD"),
      #                                          top = TRUE,
      #                                          colwidths = c(5))
      doc <- flextable::body_add_flextable(doc, tables$direct)
      doc <- flextable::body_add_flextable(doc, tables$indirect)
    }

    doc <- officer::body_end_section_landscape(doc)
  } else if (orientation == "portrait") {
      count_page <- 0
      for (test_object in test_model) {
        tables <- create_tables(test_object)
        doc <- flextable::body_add_flextable(doc, tables$direct)
        doc <- flextable::body_add_flextable(doc, tables$indirect)
        count_page <- count_page + 1
        if (count_page %% 2 == 0) {
        doc <- officer::body_add_break(doc, "after")
        }
      }
  }
  return(doc)
}

#'@export
export_table_MSWord.test_mediation <- function(test_model, digits = 4, ...) {
  tables <- create_tables(test_model = test_model, digits = digits)
  ft_direct <- tables$direct
  ft_indirect <- tables$indirect

  doc <- read_docx()
  doc <- body_add_flextable(doc, ft_direct)
  doc <- body_add_flextable(doc, ft_indirect)
  return(doc)
}

# Function using xtable to create latex table
to_latex <- function(test_model, digits = 4) {
  tables <- create_tables(test_model = test_model, digits = digits)

  data_direct <- tables$direct$body$data
  data_indirect <- tables$indirect$body$data

  table_direct <- xtable::xtable(data_direct, digits = digits,
                                 align = "lXllll")

  table_indirect <- xtable::xtable(data_indirect, digits = digits,
                                 align = "lXlll")

  xtable_direct <- capture.output(print(table_direct,
                                         tabular.environment = "tabularx",
                                         width = "1.2\\textwidth",
        include.rownames = F, hline.after = c(0:nrow(data_direct))))

  xtable_indirect <- capture.output(print(table_indirect,
                                          tabular.environment = "tabularx",
                                          width = "1.2\\textwidth",
                                          include.rownames = F,
                                          hline.after = c(0:nrow(data_indirect)))
                                    )

  full_table <- merge_vertical_xtable(xtable_direct, xtable_indirect)
  full_table <- cat(full_table, sep = '\n')
  return(full_table)

}

# Works but not so nice
merge_vertical_xtable <- function(table1, table2) {

  list_table1 <- table1
  list_table2 <- table2

  # Remove last line from first table and first 3 from second table
  list_table1 <- list_table1[1:(length(list_table1) - 1)]
  list_table2 <- list_table2[4:length(list_table2)]

  full_table <- append(list_table1, list_table2)
  return(full_table)
}

# Internal function to create two tables for a model of type test_mediation
# One table with direct effects and one table with indirect effects
create_tables <- function(test_model, digits = 4) {

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
  colnames(df_dir) <- c("Direct Effects", "Estimate", "Std. Error",
                        "z statistic", "p-value")

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
    df_dir[row, 1] <- paste("(X),",med ,"->" , sm$y)
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

  pvals <- p_value(test_model, parm = "indirect")

  #Add indirect effects (a (d) b paths)
  df_ind <- data.frame(matrix(0, nrow = indirectrows, ncol = 4))
  colnames(df_ind) <- c("Indirect Effects", "Estimate",
                        "Confidence Interval", "p-value")
  if (test_model$fit$model == "serial" ){
    row <- 1
    for (reg in sm$x) {
      # Through only first or only second mediator
      for (med in sm$m) {
        effectname <- paste(reg, '->', med, sep ='')
        df_ind[row,1] <- paste(effectname, '(Indirect)')

        if (length(sm$m) > 1) {
          df_ind[row, 2] <- test_model$indirect[effectname][[1]]
          lower <- round(test_model$ci[effectname, 1], digits)
          upper <- round(test_model$ci[effectname, 2], digits)
        } else {
          df_ind[row,2] <- test_model$indirect[reg][[1]]
          lower <- round(test_model$ci[1], digits)
          upper <- round(test_model$ci[2], digits)
        }

        df_ind[row, 3] <- paste('(', lower, ',',upper,')', sep = '')

        row <- row + 1
      }

      # Path through both mediators
      effectname <- paste(reg, '->', sm$m[1], '->', sm$m[2], sep = '')

      df_ind[row,1] <- paste(effectname, "(Indirect)", sep = '')
      df_ind[row, 2] <- test_model$indirect[effectname][[1]]

      lower <- round(test_model$ci[effectname, 1], digits)
      upper <- round(test_model$ci[effectname, 2], digits)

      df_ind[row, 3] <- paste('(', lower, ',', upper,')', sep = '')
      row <- row + 1
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
            df_ind[row, 4] <- pvals[paste("Indirect", reg, sep = "_")][[1]]

          } else {
            # only 1 indirect effect
            df_ind[row,2] <- test_model$indirect
            lower <- round(test_model$ci[1], digits)
            upper <- round(test_model$ci[2], digits)
            df_ind[row, 4] <- pvals["Indirect"][[1]]

          }
        }
        df_ind[row, 3] <- paste("(", lower, ",", upper,")", sep = "")
        row <- row + 1
      }
    }
  }

  # Create the table from the dataframes
  df_rounded <- data.frame(lapply(df_dir, function(y) if(is.numeric(y)) round(y, digits) else y))
  df_ind_rounded <- data.frame(lapply(df_ind, function(y) if(is.numeric(y)) round(y, digits) else y))

  set_flextable_defaults(
    font.size = 10,
    padding = 2,
    background.color = 'white')

  ft_direct <- flextable(df_rounded)
  ft_direct <- width(ft_direct, j = 1, width = 2.5, unit = "in")
  ft_direct <- width(ft_direct, j = 2:5, width = 1, unit = "in")
  ft_direct <- add_header_row(ft_direct, top = TRUE, values = c("METHOD"),
                              colwidths = c(5))
  ft_direct <- align(ft_direct, i = 1:directrows, j = 2:5, align = "center",
                     part = "body")
  ft_direct <- align(ft_direct, align = "center", part = "header")

  # Add spacing and a line between different kinds of paths
  ft_direct <- padding(ft_direct, i = a_paths, padding.bottom =  5,
                       part = "body")
  ft_direct <- padding(ft_direct, i = b_paths, padding.bottom =  5,
                       part = "body")
  ft_direct <- padding(ft_direct, i = c_paths, padding.bottom =  5,
                       part = "body")
  ft_direct <- padding(ft_direct, i = directrows, padding.bottom =  10,
                       part = "body")

  ft_direct <- hline(ft_direct, i = a_paths - 1, border = fp_border("gray"),
                     part = "body")
  ft_direct <- hline(ft_direct, i = b_paths - 1, border = fp_border("gray"),
                     part = "body")
  ft_direct <- hline(ft_direct, i = c_paths - 1, border = fp_border("gray"),
                     part = "body")

  ft_indirect <- flextable(df_ind_rounded)
  ft_indirect <- width(ft_indirect, j = 1, width = 2.5, unit = "in")
  ft_indirect <- width(ft_indirect, j = 3, width = 2, unit = "in")
  ft_indirect <- width(ft_indirect, j = c(2,4), width = 1, unit = "in")
  ft_indirect <- align(ft_indirect, i = 1:indirectrows, j = 2:4,
                       align = "center", part = "body")
  ft_indirect <- align(ft_indirect, j = 2:4, align = "center", part = "header")

  ft_indirect <- add_footer_lines(ft_indirect, paste('Sample size = ', nrow(test_model$fit$data),
                                                     '. Number of bootstrap samples = ', test_model$R , '.\n',
                                                     '†p < .1. *p < .05. **p < .01. ***p < .001.'))
  result = list()
  result$direct <- ft_direct
  result$indirect <- ft_indirect

  return(result)
}
})

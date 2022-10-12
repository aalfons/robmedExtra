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



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

      # Reactive expression to get data; only supports csv for now
      get_data <- reactive({
        if (input$datatype == 'csv'){
          req(input$file)
          ext <- tools::file_ext(input$file$name)
         final_df <- switch(ext, csv = vroom::vroom(input$file$datapath, delim = ","),
                 validate("Invalid file; Please upload a .csv file"))
        }
        if (input$datatype == 'Existing DataFrame'){
          req(input$dfname)
          dataframeName = input$dfname
          final_df <- as.data.frame(get(input$dfname, .GlobalEnv))

        }
        if (input$datatype == 'RData') {
          req(input$rdata_dfname)
          final_df <- as.data.frame(get(input$rdata_dfname,envir = new_env ))
        }

        final_df
      })

      numeric_data <- reactive({
        df <- get_data()
        Filter(is.numeric, df)
      })

      observeEvent(input$rngversion,{
        if(input$rngversion == 'Current'){
          RNGversion(getRversion())
        } else{
          RNGversion(input$rngversion)
        }
      })

      formula <- reactive({
        req(input$Explanatory, input$Modeltype, input$Response, input$Mediators)

        explanatory <- paste(input$Explanatory, collapse = '+')
        mediators <- paste('m(', paste(input$Mediators, collapse = ','),', .model = "',input$Modeltype,'")', sep = '')

        controls <- ''
        if(length(input$Covariates) != 0){
          controls <- paste('+','covariates(', paste(input$Covariates, collapse = ','), ')')
        }

        form_out <- as.formula(paste(input$Response, '~', mediators,'+', explanatory, controls))

      })


      # Creates boot_test_mediation object
      robust_bootstrap_test <- eventReactive(input$runRobust,
                                              {
      df <- get_data()
      f_test <- formula()
      control_var <- reg_control(efficiency = input$MM_eff, max_iterations = input$max_iter, seed = input$seedROBMED)
      robust_boot_test <- test_mediation(f_test, data = df, robust = TRUE, level = input$ConfidenceROBMED, R = input$boot_samplesROBMED,
                                         control = control_var)
      })

      ols_bootstrap_test <- eventReactive(input$runOLS,
                                             {
                                               set.seed(input$seedOLS)
                                               df <- get_data()
                                               f_test <- formula()
                                               print("Formula fine")
                                               ols_bootstrap <- test_mediation(f_test, data = df, robust = FALSE,
                                                                                level = input$ConfidenceOLS, R = input$boot_samplesOLS)
                                             })

      output$summaryOLS <- renderPrint({
        summary(ols_bootstrap_test())
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
          paste(Sys.Date() ,'plot.png', sep = '')
        },
        content = function(file) {
          png(file)
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
      summary(robust_boot_simple)$summary
    })



    observe({
      isolate(selectedInput <- input$Explanatory)
      updateSelectInput(session, inputId = 'Explanatory',
                        choices = setdiff(colnames(get_data()),
                                          c(input$Mediators,
                                            input$Response,
                                            input$Covariates)),
                        selected = selectedInput)

    })

    observe({
      isolate(selectedInput <- input$Mediators)

      updateSelectInput(session, inputId = 'Mediators',
                        choices = setdiff(colnames(numeric_data()),
                                          c(input$Explanatory,
                                            input$Response,
                                            input$Covariates)),
                        selected = selectedInput)
    })

    observe({
      isolate(selectedInput <- input$Response)

      updateSelectInput(session, inputId = 'Response',
                        choices = setdiff(colnames(numeric_data()),
                                          c(input$Explanatory,
                                            input$Mediators,
                                            input$Covariates)),
                        selected = selectedInput)
    })

    observe({
      isolate(selectedInput <- input$Covariates)

      updateSelectInput(session, inputId = 'Covariates',
                        choices = setdiff(colnames(get_data()),
                                          c(input$Explanatory,
                                            input$Response,
                                            input$Mediators)),
                        selected = selectedInput)
    })

    # Generates the UI that allows users to select variables of the uploaded data
    output$selectExplanatory <- renderUI({
      choices <- colnames(get_data())
      selectInput(inputId='Explanatory', label='Independent variable(s):', choices = choices, multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      choices = colnames(numeric_data())
      selectInput(inputId='Mediators', label='Mediating variable(s):', choices = choices, multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      choices = colnames(numeric_data())
      selectInput(inputId='Response', label='Dependent variable:', choices = choices, multiple = FALSE)
    })

    output$selectControls <- renderUI({
      isolate(self <- input$Covariates)
      choices = colnames(get_data())
      selectInput(inputId='Covariates', label='Control variables:', choices = choices, multiple = TRUE)
    })

    output$dataframechoice <- renderUI({
      if (input$datatype == 'Existing DataFrame') {
        if (is.null(unlist(eapply(.GlobalEnv,is.data.frame)))){
          helpText("The Global Environment is empty")
        } else {
          mydataframes <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
          selectInput('dfname', 'DataFrame from Global Env',
                      choices = mydataframes, multiple = FALSE)
        }
      } else if (input$datatype == 'csv') {
        fileInput("file", "CSV File:",accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv"))
      } else if (input$datatype == 'RData') {
        fileInput('rdatafile', 'RData File:',
                  accept = c('.RData'))
      }
    })

    output$rdatafile_dataframes <- renderUI({
      if (input$datatype == 'RData') {
        req(input$rdatafile)
        new_env <<- new.env()
        load(input$rdatafile$datapath, new_env)

        dataframes <- names(which(unlist(eapply(new_env,is.data.frame))))

        selectInput('rdata_dfname', 'DataFrame:',
                    choices = dataframes)
      }
    })

    output$data_table <-  DT::renderDataTable({get_data()})

    output$robmedversion <- renderText({
      paste('ROBMED Package version: ', toString(packageVersion('robmed')))})

    output$downloadbuttonplot <- renderUI({
      req(input$runRobust)
      downloadButton('downloadPlot', 'Download Plot')
    })

    output$downloadbuttonscript <- renderUI({
      req(input$runRobust)
      downloadButton('downloadScript', 'Generate R script')
    })

    observeEvent(input$ConfidenceOLS, {
      updateSliderInput(session, 'ConfidenceROBMED', value = input$ConfidenceOLS)
    })

    observeEvent(input$ConfidenceROBMED, {
      updateSliderInput(session, 'ConfidenceOLS', value = input$ConfidenceROBMED)
    })

    observe({
      updateNumericInput(session, 'seedOLS', value = input$seedROBMED)
      })

    observe({
      updateNumericInput(session, 'seedROBMED', value = input$seedOLS)
    })



    output$downloadScript <- downloadHandler(

      filename = function() {
        paste(Sys.Date(), '-Script.R', sep = '')
      },
      content = function(file) {
        file.create(file)

        filename <- paste(getwd(),'/', 'useddata.Rdata', sep = '')
        df = get_data()
        save(df, file = filename)

        # Load the RData file in the R Script
        if (input$datatype == 'RData') {env = new_env} else {env = .GlobalEnv}
        write(paste("load('",filename, "')", sep = ''), file, append = T)

        # Get the dataframe in the RData file
        df_name <- names(which(unlist(eapply(env,is.data.frame))))

        #Write test_mediation(formula, data)
        txt_controlvars <- paste('control_var <- reg_control(efficiency = ', input$MM_eff, ', max_iterations = ', input$max_iter, ', seed = ', input$seedROBMED,')')
        txt_test <- paste('bootstrap_test <- test_mediation(', format(formula()), ', data = ', df_name,', ', 'robust = TRUE,', 'level = ', input$ConfidenceROBMED, ', control = control_var)')

        write(txt_controlvars, file, append = T)
        write(txt_test, file, append = T)

        #Give summary of output
        write('summary(bootstrap_test)', file, append = T)
        dev.off()

      }
    )

    output$downloadbuttontableRobust <- renderUI({
      downloadButton('downloadTableRobust', 'Download Table')
    })

    output$downloadTableRobust <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), 'ROBMEDoutput.docx', sep = '')
      },
      content = function(file) {
        tabledoc <- summarizetable(robust_bootstrap_test())
        print(tabledoc, file)
      }
    )

    output$downloadbuttontableOLS <- renderUI({
      downloadButton('downloadTableOLS', 'Download Table')
    })

    output$downloadTableOLS <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), 'OLSoutput.docx', sep = '')
      },
      content = function(file) {
        tabledoc <- summarizetable(ols_bootstrap_test())
        print(tabledoc, file)
      }
    )

# This function takes the summary output of ROBMED and turns it into a nicely formatted table
  summarizetable <- function(test_model, rounding = 4) {

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
      coefs_b <- sm$fit_ymx$coefficients

      #Add a paths
      for (reg in sm$x) {
        df_dir[row, 1] <- paste(reg, med, sep = "->")
        df_dir[row, 2:5] <- coefs_a[reg, 2:5]
        row <- row + 1
      }

      #Add b paths
      df_dir[row, 1] <- paste('(X),',med ,'->' , sm$y)
      df_dir[row, 2:5] <- coefs_b[med, 2:5]
      row <- row + 1
    }

    # Add direct effect of regressor on response and total effect (c path and c' path)
    for (reg in sm$x){
      df_dir[row, 1] <- paste(reg,'->', sm$y, '(direct)')
      df_dir[row, 2:5] <- sm$direct[reg, 2:5]
      row <- row + 1

      df_dir[row, 1] <- paste(reg, '->', sm$y, '(total)')
      df_dir[row, 2:5] <- sm$total[reg, 2:5]
      row <- row + 1
    }

    #Add indirect effects (a (d) b paths)
    df_ind <- data.frame(matrix(0, nrow = indirectrows, ncol = 4))
    colnames(df_ind) <- c("Indirect Effects", 'Estimate', 'Confidence Interval', 'p-value')
    if (test_model$fit$model == "serial" ){
      row <- 1
      for (reg in sm$x) {
        # Through only first or second mediator
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

        # Through both mediators TODO: fix error
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
            } else {
              lower <- round(test_model$ci[med, 1], rounding)
              upper <- round(test_model$ci[med, 2], rounding)
            }
            df_ind[row, 2] <- test_model$indirect[effectname][[1]]
          } else {
            if (length(sm$x) > 1) {
              lower <- round(test_model$ci[reg, 1], rounding)
              upper <- round(test_model$ci[reg, 2], rounding)
              df_ind[row,2] <- test_model$indirect[reg][[1]]
            } else {
              df_ind[row,2] <- test_model$indirect
              lower <- round(test_model$ci[1], rounding)
              upper <- round(test_model$ci[2], rounding)
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
    ft_direct <- width(ft_direct, j = 1, width = 2.5, unit = 'in')
    ft_direct <- width(ft_direct, j = 2:5, width = 1, unit = 'in')
    ft_direct <- align(ft_direct, i = 1:directrows, j = 2:5, align = 'center', part = 'body')
    ft_direct <- align(ft_direct, j = 2:5, align = 'center', part = 'header')

    ft_indirect <- flextable(df_ind_rounded)
    ft_indirect <- width(ft_indirect, j = 1, width = 2.5, unit = 'in')
    ft_indirect <- width(ft_indirect, j = 3, width = 2, unit = 'in')
    ft_indirect <- width(ft_indirect, j = c(2,4), width = 1, unit = 'in')
    ft_indirect <- align(ft_indirect, i = 1:indirectrows, j = 2:4, align = 'center', part = 'body')
    ft_indirect <- align(ft_indirect, j = 2:4, align = 'center', part = 'header')

    doc <- read_docx()
    doc <- body_add_flextable(doc, ft_direct)
    doc <- body_add_flextable(doc, ft_indirect)
    return(doc)
  }

})

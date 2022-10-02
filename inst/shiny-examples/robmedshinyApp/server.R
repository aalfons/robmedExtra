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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
          final_df <- as.data.frame(get(input$dfname))

        }
        if (input$datatype == 'RData') {
          req(input$rdata_dfname)
          final_df <- as.data.frame(get(input$rdata_dfname))
        }

        final_df

      })

      observeEvent(input$rngversion,{
        if(input$rngversion != 'Current'){
          RNGversion(input$rngversion)
        }
      })

      # For now supports only all serial or all parallel mediators
      # TODO: - add support for combination of mediator type ?

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

      control_var = reg_control(efficiency = input$MM_eff, max_iterations = input$max_iter, seed = input$seed)

      robust_boot_test <- test_mediation(f_test, data = df, robust = TRUE, level = input$Confidence, R = input$boot_samples,
                                         control = control_var)
      })

      ols_bootstrap_test <- eventReactive(input$runOLS,
                                             {
                                               df <- get_data()
                                               f_test <- formula()

                                               ols_bootstrap_test <- test_mediation(f_test, data = df, robust = FALSE, level = input$Confidence, R = input$boot_samples)
                                             })

      output$summaryOLS <- renderPrint({
        summary(ols_bootstrap_test())
      })


      # Renders plot of expected vs empirical weights
      output$plot_weights <- renderPlot({
      robust_boot_simple <- robust_bootstrap_test()

      summary_simple <- summary(robust_boot_simple)
      summary_simple$plot
      })

    # Renders the summary text
    output$summary <- renderPrint({
      robust_boot_simple <- robust_bootstrap_test()
      summary(robust_boot_simple)

      })


    # Generates the UI that allows users to select variables of the uploaded data
    output$selectExplanatory <- renderUI({
      df <- get_data()
      selectInput(inputId='Explanatory', label='Independent variable(s):', choices = colnames(df), multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      df <- get_data()
      selectInput(inputId='Mediators', label='Mediating variable(s):', choices = colnames(df), multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      df <- get_data()
      selectInput(inputId='Response', label='Dependent variable:', choices = colnames(df), multiple = TRUE)
    })

    output$selectControls <- renderUI({
      df <- get_data()
      selectInput(inputId='Covariates', label='Control variables:', choices = colnames(df), multiple = TRUE)
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
        load(input$rdatafile$datapath)

        dataframes <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

        selectInput('rdata_dfname', 'DataFrame:',
                    choices = dataframes)
      }
    })
    output$data_table <-  DT::renderDataTable({get_data()})

    output$robmedversion <- renderText({
      paste('ROBMED Package version: ', toString(packageVersion('robmed')))})
})


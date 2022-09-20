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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

      # Reactive expression to get data; only supports csv for now
      data <- reactive({
        req(input$file)

        ext <- tools::file_ext(input$file$name)
        switch(ext, csv = vroom::vroom(input$file$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )

      })

      # For now supports only all serial or all parallel mediators
      # TODO: - add support for combination of mediator type ?

      formula <- reactive({
        req(input$Explanatory, input$Modeltype, input$Response, input$Mediators)

        explanatory <- paste(input$Explanatory, collapse = '+')
        mediators <- paste('m(', paste(input$Mediators, collapse = ','),', .model = "',input$Modeltype,'")',sep = '')

        controls <- ''
        if(length(input$Covariates) != 0){
          controls <- paste('+','covariates(', paste(input$Covariates, collapse = ','), ')')
        }

        form_out <- as.formula(paste(input$Response, '~', mediators,'+', explanatory, controls))

      })


      # Creates boot_test_mediation object
      robust_bootstrap_test <- eventReactive(input$runRobust,
                                              {
      df <- data()
      f_test <- formula()

      #TODO: control_var should be different if method is not robust. Then use cov_control instead
      control_var = reg_control(efficiency = input$MM_eff, max_iterations = input$max_iter, seed = input$seed)

      robust_boot_test <- test_mediation(f_test, data = df, robust = TRUE, level = input$Confidence, R = input$boot_samples,
                                         control = control_var)
      })

      ols_bootstrap_test <- eventReactive(input$runOLS,
                                             {
                                               df <- data()
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
      df <- data()
      selectInput(inputId='Explanatory', label='Explanatory variables', choices = colnames(df), multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      df <- data()
      selectInput(inputId='Mediators', label='Mediating variable', choices = colnames(df), multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      df <- data()
      selectInput(inputId='Response', label='Response variable', choices = colnames(df), multiple = TRUE)
    })

    output$selectControls <- renderUI({
      df <- data()
      selectInput(inputId='Covariates', label='Control variables', choices = colnames(df), multiple = TRUE)
    })

    output$data_table <- renderDataTable({data()})

})


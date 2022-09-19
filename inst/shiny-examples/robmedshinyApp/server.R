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
      perform_bootstrap_test <- eventReactive(input$run,
                                              {
      df <- data()

      set.seed(input$seed)
      f_test <- formula()
      robust_boot_test <- test_mediation(f_test, data = df, robust = TRUE, level = input$Confidence)
      })


      # Renders plot of expected vs empirical weights
      output$plot_weights <- renderPlot({
      robust_boot_simple <- perform_bootstrap_test()

      summary_simple <- summary(robust_boot_simple)
      summary_simple$plot
      })

    # Renders the summary text
    output$summary <- renderPrint({
      robust_boot_simple <- perform_bootstrap_test()
      summary(robust_boot_simple)

      })


    # Generates the UI that allows users to select variables of the uploaded data
    output$selectExplanatory <- renderUI({
      df <- data()
      selectInput(inputId='Explanatory', label='Explanatory variables', choices = colnames(df), multiple = TRUE)
    })

    output$selectMediator <- renderUI({
      df <- data()
      selectInput(inputId='Mediators', label='Mediating variable', choices = colnames(data()), multiple = TRUE)
    })

    output$selectResponse <- renderUI({
      df <- data()
      selectInput(inputId='Response', label='Response variable', choices = colnames(data()), multiple = TRUE)
    })

    output$selectControls <- renderUI({
      df <- data()
      selectInput(inputId='Covariates', label='Control variables', choices = colnames(data()), multiple = TRUE)
    })

    output$data_table <- renderDataTable({data()})







})

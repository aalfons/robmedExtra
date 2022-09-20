#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(robmed)
library(shinythemes)



# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel( h1("ROBMED", align = 'center')),

    tabsetPanel(
      tabPanel('Data and Model',
                             sidebarLayout(
                               sidebarPanel(
                                h1("Data"),
                                fileInput("file", "Choose CSV File",accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv")),
                                uiOutput('selectUI'),
                                uiOutput('selectResponse'),
                                uiOutput('selectExplanatory'),
                                uiOutput('selectMediator'),
                                uiOutput('selectControls'),

                                selectInput("Modeltype", "Type of mediator",
                                            choices = c('parallel', 'serial')),

                                sliderInput("Confidence", "Confidence level test",
                                            min = 0, max = 1, value = 0.95),
                                numericInput('seed', label = 'Random seed', value = 0)



                             ),

                             mainPanel(
                             dataTableOutput('data_table')
                             )
    )


    ),
      tabPanel("ROBMED",
               sidebarLayout(
                 sidebarPanel(actionButton('runRobust', "Run"),
                              h2("Robust Bootstrap Settings"),
                              numericInput("max_iter", "Maximum number of iterations",
                                           value = 10000),

                              selectInput("MM_eff", "Efficiency of the MM estimator",
                                           choices = c(80, 85, 90, 95)),
                              numericInput('boot_samples', label = 'Number of bootstrap samples', value = 5000),

                 ),


                 # Show a plot of the generated distribution
                 mainPanel(hr(),
                           h3('Diagnostic plot'),
                           plotOutput("plot_weights"),
                           hr(),
                           h3('Model and Test summary'),
                           verbatimTextOutput('summary'),
                           verbatimTextOutput('formula')
                 )
               )

               ),
    tabPanel("OLS Bootstrap",
             sidebarLayout(
               sidebarPanel(
                 h2("OLS Bootstrap Test"),
                 actionButton('runOLS', 'Run'),
                 p("TODO: Settings specific to OLS?")
               ),
               mainPanel(
                 verbatimTextOutput('summaryOLS')
               )
             )


    )


    )
))

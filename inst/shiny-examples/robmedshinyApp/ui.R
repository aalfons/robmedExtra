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
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("sandstone"),

    # Application title
    titlePanel(h1("ROBMED", align = 'center')
               ),

    tabsetPanel(
      tabPanel('Data and Model',
                             sidebarLayout(
                               sidebarPanel(
                                textOutput('robmedversion'),
                                h1("Data"),
                                selectInput('datatype', 'datatype',
                                            choices = c('csv', 'Existing DataFrame',
                                                        'RData')),
                                uiOutput('dataframechoice'),
                                uiOutput('rdatafile_dataframes'),
                                h1("Model"),
                                uiOutput('selectUI'),
                                uiOutput('selectResponse'),
                                uiOutput('selectExplanatory'),
                                uiOutput('selectMediator'),
                                uiOutput('selectControls'),

                                selectInput("Modeltype", "Multiple mediator model:",
                                            choices = c('parallel', 'serial')),
                                h1("Options"),
                                selectInput("rngversion", "Random Number Generator Version:",
                                            choices = c("Current", '4.2.0', '4.1.3',
                                                        '4.1.2', '4.1.1', '4.1.0',
                                                        '4.0.5', '4.0.4', '4.0.3',
                                                        '4.0.2', '4.0.1', '4.0.0',
                                                        '3.6.3', '3.6.2', '3.6.1', '3.6.0',
                                                        '3.5.3', '3.5.2', '3.5.1', '3.5.0')
                                            )
                             ),

                             mainPanel(
                             DT::dataTableOutput('data_table')
                             )
    )

    ),
      tabPanel("ROBMED",
               sidebarLayout(
                 sidebarPanel(actionButton('runRobust', "Run"),
                              h2("Robust Bootstrap Settings"),
                              numericInput("max_iter", "Maximum number of iterations:",
                                           value = 10000),

                              selectInput("MM_eff", "Efficiency at normal distribution:",
                                           choices = c(0.80, 0.85, 0.90, 0.95), selected = 0.85),
                              numericInput('boot_samplesROBMED', label = 'Number of bootstrap samples:', value = 5000),

                              uiOutput('downloadbuttonplot'),
                              br(),
                              uiOutput('downloadbuttonscript'),
                              h1("Options"),
                              sliderInput("ConfidenceROBMED", "Confidence level:",
                                          min = 0, max = 1, value = 0.95),
                              numericInput('seedROBMED', label = 'Seed:', value = 0),
                              uiOutput('downloadbuttontable')


                 ),

                 # Show a plot of the generated distribution
                 mainPanel(hr(),
                           h3('Diagnostic plot'),
                           plotOutput("plot_weights"),
                           hr(),
                           h3('Model and Test summary'),
                           verbatimTextOutput('summary'),
                           textOutput('textWithNewlines')
                 )
               )

               ),
    tabPanel("OLS Bootstrap",
             sidebarLayout(
               sidebarPanel(
                 h2("OLS Bootstrap Test"),
                 actionButton('runOLS', 'Run'),
                 h1("Options"),
                 numericInput('boot_samplesROBMED', label = 'Number of bootstrap samples:', value = 5000),
                 sliderInput("ConfidenceOLS", "Confidence level:",
                             min = 0, max = 1, value = 0.95),
                 numericInput('seedOLS', label = 'Seed:', value = 0),

               ),
               mainPanel(
                 verbatimTextOutput('CI'),
                 h3("Model and Test summary"),
                 verbatimTextOutput('summaryOLS')
               )
             )
      )
    )
))

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
                             ),

                             mainPanel(
                             DT::dataTableOutput('data_table')
                             )
    )

    ),
      tabPanel("ROBMED",
               sidebarLayout(
                 sidebarPanel(h2("Robust Bootstrap Test"),
                              actionButton("runRobust", "Run"),
                              h2("Options"),
                              sliderInput("ConfidenceROBMED", "Confidence level",
                                          min = 0, max = 1, value = 0.95),
                              numericInput("boot_samplesROBMED", label = "Number of bootstrap samples", value = 5000),
                              h2("Random Number Generator"),
                              numericInput("seedROBMED", label = "Seed", value = 0),
                              textInput(inputId = "rng_version_robust", label = "Version",
                                        value = as.character(getRversion())),


                              h2("MM-estimator"),
                              selectInput("MM_eff", "Efficiency at normal distribution",
                                          choices = c(0.80, 0.85, 0.90, 0.95), selected = 0.85),
                              numericInput("max_iter", "Maximum number of iterations",
                                           value = 10000),

                              uiOutput("downloadbuttonplot"),
                              br(),
                              uiOutput("downloadbuttonscript"),
                              uiOutput("downloadbuttontableRobust"),

                 ),

                 # Show a plot of the generated distribution
                 mainPanel(hr(),
                           h3('Diagnostic plot'),
                           plotOutput("plot_weights"),
                           hr(),
                           h3('Model and Test summary'),
                           verbatimTextOutput('summary')
                 )
               )

               ),
    tabPanel("OLS Bootstrap",
             sidebarLayout(
               sidebarPanel(
                 h2("OLS Bootstrap Test"),
                 actionButton('runOLS', 'Run'),
                 h2("Options"),
                 numericInput('boot_samplesOLS', label = 'Number of bootstrap samples', value = 5000),
                 sliderInput("ConfidenceOLS", "Confidence level",
                             min = 0, max = 1, value = 0.95),
                 h2("Random Number Generator"),
                 numericInput('seedOLS', label = 'Seed', value = 0),
                 uiOutput('downloadbuttontableOLS'),
                 textInput(inputId = 'rng_version_ols', label = 'Version',
                           value = as.character(getRversion()))


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

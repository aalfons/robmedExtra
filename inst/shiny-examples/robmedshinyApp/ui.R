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
    titlePanel( h2("ROBMED", align = 'center')),

    tabsetPanel(
      tabPanel("main",
               sidebarLayout(
                 sidebarPanel(actionButton('run', "Run"),
                              h2("Settings"),


                              fileInput("file", "Choose CSV File",accept = c("text/csv",
                                                                             "text/comma-separated-values,text/plain",
                                                                             ".csv")
                              ),

                              uiOutput('selectUI'),
                              uiOutput('selectResponse'),
                              uiOutput('selectExplanatory'),
                              uiOutput('selectControls'),
                              uiOutput('selectMediator'),

                              selectInput("Modeltype", "Type of mediator",
                                          choices = c('parallel', 'serial')),

                              sliderInput("Confidence", "Confidence level test",
                                          min = 0, max = 1, value = 0.95),

                              numericInput('seed', label = 'Random seed', value = 0)
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
    tabPanel('View Data',
             h1("Data")
             )

    )
))

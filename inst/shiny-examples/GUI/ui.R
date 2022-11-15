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
library(shinybusy)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinybusy::add_busy_spinner(spin = "fading-circle", height = "60px",
                                width = "60px"),
    theme = shinytheme("sandstone"),
    title = "ROBMED",

    # Application title
    titlePanel(h1("ROBMED", align = 'center')
               ),

    tabsetPanel(
      tabPanel('Data',
                             sidebarLayout(
                               sidebarPanel(
                                textOutput("robmedversion"),
                                selectInput("datatype", "datatype",
                                            choices = c(
                                                        "Existing data frame",
                                                        "RData"),
                                            selected = "RData"),
                                uiOutput('dataframechoice'),
                                uiOutput('rdatafile_dataframes'),
                             ),

                             mainPanel(
                             DT::dataTableOutput('data_table')
                               )
        )
    ),
    tabPanel("Model",
             sidebarLayout(
               sidebarPanel(
                 uiOutput('selectUI'),
                 uiOutput('selectResponse'),
                 uiOutput('selectExplanatory'),
                 uiOutput('selectMediator'),
                 uiOutput('selectControls'),
                 uiOutput('ui_model_type')
               ),
               mainPanel(

               )
             )
            ),
      tabPanel("ROBMED",
               sidebarLayout(
                 sidebarPanel(uiOutput('ui_runbutton_robmed'),
                              h2("Options"),
                              sliderInput("ConfidenceROBMED", "Confidence level",
                                          min = 0, max = 1, value = 0.95),
                              numericInput("boot_samplesROBMED",
                                           label = "Number of bootstrap samples",
                                           value = 5000),
                              h2("Random Number Generator"),
                              numericInput("seedROBMED", label = "Seed",
                                           value = NULL),
                              textInput(inputId = "rng_version_robust",
                                        label = "Version",
                                        value = as.character(getRversion())),


                              h2("MM-estimator"),
                              selectInput("MM_eff",
                                          "Efficiency at normal distribution",
                                          choices = c(0.80, 0.85, 0.90, 0.95),
                                          selected = 0.85),
                              numericInput("max_iter",
                                           "Maximum number of iterations",
                                           value = 10000),


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
                 uiOutput("ui_runbutton_ols"),
                 h2("Options"),
                 sliderInput("ConfidenceOLS", "Confidence level",
                             min = 0, max = 1, value = 0.95),
                 numericInput('boot_samplesOLS',
                              label = 'Number of bootstrap samples',
                              value = 5000),
                 h2("Random Number Generator"),
                 numericInput('seedOLS', label = 'Seed', value = NULL),
                 textInput(inputId = 'rng_version_ols', label = 'Version',
                           value = as.character(getRversion()))


               ),
               mainPanel(
                 verbatimTextOutput('CI'),
                 h3("Model and Test summary"),
                 verbatimTextOutput('summaryOLS')
               )
             )
      ),
    tabPanel("Export",
             sidebarLayout(
               sidebarPanel(
                 h2("Table"),
                 radioButtons(inputId = "table_orientation",
                              label = "Orientation of tables",
                              choices = c("portrait", "landscape")),
                 downloadButton(outputId = "download_tables",
                                label = "Download tables"),
                 uiOutput("downloadbuttontableRobust"),
                 uiOutput('downloadbuttontableOLS'),
                 h2(" Diagnostic Plot"),
                 radioButtons(inputId = "plot_format",
                              label = 'File format',
                              choices = c("pdf", "png"), selected = "pdf"),
                 radioButtons(inputId = "plot_units",
                              label = "Units of height/width",
                              choices = c("in", "cm"),
                              selected = "in"),
                 uiOutput("ui_resolution"),
                 numericInput(inputId = "width_plot", label = "Width plot",
                              value = 7),
                 numericInput(inputId = "height_plot", label = "Height plot",
                              value = 7),
                 uiOutput("downloadbuttonplot"),
                 h2("R Script"),
                 uiOutput("downloadbuttonscript"),
               ),
               mainPanel(

               )
             )
            )
    )
))

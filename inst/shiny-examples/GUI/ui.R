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
    title = "robmed",

    # Application title
    titlePanel(h1("robmed", align = "center")
               ),

    tabsetPanel(
      tabPanel("Data",
                       sidebarLayout(
                         sidebarPanel(
                          selectInput("datatype", "Data source",
                                      choices = c("R environment",
                                                  "RData file"),
                                      selected = "R environment"),
                          uiOutput("dataframechoice"),
                          uiOutput("rdatafile_dataframes"),
                          uiOutput("save_rdata_ui")
                       ),

                       mainPanel(
                       DT::dataTableOutput("data_table")
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
                 h2("Notation"),
                 p("Y: Dependent variable"),
                 p("X: Independent variable"),
                 p("M: Mediator"),

                 h2("Simple mediation model"),
                 p("The mediation model in its simplest form looks as follows:"),
                 img(src = "mediation-simple.png", height = 120, width = 260),

                 h2("Parallel multiple mediator model"),
                 p(HTML(paste0("The simple mediation model can be extended with
                 multiple parallel mediators. In the parallel multiple mediator
                 model, an independent variable X is hypothesized to influence a
                 dependent variable Y through multiple mediators M",tags$sub(1),
                               ", ..., M",  tags$sub("k"),
                 " while the mediator variables do not influence each other."))),
                 img(src = "mediation-parallel.png", height = 140, width = 260),

                 h2("Serial multiple mediator model"),
                 p("The serial multiple mediator model differs from the parallel
                    multiple mediator model in that it allows the hypothesized
                    mediators to influence each other in a sequential manner."),
                 img(src = "mediation-serial.png", height = 140, width = 260),


                 h2("Multiple independent variables"),
                 p("All the above models can extended by allowing multiple
                   independent variables."),
                 img(src = "mediation-multiple.png", height = 140, width = 260),

                 h2("Control variables"),
                 p("To isolate the effects of the independent variables of
                   interest from other factors, control variables can be added
                   to all regression equations of a mediation model. Note that
                   that there is no intrinsic difference between independent
                   variables of interest and control variables in terms of the
                   model or its estimation. The difference is purely conceptual
                   in nature: for the control variables, the estimates of the
                   direct and indirect paths are not of particular interest to
                   the researcher. Control variables can therefore be specified
                   separately from the independent variables of interest.
                   Only for the latter, results for the indirect effects
                   are included in the output.")
               )
             )
            ),
      tabPanel("robmed",
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
                           value = as.character(getRversion())
                           )
               ),
               mainPanel(
                 verbatimTextOutput("CI"),
                 h3("Model and Test summary"),
                 verbatimTextOutput('summaryOLS')
               )
             )
      ),
    tabPanel("Export",
             sidebarLayout(
               sidebarPanel(
                 h2("Table"),
                 uiOutput("ui_checkbox_table"),
                 radioButtons(inputId = "table_orientation",
                              label = "Orientation",
                              choices = c("portrait", "landscape")),
                 downloadButton(outputId = "download_tables",
                                label = "Download table(s)"),
                 checkboxInput(inputId = "include_pval",
                               label = p("Include p-values in table",
                                         span("(Computing p-values takes time)",
                                              style = "color: 	#A0A0A0")),
                               value = TRUE),
                 uiOutput("checkbox_latex_table"),
                 h2("Diagnostic Plot"),
                 radioButtons(inputId = "plot_format",
                              label = "File format",
                              choices = c("pdf", "png"), selected = "pdf"),
                 radioButtons(inputId = "plot_units",
                              label = "Units of height/width",
                              choices = c("in", "cm"),
                              selected = "in"),
                 numericInput(inputId = "width_plot", label = "Width",
                              value = 7),
                 numericInput(inputId = "height_plot", label = "Height",
                              value = 7),
                 uiOutput("ui_resolution"),
                 uiOutput("downloadbuttonplot"),
                 h2("R Script"),
                 uiOutput("downloadbuttonscript"),
               ),
               mainPanel(uiOutput("button_latex_robust"),
                 verbatimTextOutput("text_latex_robust"),
                 uiOutput("button_latex_ols"),
                 verbatimTextOutput("text_latex_ols")
               )
             )
            ),
    tabPanel("Info",
             sidebarLayout(
               sidebarPanel(

               ),
               mainPanel(
                 h2("Package Version"),
                 textOutput("robmedversion"),
                 p("GUI version 0.1.0"),
                 h2("Citation"),
                 verbatimTextOutput("citation_text")
               )
             )

    )
    )
))

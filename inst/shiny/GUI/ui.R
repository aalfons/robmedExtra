# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Load required packages -----
# we only load packages for which we don't use the :: operator to call functions
# (to keep the namespace clean)
library("shiny")


# User interface definition for GUI -----

#' @import shiny
#' @importFrom DT dataTableOutput

shinyUI(fluidPage(
  title = "ROBMED",

  # header for shiny app
  titlePanel(
    h1("ROBMED: Robust Mediation Analysis", align = "center")
  ),

  # set of tabs for various functionality
  tabsetPanel(

    # tab for selecting the data
    tabPanel(
      "Data",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
          # header for the data selection sidebarpanel
          h3("Import your data"),
          # input for selecting a data source
          uiOutput("select_data_source"),
          # input for selecting a data frame from the global environment
          uiOutput("select_df_global"),
          # inputs for selecting a data frame from an RData file
          uiOutput("select_Rdata_file"),
          uiOutput("select_df_RData"),
          # element to show any error messages for the selected data frame
          uiOutput("error_data")
        ),
        # output panel on right hand side
        mainPanel(
          # show the selected data set
          DT::dataTableOutput("data_table")
        )
      )
    ),

    # tab for specifying the model
    tabPanel(
      "Model",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
          # header for the model specification sidebarpanel
          h3("Choose your variables"),
          # inputs for variables and type of mediation model
          uiOutput("select_variables"),
          uiOutput("select_model")
        ),
        # output panel on right hand side
        mainPanel(

          h3("Simple mediation model"),
          p("In the simplest form of a mediation model, an independent",
            "variable X is hypothesized to influence a dependent variable",
            "Y indirectly through a mediator M."),
          img(src = "mediation-simple.png"),

          h3("Parallel multiple mediator model"),
          p("The simple mediation model can be extended with multiple",
            "parallel mediators. In the parallel multiple mediator model,",
            "an independent variable X is hypothesized to influence a",
            "dependent variable Y through multiple mediators",
            HTML("M<sub>1</sub>, ..., M<sub>k</sub>,"),
            "while the mediators do not influence each other."),
          img(src = "mediation-parallel.png"),

          h3("Serial multiple mediator model"),
          p("The serial multiple mediator model differs from the parallel",
            "multiple mediator model in that it allows the hypothesized",
            "mediators", HTML("M<sub>1</sub>, ..., M<sub>k</sub>"),
            "to influence each other in a sequential manner.",
            "The serial multiple mediator model quickly grows in complexity",
            "with increasing number of mediators due to the combinatorial",
            "increase in indirect paths through the mediators. It is",
            "therefore only implemented for two and three mediators to",
            "maintain a focus on easily interpretable models."),
          img(src = "mediation-serial-two.png"),
          img(src = "mediation-serial-three.png"),

          h3("Multiple independent variables to be mediated"),
          p("The simple mediation model can also be extended by allowing",
            "multiple independent variables",
            HTML("X<sub>1</sub>, ..., X<sub>l</sub>"),
            "to influence the dependent variable Y through a hypothesized",
            "mediator M. Note that an important special case of this model",
            "occurs when a categorical independent variable is represented",
            "by a group of dummy variables."),
          img(src = "mediation-multiple.png"),

          h3("Control variables"),
          p("To isolate the effects of the independent variables of interest",
            "from other factors, control variables can be added to all",
            "regression equations of a mediation model. Note that that there",
            "is no intrinsic difference between independent variables of",
            "interest and control variables in terms of the model or its",
            "estimation. The difference is purely conceptual in nature: for",
            "the control variables, the estimates of the direct and indirect",
            "paths are not of particular interest to the researcher. Control",
            "variables can therefore be specified separately from the",
            "independent variables of interest. Only for the latter, results",
            "for the indirect effects are included in the output."),

          h3("More complex models"),
          p("Some of the models described above can be combined, for instance",
            "parallel and serial multiple mediator models support multiple",
            "independent variables of interest and control variables.")

        )
      )
    ),

    # tab for performing ROBMED
    tabPanel(
      "ROBMED",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
          # header for the ROBMED sidebarpanel
          h3("Run Robust Mediation Analysis"),
          # inputs for various options
          uiOutput("options_ROBMED"),
          uiOutput("advanced_options_ROBMED")
        ),
        # output panel on right hand side
        mainPanel(
          # diagnostic plot
          uiOutput("plot_ROBMED_header"),
          # TODO: allow plot height to scale with the number of regressions
          plotOutput("plot_ROBMED", height = "450px"),
          # model and test summaries
          uiOutput("summary_ROBMED_header"),
          verbatimTextOutput("summary_ROBMED")
        )
      )
    ),

    # tab for performing OLS bootstrap
    tabPanel(
      "OLS Bootstrap",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
          # header for the OLS Bootstrap sidebarpanel
          h3("Run Mediation Analysis using OLS Bootstrap test"),
          # inputs for various options
          uiOutput("options_OLS_boot"),
          uiOutput("advanced_options_OLS_boot")
        ),
        # output panel on right hand side
        mainPanel(
          # model and test summaries
          uiOutput("summary_OLS_boot_header"),
          verbatimTextOutput("summary_OLS_boot")
        )
      )
    ),

    # tab for exporting results
    tabPanel(
      "Export",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
          # header for the export sidebarpanel
          h3("Export your results"),
          # information on export files
          h5("Results will be exported as a .zip file containing:"),
          HTML("<ul>
          <li> Plots of the results in the chosen filetypes (.png, .pdf) </li>
          <li> Tables of the results in the chosen filetypes (.docx, .ppt) </li>
          <li> Script used to generate the results as a .R file </li>
          <li> Data used for the tests as a .RData file </li>
          </ul>"),
          # buttons to generate/downlaod files and inputs for table options
          uiOutput("select_file_type_table"),
          uiOutput("options_table"),
          # uiOutput("select_orientation"),
          # input for plot options
          uiOutput("select_file_type_plot"),
          uiOutput("options_plot"),
          uiOutput("select_resolution")
        ),
        # output panel on right hand side
        mainPanel(
          # table preview
          uiOutput("table_preview_header"),
          uiOutput("table_preview"),
          # file preview for diagnostic plot
          uiOutput("plot_preview_header"),
          plotOutput("plot_preview")
        )
      )
    ),

    # tab for information on the shiny app
    tabPanel(
      "Info",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
          h3("Download references"),
          selectInput("citation_format", "Citation format",
                      choices = c("EndNote", "BibTeX"),
                      selected = "EndNote", multiple = FALSE),
          downloadButton("download_references", "Download references")
        ),
        # output panel on right hand side
        mainPanel(

          # information on software versions
          h3("Version"),
          uiOutput("version_info"),

          # citation information
          h3("Citation"),
          uiOutput("citation_info")

        )
      )
    )

  )

))

# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Internal functions -----

# function to construct labels for variable selection inputs
get_label <- function(label, info) {
  # FIXME: color is hard-coded to be the same as help text in in bootstrap theme
  p(label, span(info, style = "color: #737373; font-weight:normal;"))
}


# User interface definition for GUI -----

#' @import shiny
#' @importFrom DT dataTableOutput

shinyUI(fluidPage(
  title = "robmed",

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
          uiOutput("select_data_source"),
          uiOutput("select_Rdata_file"),
          uiOutput("select_df_global"),
          uiOutput("select_df_RData")
        ),
        # output panel on right hand side
        mainPanel(

          # # for testing whether inputs are handled correctly
          # textOutput("test_data_source"),
          # textOutput("test_RData_file"),
          # textOutput("test_df_name"),

          # show selected data set
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
          selectInput("response",
                      label = get_label("Dependent variable", "(Numeric)"),
                      choices = "", selected = NULL, multiple = FALSE),
          selectInput("explanatory", label = "Independent variable(s)",
                      choices = character(), selected = NULL, multiple = TRUE),
          selectInput("mediators",
                      label = get_label("Mediator(s)", "(Numeric)"),
                      choices = character(), selected = NULL, multiple = TRUE),
          selectInput("covariates", label = "Covariate(s)",
                      choices = character(), selected = NULL, multiple = TRUE)
        ),
        # output panel on right hand side
        mainPanel(

          # # for testing whether inputs are handled correctly
          # textOutput("test_y"),
          # textOutput("test_x"),
          # textOutput("test_m"),
          # textOutput("test_covariates")

          h2("Simple mediation model"),
          p("In the simplest form of a mediation model, an independent",
            "variable X is hypothesized to influence a dependent variable",
            "Y indirectly through a mediator M."),
          img(src = "mediation-simple.png"),

          h2("Parallel multiple mediator model"),
          p("The simple mediation model can be extended with multiple",
            "parallel mediators. In the parallel multiple mediator model,",
            "an independent variable X is hypothesized to influence a",
            "dependent variable Y through multiple mediators",
            HTML("M<sub>1</sub>, ..., M<sub>k</sub>,"),
            "while the mediators do not influence each other."),
          img(src = "mediation-parallel.png"),

          h2("Serial multiple mediator model"),
          p("The serial multiple mediator model differs from the parallel",
            "multiple mediator model in that it allows the hypothesized",
            "mediators", HTML("M<sub>1</sub>, ..., M<sub>k</sub>"),
            "to influence each other in a sequential manner.",
            "The serial multiple mediator model quickly grows in complexity",
            "with increasing number of mediators due to the combinatorial",
            "increase in indirect paths through the mediators. It is",
            "therefore only implemented for two and three mediators to",
            "maintain a focus on easily interpretable models.",
            # "The diagram below visualizes a serial multiple model with two",
            # "mediators."
            ),
          img(src = "mediation-serial-two.png"),
          img(src = "mediation-serial-three.png"),


          h2("Multiple independent variables to be mediated"),
          p("The simple mediation model can also be extended by allowing",
            "multiple independent variables",
            HTML("X<sub>1</sub>, ..., X<sub>l</sub>"),
            "to influence the dependent variable Y through a hypothesized",
            "mediator M. Note that an important special case of this model",
            "occurs when a categorical independent variable is represented",
            "by a group of dummy variables."),
          img(src = "mediation-multiple.png"),

          h2("Control variables"),
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

          h2("More complex models"),
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
        ),
        # output panel on right hand side
        mainPanel(
        )
      )
    ),

    # tab for performing OLS bootstrap
    tabPanel(
      "OLS Bootstrap",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
        ),
        # output panel on right hand side
        mainPanel(
        )
      )
    ),

    # tab for exporting results
    tabPanel(
      "Export",
      sidebarLayout(
        # input panel on left hand side
        sidebarPanel(
        ),
        # output panel on right hand side
        mainPanel(
        )
      )
    ),

    # tab for information on the shiny app
    tabPanel(
      "Info",
      fillPage(
          h2("Package versions"),
          p(strong("robmed:"), toString(packageVersion("robmed"))),
          p(strong("robmedExtra:"), toString(packageVersion("robmedExtra")))
      )
    )

  )

))

# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************


# Internal functions -----

# internal function to construct labels for variable selection inputs
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
          # TODO: by default, "RData file" should be selected if there are no
          #       data frames in the global environment, otherwise "R environment"
          selectInput("data_source", "Data source",
                      choices = c("R environment", "RData file"),
                      selected = "R environment", multiple = FALSE),
          uiOutput("select_Rdata_file"),
          uiOutput("select_data_frame")
        ),
        # output panel on right hand side
        mainPanel(
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
          textOutput("test_y"),
          textOutput("test_x"),
          textOutput("test_m"),
          textOutput("test_covariates")
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

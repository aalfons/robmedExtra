# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
#
# based on code by Vincent Drenth
# ************************************

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
          selectInput("data_source", "Data source",
                      choices = c("R environment",
                                  "RData file"),
                      selected = "R environment")
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
        ),
        # output panel on right hand side
        mainPanel(
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

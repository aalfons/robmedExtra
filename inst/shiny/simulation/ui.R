# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# Load required packages -----
# we only load packages for which we don't use the :: operator to call functions
# (to keep the namespace clean)
library("shiny")


# Define relevant function and objects -----

# function to construct labels for UI inputs
get_label <- function(label, info) {
  # FIXME: color is hard-coded to be the same as in help text in bootstrap theme
  p(label, span(info, style = "color: #737373; font-weight:normal;"))
}

# label for inputs for outlier shift
shift_label <- get_label("Shift", "(in standard deviations)")

# reference for ROBMED (since it's shown in all tabs)
ROBMED_reference <- p(
  HTML("Alfons, A., Ate&scedil;, N. Y., & Groenen, P. J. F. (2022)."),
  "A robust bootstrap test for mediation analysis.",
  HTML("<em>Organizational Research Methods</em>, <em>25</em>(3),",
       "591&ndash;617."),
  a("https://doi.org/10.1177/1094428121999096",
    href = "https://doi.org/10.1177/1094428121999096")
)


# User interface definition for GUI -----

#' @import shiny

shinyUI(fluidPage(
  title = "Mediation Analysis",

  # header for shiny app
  titlePanel(
    h1("Mediation Analysis: Simulation with Outliers", align = "center")
  ),

  # set of tabs for various functionality
  tabsetPanel(

    # tab for specifying the model parameters and number of observations
    tabPanel(
      "Data",

      # first row with inputs and outputs
      fluidRow(
        # input panel on left hand side
        sidebarPanel(
          sliderInput("n_observations", "Number of observations",
                      min = 50, max = 1000, value = 100, step = 10),
          sliderInput("a", "Path a", min = -1, max = 1, value = 0.4,
                      step = 0.01),
          sliderInput("b", "Path b", min = -1, max = 1, value = 0.4,
                      step = 0.01),
          sliderInput("c", "Path c", min = -1, max = 1, value = 0.4,
                      step = 0.01),
          numericInput("seed", "Seed of the random number generator",
                       value = as.integer(format(Sys.Date(), "%Y%m%d")))
        ),
        # output panel on right hand side
        mainPanel(
          uiOutput("header_plot_clean_data"),
          plotOutput("plot_clean_data")
        )
      ),

      # second row with description
      fluidRow(
        column(
          width = 12,
          # shiny supports MathJax, but we need to configure inline expressions
          withMathJax(),
          div(HTML("
                   <script type='text/x-mathjax-config'>
                   MathJax.Hub.Config({
                   tex2jax: {inlineMath: [['$','$']]}
                   });
                   </script>
                   ")),
          # description of the data generating process
          h3("Description"),
          p("The data generating process and default settings are similar to",
            "one of the simulation designs in Alfons, Ate\U015F & Groenen",
            "(2022). The observations are simulated according to the",
            "mediation model",
            "$$ \\begin{align}
             M &= a X + e_{1}, \\\\
             Y &= b M + c X + e_{2},
             \\end{align} $$",
            "where the independent variable $X$ as well as the error terms",
            "$e_{1}$ and $e_{2}$ are generated from a standard normal",
            "distribution."),
          h3("References"),
          ROBMED_reference
        )
      )

    ),

    # tab for specifying the outlier settings
    tabPanel(
      "Outliers",

      # first row with inputs and outputs
      fluidRow(
        # input panel on left hand side
        sidebarPanel(
          uiOutput("select_n_outliers"),
          h4("Independent variable (X)"),
          sliderInput("scaling_X", "Scaling factor",
                      min = 0, max = 10, value = 1, step = 0.01),
          sliderInput("shift_X", shift_label,
                      min = -6, max = 6, value = 0, step = 0.01),
          h4("Mediator (M)"),
          sliderInput("scaling_M", "Scaling factor",
                      min = 0, max = 10, value = 0.1, step = 0.01),
          sliderInput("shift_M", shift_label,
                      min = -6, max = 6, value = -3, step = 0.01),
          h4("Dependent variable (Y)"),
          sliderInput("scaling_Y", "Scaling factor",
                      min = 0, max = 10, value = 0.1, step = 0.01),
          sliderInput("shift_Y", shift_label,
                      min = -6, max = 6, value = 3, step = 0.01)
        ),
        # output panel on right hand side
        mainPanel(
          uiOutput("header_plot_contaminated_data"),
          plotOutput("plot_contaminated_data")
        )
      ),

      # second row with description
      fluidRow(
        column(
          width = 12,
          h3("Description"),
          p("The outlier generating process and default settings are similar",
            "to those in Alfons, Ate\U015F & Groenen (2022). Outliers are",
            "generated by scaling and shifting a subset of the simulated",
            "observations. Let $X_{i}$, $M_{i}$, and $Y_{i}$ denote the",
            "$i$-th observation in the independent variable $X$,",
            "the mediator $M$, and the dependent variable $Y$, respectively.",
            "Furthermore, let $\\sigma_{X}$, $\\sigma_{M}$, and $\\sigma_{Y}$",
            "denote the true standard deviations of the corresponding",
            "variables, $\\delta_{X}$, $\\delta_{M}$, and $\\delta_{Y}$ the",
            "outlier scaling factors, and $\\tau_{X}$, $\\tau_{M}$, and",
            "$\\tau_{Y}$ the outlier shifts. Then the outliers are generated",
            "as",
            "$$ \\begin{align}
             X^{*}_{i} &= \\delta_{X} X_{i} + \\tau_{i} \\sigma_{X}, \\\\
             M^{*}_{i} &= \\delta_{M} M_{i} + \\tau_{i} \\sigma_{M}, \\\\
             Y^{*}_{i} &= \\delta_{Y} Y_{i} + \\tau_{i} \\sigma_{Y}.
             \\end{align} $$"),
          h3("References"),
          ROBMED_reference
        )
      )

    ),

    # tab for selecting the methods
    tabPanel(
      "Methods",

      # first row with inputs and outputs
      fluidRow(
        # input panel on left hand side
        sidebarPanel(
          checkboxGroupInput("methods", "Methods to compare",
                             choices = c("OLS bootstrap" = "ols_boot",
                                         "Winsorized bootstrap" = "winsorized_boot",
                                         "Median bootstrap" = "median_boot",
                                         "ROBMED"),
                             selected = c("ols_boot", "ROBMED")),
          sliderInput("R", "Number of bootstrap samples", min = 1000,
                      max = 10000, value = 5000, step = 100),
        ),
        # output panel on right hand side
        mainPanel(
          uiOutput("header_density_plot"),
          plotOutput("density_plot")
        )
      ),

      # second row with description
      fluidRow(
        column(
          width = 12,
          h3("Description"),
          p("You can compare various bootstrap methods for mediation analysis:",
            "the OLS bootstrap test of Preacher & Hayes (2004, 2008), the",
            "winsorized bootstrap test of Zu & Yuan (2010), the bootstrap",
            "test based on median regression of Yuan & MacKinnon (2014), and ",
            "the robust boostrap test (ROBMED) of Alfons, Ate\U015F & Groenen",
            "(2022)."),
          p("For each selected method, the bootstrap distribution of the",
            "indirect effect is shown together with a shaded area",
            "representing the 95% confidence interval.  If the app does not",
            "update quickly enough, try to reduce the number of bootstrap",
            "samples."),
          h3("References"),
          ROBMED_reference,
          p("Preacher, K. J. and Hayes, A. F. (2004) SPSS and SAS procedures",
            "for estimating indirect effects in simple mediation models.",
            HTML("<em>Behavior Research Methods, Instruments, &",
                 "Computers</em>,", "<em>36</em>(4),", "717&ndash;731."),
            a("https://doi.org/10.3758/BF03206553",
              href = "https://doi.org/10.3758/BF03206553")),
          p("Preacher, K. J. and Hayes, A. F. (2008) Asymptotic and",
            "resampling strategies for assessing and comparing indirect",
            "effects in multiple mediator models.",
            HTML("<em>Behavior Research Methods</em>, <em>40</em>(3),",
                 "879&ndash;891."),
            a("https://doi.org/10.3758/BRM.40.3.879",
              href = "https://doi.org/10.3758/BRM.40.3.879")),
          p("Yuan, Y. and MacKinnon, D. P. (2014) Robust mediation analysis",
            "based on median regression.",
            HTML("<em>Psychological Methods</em>, <em>19</em>(1),",
                 "1&ndash;20."),
            a("https://doi.org/10.1037/a0033820",
              href = "https://doi.org/10.1037/a0033820")),
          p("Zu, J. and Yuan, K.-H. (2010) Local influence and robust",
            "procedures for mediation analysis.",
            HTML("<em>Multivariate Behavioral Research</em>, <em>45</em>(1),",
                 "1&ndash;44."),
            a("https://doi.org/10.1080/00273170903504695",
              href = "https://doi.org/10.1080/00273170903504695"))
        )
      )

    )

  )

))

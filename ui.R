
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(AER)

data("CASchools")

shinyUI(fluidPage(

  includeCSS("http://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.1.1/gh-fork-ribbon.min.css"),
  includeCSS("github_fork.css"),
  includeHTML("github_fork.html"),
  
  # Application title
  fluidRow(
    column(width = 1),
    column(width = 11,
           titlePanel("Linear Regression")
    )
  ),

  h4("California Test Score Dataset", align = "center"),
  fluidRow(
    column(width = 7,
           ggvisOutput('plot')),
    column(width = 5,
           align = "right",
           tableOutput("coefficients"),
           fluidRow(
             column(width = 8,
                    align = "right",
                    tableOutput("r_squared"),
                    ggvisOutput('tdist_plot')
             ),
             column(width = 4,
                    align = "right",
                    tableOutput("residuals")
             )
           ),
           ggvisOutput('pvalue_plot')
    )
  ),

  hr(),
  
  fluidRow(
    column(width = 3,
           sliderInput('sample_size', 'Sample Size', 
                       min = 2, 
                       max = nrow(CASchools), 
                       value = 10, 
                       step = 5,
                       animate = animationOptions(interval = 200))
    ),
    column(width = 2,
           actionButton("resample", label = "Resample", icon = icon("refresh"))
    ),
    column(width = 4,
           sliderInput('confidence_interval', 'Confidence Interval (%)', 
                       min = 0, 
                       max = 99.9, 
                       value = 95, 
                       step = 0.1)
    ),
    column(width = 2,
           checkboxGroupInput("show_residuals", label = "Show Residuals", 
                              choices = list("Lines" = 1, "Values" = 2),
                              selected = 1)
    )
  )
))


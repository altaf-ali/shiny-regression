
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(AER)

data("CASchools")
dataset <- CASchools

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Linear Regression"),

  h4("California Schools", align = "center"),
  fluidRow(
    column(width = 7,
           ggvisOutput('plot')),
    column(width = 5,
           tableOutput("coefficients"),
           tableOutput("r_squared"),
           tableOutput("residuals"))
  ),

  hr(),
  
  fluidRow(
    column(width = 4,
           sliderInput('sample_size', 'Sample Size', 
                       min = 2, 
                       max = nrow(dataset), 
                       value = 10, 
                       step = 5,
                       animate = animationOptions(interval = 200))
    ),
    column(width = 4,
           sliderInput('confidence_interval', 'Confidence Interval (%)', 
                       min = 0, 
                       max = 99.9, 
                       value = 95, 
                       step = 0.1)
    ),
    column(width = 4,
           checkboxGroupInput("show_residuals", label = "Show Residuals", 
                              choices = list("Lines" = 1, "Values" = 2),
                              selected = 1)
    )
  )
))


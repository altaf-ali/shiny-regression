
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

  tabsetPanel(
    tabPanel("Regression", 
             h4("California Schools", align = "center"),
             fluidRow(
               column(width = 8,
                      ggvisOutput('plot')),
               column(width = 4,
                      tableOutput("coefficients"),
                      tableOutput("r_squared"))
             )
    ),
    tabPanel("Dataset",
             dataTableOutput("dataset_table")
    )
  ),

  hr(),
  
  fluidRow(
    column(width = 6,
           sliderInput('sample_size', 'Sample Size', 
                       min=3, 
                       max=nrow(dataset), 
                       value=10, 
                       step=5,
                       animate = animationOptions(interval = 200))
    ),
    column(width = 6,
           checkboxGroupInput("show_residuals", label = "Show Residuals", 
                              choices = list("Lines" = 1, "Values" = 2),
                              selected = 1)
    )
  )
))


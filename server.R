# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(plyr)
library(tidyr)
library(dplyr)

data("CASchools")    

axis_range <- function(axis, accuracy) {
  c(round_any(min(axis), accuracy, f = floor), round_any(max(axis), accuracy, f = ceiling))
}

set.seed(666) # use 111 to see upward sloping line initially
dataset <- CASchools[sample(1:nrow(CASchools), nrow(CASchools)),]
dataset$Student.Teacher.Ratio <- dataset$students / dataset$teachers
dataset$score <- (dataset$math + dataset$read) / 2

str_domain <- axis_range(dataset$Student.Teacher.Ratio, 2)
score_domain <- axis_range(dataset$score, 20)

shinyServer(function(input, output, session) {

  data_sampler <- reactive({
    dataset[1:input$sample_size,]
  })
  
  linear_model <- reactive({
    lm(score ~ Student.Teacher.Ratio, data = data_sampler()) 
  })
  
  output$coefficients <- renderTable({
    results <- summary(linear_model())$coefficients
    colnames(results) <- c("Estimate", "Std. Error", "t-statistic", "p-value")
    results
  })
  
  output$r_squared <- renderTable({
    model_summary <- summary(linear_model())
    results <- data.frame(R2=model_summary$r.squared, Adj.R2=model_summary$adj.r.squared)
    rownames(results) <- c("value")
    colnames(results) <- c("R-squared", "Adjusted R-squared")
    results
  })  
  
  output$residuals <- renderTable({
    residuals_summary <- summary(residuals(linear_model()))
    results <- data.frame(row.names = names(residuals_summary))
    results$Residuals <- residuals_summary
    results
  })
  
  data_modeler <- reactive({
    dataset <- data_sampler()
    model <- linear_model() 
    
    dataset <- dataset %>%
      dplyr::mutate(predictions = predict(model)) %>%
      dplyr::mutate(residuals = signif(residuals(model), 5)) %>%
      tidyr::gather(Score.Type, Score.Value, c(score, predictions)) %>%
      dplyr::mutate(score = ifelse(Score.Type == "score", Score.Value, NA)) %>%
      dplyr::mutate(predictions = ifelse(Score.Type == "predictions", Score.Value, NA)) %>%
      dplyr::mutate(predictions = ifelse(is.na(predictions), score, predictions)) %>%
      dplyr::select(district, Student.Teacher.Ratio, score, predictions, residuals) 
    
    if (!any(input$show_residuals == 1)) {
      dataset <- dataset %>% 
        dplyr::mutate(predictions = min(predictions))
    }

    if (!any(input$show_residuals == 2)) {
      dataset <- dataset %>% 
        dplyr::mutate(residuals = 0)
    }

    dataset
  })
  
  reactive({
    data_modeler %>%
      ggvis(x = ~Student.Teacher.Ratio, y = ~score) %>%
      set_options(height = 450, width = 800, duration = 0) %>%
      add_axis("x", title = "Student-Teacher Ratio") %>% 
      scale_numeric("x", domain = str_domain, nice = TRUE) %>% 
      add_axis("y", title = "Test Score") %>% 
      scale_numeric("y", domain = score_domain, nice = TRUE) %>% 
      group_by(district) %>% 
      layer_paths(x = ~Student.Teacher.Ratio, y = ~predictions,  stroke := "blue") %>% 
      ungroup() %>% 
      filter(!is.na(score)) %>%
      layer_points(stroke := "black", size = 0.5, fill := "gray") %>%
      layer_model_predictions(model = "lm", stroke := "red", se = TRUE, model_args = list(level = input$confidence_interval / 100)) %>%
      filter(residuals != 0) %>%
      layer_text(text := ~residuals)
  }) %>%
  bind_shiny("plot")
  
})

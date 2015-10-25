
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(calibrate)
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

STR_range <- axis_range(dataset$Student.Teacher.Ratio, 2)
score_range <- axis_range(dataset$score, 20)

shinyServer(function(input, output, session) {

  data_sampler <- function(sample_size) {
    dataset[1:sample_size,]
  }
  
  linear_model <- function(dataset) {
    lm(score ~ Student.Teacher.Ratio, data = dataset) 
  }
  
  output$coefficients <- renderTable(summary(linear_model(data_sampler(input$sample_size))))
  output$r_squared <- renderTable({
    model_summary <- summary(linear_model(data_sampler(input$sample_size)))
    result <- data.frame(R2=model_summary$r.squared, Adj.R2=model_summary$adj.r.squared)
    rownames(result) = c("value")
    colnames(result) = c("R-squared", "Adjusted R-squared")
    result
  })  
  
  data_modeler <- reactive({
    dataset <- data_sampler(input$sample_size)
    model <- linear_model(dataset) 
    
    #segments(dataset$dataset, dataset$score, dataset$Student.Teacher.Ratio, pred, col="red")
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
        dplyr::mutate(residuals = "")
    }

    dataset
  })
  
  #linear_model <- reactive({
  #  dataset <- data_sampler()
  #  Student.Teacher.Ratio <- dataset$Student.Teacher.Ratio
  #  lm(dataset$score ~ Student.Teacher.Ratio)    
  #})
  
  layer_residuals <- function(vis, ...) {
    write("===layer_residuals", file = "/tmp/shiny.log", append = TRUE)
    write(sprintf("  sample size: %d", sample_size), file = "/tmp/shiny.log", append = TRUE)
    pipeline <- function(vis) {
      cat("XXXXXXX\n")    
      sample_size <<- sample_size + 5
      #dataset <- get_data(vis)$".0"
      dataset <- dataset[1:sample_size,]
      
      model <- lm(score ~ Student.Teacher.Ratio, data = dataset) 
      dataset$predictions <- predict(model)
      dataset <- dataset %>%
        tidyr::gather(Score.Type, Score.Value, c(score, predictions)) %>%
        group_by(district)
      
      vis %>% 
        group_by(district) %>% 
        layer_paths(x = ~Student.Teacher.Ratio, y = ~predictions,  stroke := "blue", ...) %>% 
        ungroup()
    }
    
    layer_f(vis, pipeline)
  }
  
  data_modeler %>%
    ggvis(x = ~Student.Teacher.Ratio, y = ~score) %>%
    set_options(height = 500, width = 800, duration = 0) %>%
    add_axis("x", title = "Student-Teacher Ratio") %>% 
    scale_numeric("x", domain = STR_range) %>% 
    add_axis("y", title = "Test Score") %>% 
    scale_numeric("y", domain = score_range) %>% 
    group_by(district) %>% 
    layer_paths(x = ~Student.Teacher.Ratio, y = ~predictions,  stroke := "blue") %>% 
    ungroup() %>% 
    filter(!is.na(score)) %>%
    layer_points(stroke := "black", size = 0.5, fill := "gray") %>%
    layer_text(text := ~residuals)  %>%
    layer_model_predictions(model = "lm", stroke := "red", se = TRUE, model_args = list(level = 0.95)) %>%
    
    #layer_paths(x = ~str, y = ~score, data = dfx)  %>%
#     layer_f(function(vis) {
#       data <- get_data(vis)$".0"
#       data %>%
#         filter()
#       vis %>%
#         gather(Score.Type, Score.Value, data$Student.Teacher.Ratio:data$score) %>%
#       #  group_by(district) %>%
#         layer_lines()
#     }) %>%
#     
#    gather(Score.Type, Score.Value, ~Student.Teacher.Ratio:~score) %>%
#     layer_f(function(vis) {
#       vis %>%
#         gather(Score.Type, Score.Value, ~Student.Teacher.Ratio:~score) %>%
#         group_by(district) %>%
#         layer_lines() %>%
#        ungroup() %>%
#        spread(TYPE, VAL)
#    }) %>%
    #emit_paths(x = ~Student.Teacher.Ratio, y = ~score, x2 = ~Student.Teacher.Ratio, y2 = ~predictions) %>%
    bind_shiny("plot")
  
#   output$plot1 <- renderPlot({
#     dataset <- data_sampler()
#     
#     #par(bg = "white", cex.main = 1.5, cex.lab = 1.3)
#     
#     plot(dataset$Student.Teacher.Ratio, 
#          dataset$score, 
#          frame = FALSE,
#          xlim = Student.Teacher.Ratio_range, 
#          ylim = score_range,
#          xlab = "Student-Teacher Ratio",
#          ylab = "Test Score",
#          main = "California Schools")
#     
#     model <- linear_model()
#     abline(model, lwd = 2, col = "blue")
#     
#     if (1 %in% input$show_residuals) {
#       pred = predict(model)
#       segments(dataset$Student.Teacher.Ratio, dataset$score, dataset$Student.Teacher.Ratio, pred, col="red")
#     }
#     
#     if (2 %in% input$show_residuals) {
#       res <- signif(residuals(model), 5)    
#       textxy(dataset$Student.Teacher.Ratio, dataset$scor, res, cex=1)    
#     }
#   })

#   output$coefficients <- renderTable(summary(linear_model()))
#   output$r_squared <- renderTable({
#     model_summary <- summary(linear_model())
#     result <- data.frame(R2=model_summary$r.squared, Adj.R2=model_summary$adj.r.squared)
#     rownames(result) = c("value")
#     result
#   })
  output$dataset_table <- renderDataTable(data_sampler(), options = list(iDisplayLength = 6)) 
  
})

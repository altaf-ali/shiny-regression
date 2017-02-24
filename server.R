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
library(AER)

axis_range <- function(axis, accuracy) {
  c(plyr::round_any(min(axis), accuracy, f = floor), plyr::round_any(max(axis), accuracy, f = ceiling))
}

shinyServer(function(input, output, session) {

  data("CASchools")

  set.seed(666) # use 111 to see upward sloping line initially
  CA_dataset <- CASchools[sample(1:nrow(CASchools), nrow(CASchools)),]
  CA_dataset$Student.Teacher.Ratio <- CA_dataset$students / CA_dataset$teachers
  CA_dataset$score <- (CA_dataset$math + CA_dataset$read) / 2

  str_domain <- axis_range(CA_dataset$Student.Teacher.Ratio, 2)
  score_domain <- axis_range(CA_dataset$score, 20)

  v <- reactiveValues(data = CA_dataset)

  observeEvent(input$resample, {
    set.seed(NULL) # use 111 to see upward sloping line initially
    v$data <- CA_dataset[sample(1:nrow(CA_dataset), nrow(CA_dataset)),]
  })

  data_sampler <- reactive({
    v$data[1:input$sample_size,]
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
    results$Residuals <- as.double(residuals_summary)
    results
  }, rownames = TRUE)

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
      dataset <- dplyr::mutate(dataset, predictions = min(predictions))
    }

    if (!any(input$show_residuals == 2)) {
      dataset <- dplyr::mutate(dataset, residuals = 0)
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

  tdist_maker <- reactive({
    model <- linear_model()
    model_coef <- coef(summary(model))
    tval <- abs(model_coef["Student.Teacher.Ratio","t value"])

    x <- seq(-5,5,length.out=100)
    pdf <- data.frame(x, y = dt(x, df = Inf), group_id = 0)
#    pdf <- data.frame(x, y = dnorm(x, 0, 1), group_id = 0)

    bottom_left <- pdf %>%
      dplyr::filter(y > 0 & x < (-1 * tval)) %>%
      dplyr::mutate(y = 0, group_id = -1)  %>%
      dplyr::arrange(desc(x))

    top_left <- pdf %>%
      dplyr::filter(y > 0 & x < (-1 * tval)) %>%
      dplyr::mutate(group_id = -1)

    bottom_right <- pdf %>%
      dplyr::filter(y > 0 & x > tval) %>%
      dplyr::mutate(y = 0, group_id = 1)  %>%
      dplyr::arrange(desc(x))

    top_right <- pdf %>%
      dplyr::filter(y > 0 & x > tval) %>%
      dplyr::mutate(group_id = 1)

    rbind(pdf, top_left, bottom_left, bottom_right, top_right)
  })

  reactive({
    tdist_maker %>%
      ggvis(x = ~x, y = ~y) %>%
      set_options(height = 200, width = 300, duration = 0, resizable = FALSE) %>%
      add_axis("x", title = "t-statistic") %>%
      scale_numeric("x", domain = c(-5, 5), nice = TRUE) %>%
      add_axis("y", title = "") %>%
      scale_numeric("y", domain = c(0, 0.4), nice = TRUE) %>%
      group_by(group_id) %>%
      layer_paths(stroke := "black", fill := NA) %>%
      filter(group_id != 0) %>%
      layer_paths(stroke := NA, fill := "red")
  }) %>%
  bind_shiny("tdist_plot")

  pvalue_maker <- reactive({
    model <- linear_model()
    pvalue <- summary(model)$coefficients["Student.Teacher.Ratio","Pr(>|t|)"]
    if (is.nan(pvalue))
      pvalue <- 0
    data.frame(x = seq(0, pvalue, 0.001), y = 1)
  })

  reactive({
    pvalue_maker %>%
      ggvis(~x, ~y) %>%
      set_options(height = 60, width = 500, duration = 0, resizable = FALSE) %>%
      layer_lines(strokeWidth := 5, stroke := "red") %>%
      add_axis("x", title = "p-value", subdivide = 1) %>%
      scale_numeric("x", domain = c(0, 1)) %>%
      add_axis("y", title = "", values = c(1, 1), properties = axis_props(labels = list(fontSize = 0))) %>%
      scale_numeric("y", domain = c(0, 1))
  }) %>%
  bind_shiny("pvalue_plot")
})

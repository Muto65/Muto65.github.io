# Load required libraries
install.packages('shinycssloaders')
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(magrittr)

# Load the data
training_data <- readRDS("data/training_data.rds")
testing_data <- readRDS("data/testing_data.rds")

# Fit the models
big_model <- lm(BMI ~ Gender + Age + Race + Smoking, data = training_data)
small_model <- lm(BMI ~ Gender, data = training_data)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Add modern theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")  # Link custom CSS
  ),
  titlePanel("Enhanced Study Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Select Analysis Type:",
                  choices = list(
                    "Summary" = "summary",
                    "Visualizations" = "visualizations",
                    "Prediction Performance" = "prediction_performance",
                    "Diagnostic Plots" = "diagnostic_plots"
                  )),
      conditionalPanel(
        condition = "input.analysis_type == 'visualizations'",
        selectInput("visualization_type", "Select Visualization:",
                    choices = list("BMI by Gender" = "bmi_gender",
                                   "BMI Distribution" = "bmi_distribution"))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", withSpinner(DTOutput("summary_table"), color = "#007bff")),
        tabPanel("Visualizations", plotlyOutput("main_plot") %>% withSpinner(color = "#007bff")),
        tabPanel("Prediction Performance", withSpinner(DTOutput("prediction_performance_table"), color = "#007bff")),
        tabPanel("Diagnostic Plots", plotOutput("diagnostic_plots") %>% withSpinner(color = "#007bff")),
        tabPanel("Statistical Results", verbatimTextOutput("stat_results"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive filtered data for visualizations
  filtered_data <- reactive({
    if (input$visualization_type == "bmi_gender") {
      training_data
    } else {
      training_data
    }
  })
  
  # Summary Table
  output$summary_table <- renderDT({
    training_data %>%
      summarise(
        Mean_BMI = mean(BMI, na.rm = TRUE),
        SD_BMI = sd(BMI, na.rm = TRUE),
        Min_BMI = min(BMI, na.rm = TRUE),
        Max_BMI = max(BMI, na.rm = TRUE),
        N = n()
      ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  # Visualizations
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    if (input$visualization_type == "bmi_gender") {
      p <- ggplot(data, aes(x = Gender, y = BMI, fill = Gender)) +
        geom_boxplot() +
        coord_flip() +
        labs(title = "BMI by Gender", x = "Gender", y = "BMI") +
        theme_minimal()
      ggplotly(p)
    } else {
      p <- ggplot(data, aes(x = BMI)) +
        geom_histogram(fill = "blue", alpha = 0.7, bins = 30) +
        labs(title = "BMI Distribution", x = "BMI", y = "Frequency") +
        theme_minimal()
      ggplotly(p)
    }
  })
  
  # Prediction Performance Table
  output$prediction_performance_table <- renderDT({
    # Predict on testing data
    test_predictions_big <- predict(big_model, testing_data)
    test_predictions_small <- predict(small_model, testing_data)
    
    # Calculate errors
    residuals_big <- testing_data$BMI - test_predictions_big
    residuals_small <- testing_data$BMI - test_predictions_small
    
    data.frame(
      Model = c("Big Model", "Small Model"),
      MAE = c(mean(abs(residuals_big)), mean(abs(residuals_small))),
      RMSE = c(sqrt(mean(residuals_big^2)), sqrt(mean(residuals_small^2))),
      R_squared = c(cor(testing_data$BMI, test_predictions_big)^2,
                    cor(testing_data$BMI, test_predictions_small)^2)
    ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  # Diagnostic Plots
  output$diagnostic_plots <- renderPlot({
    if (input$analysis_type == "diagnostic_plots") {
      residuals <- resid(big_model)
      fitted_values <- fitted(big_model)
      
      # Residuals vs Fitted Plot
      p1 <- ggplot(data = data.frame(fitted_values, residuals), aes(x = fitted_values, y = residuals)) +
        geom_point(color = "blue", alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      
      # Normal Q-Q Plot
      p2 <- ggplot(data = data.frame(residuals), aes(sample = residuals)) +
        stat_qq() +
        stat_qq_line(color = "red") +
        labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_minimal()
      
      # Scale-Location Plot
      p3 <- ggplot(data = data.frame(fitted_values, sqrt(abs(residuals))), aes(x = fitted_values, y = sqrt(abs(residuals)))) +
        geom_point(color = "blue", alpha = 0.6) +
        geom_smooth(method = "loess", color = "red", se = FALSE) +
        labs(title = "Scale-Location Plot", x = "Fitted Values", y = "√|Residuals|") +
        theme_minimal()
      
      # Residuals vs Leverage Plot
      leverage <- hatvalues(big_model)
      p4 <- ggplot(data = data.frame(leverage, residuals), aes(x = leverage, y = residuals)) +
        geom_point(color = "blue", alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Leverage", x = "Leverage", y = "Residuals") +
        theme_minimal()
      
      # Combine the plots
      gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
    }
  })
  
  # Statistical Results
  output$stat_results <- renderPrint({
    summary(big_model)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

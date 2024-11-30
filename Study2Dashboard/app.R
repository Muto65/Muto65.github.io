library(shiny)
library(tidyverse)
library(mice)
library(broom.mixed)
library(janitor)
library(ggplot2)
library(gridExtra)
# Load Data
complete_data <- readRDS("data/Analyses2Data.rds")

# Imputation for Missing Data
imputed_data <- mice(complete_data, m = 1, method = 'pmm', maxit = 5)
complete_data <- complete(imputed_data)

# Split Data into Training and Testing
set.seed(112724)
training_data <- complete_data %>% slice_sample(prop = 0.7)
testing_data <- anti_join(complete_data, training_data, by = "SEQN")

# Define UI
ui <- fluidPage(
  titlePanel("Comparison of Big Model and Small Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Select Analysis Type:",
                  choices = list(
                    "Summary" = "summary",
                    "Visualizations" = "visualizations",
                    "Prediction Performance" = "prediction_performance",
                    "Diagnostic Plots" = "diagnostic_plots"
                  ))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summary_table")),
        tabPanel("Visualizations", plotOutput("main_plot")),
        tabPanel("Prediction Performance", tableOutput("prediction_performance_table")),
        tabPanel("Diagnostic Plots", plotOutput("diagnostic_plots")),
        tabPanel("Statistical Results", verbatimTextOutput("stat_results"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Fit Models
  big_model <- reactive({
    lm(BMI ~ Gender + Age + Race + Smoking, data = training_data)
  })
  
  small_model <- reactive({
    lm(BMI ~ Gender, data = training_data)
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    if (input$analysis_type == "summary") {
      training_data %>% 
        summarise(
          Mean_BMI = mean(BMI, na.rm = TRUE),
          SD_BMI = sd(BMI, na.rm = TRUE),
          Min_BMI = min(BMI, na.rm = TRUE),
          Max_BMI = max(BMI, na.rm = TRUE),
          N = n()
        )
    }
  })
  
  # Visualizations
  output$main_plot <- renderPlot({
    if (input$analysis_type == "visualizations") {
      ggplot(training_data, aes(x = Gender, y = BMI, fill = Gender)) +
        geom_boxplot() +
        labs(title = "BMI by Gender (Training Data)",
             x = "Gender", y = "BMI") +
        theme_minimal()
    }
  })
  
  # Prediction Performance
  output$prediction_performance_table <- renderTable({
    if (input$analysis_type == "prediction_performance") {
      
      # Align factor levels for testing data
      testing_data$Race <- factor(testing_data$Race, levels = levels(training_data$Race))
      testing_data$Smoking <- factor(testing_data$Smoking, levels = levels(training_data$Smoking))
      
      # Generate Predictions
      big <- big_model()
      small <- small_model()
      testing_data$Pred_Big <- predict(big, newdata = testing_data)
      testing_data$Pred_Small <- predict(small, newdata = testing_data)
      
      # Calculate Performance Metrics
      residuals_big <- testing_data$BMI - testing_data$Pred_Big
      residuals_small <- testing_data$BMI - testing_data$Pred_Small
      
      mae_big <- mean(abs(residuals_big), na.rm = TRUE)
      mae_small <- mean(abs(residuals_small), na.rm = TRUE)
      
      rmse_big <- sqrt(mean(residuals_big^2, na.rm = TRUE))
      rmse_small <- sqrt(mean(residuals_small^2, na.rm = TRUE))
      
      r_squared_big <- cor(testing_data$BMI, testing_data$Pred_Big, use = "complete.obs")^2
      r_squared_small <- cor(testing_data$BMI, testing_data$Pred_Small, use = "complete.obs")^2
      
      # Return Performance Table
      data.frame(
        Model = c("Big Model", "Small Model"),
        MAE = c(mae_big, mae_small),
        RMSE = c(rmse_big, rmse_small),
        R_Squared = c(r_squared_big, r_squared_small)
      )
    }
  })
  
  # Diagnostic Plots
  output$diagnostic_plots <- renderPlot({
    if (input$analysis_type == "diagnostic_plots") {
      big <- big_model()
      
      par(mfrow = c(2, 2))
      plot(big, which = 1:4, col = "blue", pch = 19, lwd = 2, cex = 0.7)
    }
  })
  
  # Statistical Results
  output$stat_results <- renderPrint({
    if (input$analysis_type == "summary") {
      summary(big_model())
    } else if (input$analysis_type == "prediction_performance") {
      summary(small_model())
    }
  })
}

# Run the Application
shinyApp(ui = ui, server = server)

#My shinyapp
library(shiny)
library(forcats)
library(boot)
library(ggplot2)
library(ggpubr)
library(kableExtra)
library(janitor)
library(naniar)
library(datawizard)
library(patchwork)
library(gridExtra)
library(broom.mixed)
library(Epi)
library(vcd)
library(xfun)
library(epitools)
library(rsconnect)
library(tidyverse)
theme_set(theme_light())  
knitr::opts_chunk$set(comment=NA)
# Load the data
analysisA_data <- readRDS("data/AnalysisA.rds")
analysisB_data <- readRDS("data/analysisB.rds")
analysisD_data <- readRDS("data/analysisD.rds")
analysisE_data <- readRDS("data/analysisE.rds")

# Define UI
ui <- fluidPage(
  titlePanel("NHANES Interactive Data Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Select Analysis Type:",
                  choices = list(
                    "Blood Pressure Analysis" = "bp_analysis",
                    "BMI by Gender" = "bmi_gender",
                    "Smoking vs Obesity" = "smoking_obesity",
                    "Race vs Education" = "race_education"
                  )),
      conditionalPanel(
        condition = "input.analysis_type == 'bmi_gender'",
        sliderInput("bmi_range", "Select BMI Range:",
                    min = 15, max = 50, value = c(20, 30))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'race_education'",
        selectInput("race_filter", "Filter by Race:",
                    choices = c("All", unique(analysisE_data$Race)))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'smoking_obesity'",
        selectInput("smoking_filter", "Filter by Smoking Status:",
                    choices = c("All", unique(analysisD_data$Smoking)))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summary_table")),
        tabPanel("Plot", plotOutput("main_plot")),
        tabPanel("Statistical Results", verbatimTextOutput("stat_results"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive filtering of data
  filtered_data <- reactive({
    if (input$analysis_type == "bmi_gender") {
      analysisB_data %>% 
        filter(BMI >= input$bmi_range[1], BMI <= input$bmi_range[2])
    } else if (input$analysis_type == "race_education") {
      if (input$race_filter == "All") {
        analysisE_data
      } else {
        analysisE_data %>% filter(Race == input$race_filter)
      }
    } else if (input$analysis_type == "smoking_obesity") {
      if (input$smoking_filter == "All") {
        analysisD_data
      } else {
        analysisD_data %>% filter(Smoking == input$smoking_filter)
      }
    } else {
      analysisA_data
    }
  })
  
  # Main Plot
  output$main_plot <- renderPlot({
    data <- filtered_data()
    if (input$analysis_type == "bp_analysis") {
      p1 <-ggplot(data, aes(x = BPXOSY1, y = BPXOSY2)) +
        geom_point(alpha = 0.6) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Systolic BP: 1st vs 2nd Readings",
             x = "1st Reading (mmHg)", y = "2nd Reading (mmHg)") +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      p2 <-ggplot(data, aes(x = log(BPXOSY1))) +
        geom_histogram(fill = "slateblue", col = "white",
                       bins = 20) + 
        labs(title = "Log-SBP 1st Reading in `NHANES`",
             x = "log(SBP 1st Reading (mm Hg))")+
        theme_classic(base_size = 10) +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      
      p3 <-ggplot(data, aes(x = log(BPXOSY2))) +
        geom_histogram(fill = "orange", col = "white",
                       bins = 20) + 
        labs(title = "SBP 2nd Reading in `NHANES`",
             x = "log(SBP 2nd Reading(mm Hg))")+
        theme_classic(base_size = 10) +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      
      
      p4 <-ggplot(data, aes(x = "", y = log(BPXOSY1))) +
        geom_boxplot(fill = "slateblue", outlier.size = 2,
                     outlier.color = "slateblue") +
        coord_flip() + 
        labs(y = "log(SBP 1st-Reading (mm Hg))")+
        theme_classic(base_size = 10) +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      
      
      p5 <-ggplot(data, aes(x = "", y = log(BPXOSY2)))+
        geom_boxplot(fill = "orange", outlier.size = 2,
                     outlier.color = "orange") +
        coord_flip() + 
        labs(y = "log(SBP 2nd Reading(mm Hg))")+
        theme_classic(base_size = 10) +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      
      p1 +p2 + p3 + p4/p5
    } else if (input$analysis_type == "bmi_gender") {
      p1 <- ggplot(data, aes(x = BMI)) +
        geom_histogram(fill = "slateblue", col = "white",
                       bins = 20) + 
        labs(title = "BMI Distribution in `NHANES`",
             x = "Body Mass Index")+
        theme_classic(base_size = 10) +
        theme(legend.position = "left",
              axis.text = element_text(face = "bold",size = 10),
              axis.title.y = element_text(face = "bold", size = 10),
              axis.title.x = element_text(face = "bold", size = 10),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
      
      p2 <- ggplot(data, aes(sample = BMI)) +
        geom_qq(col = "slateblue") + 
        geom_qq_line(col = "magenta") + 
        theme(aspect.ratio = 1) + 
        labs(title = "Normal Q-Q: NHANES BMI")+
        theme_classic(base_size = 12) +
        theme(legend.position = "none",
              axis.text = element_text(face = "bold",size = 10),
              axis.title.y = element_text(face = "bold", size = 10),
              axis.title.x = element_text(face = "bold", size = 10),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 10))
      #Boxplot
      p3<- ggplot(data, aes(x = Gender, y = BMI, fill = Gender)) +
        geom_boxplot() + coord_flip()+
        labs(title = "Comparison of BMI by Gender",
             x = "Gender",
             y = "Body Mass Index (BMI)") +
        theme_classic(base_size = 12) +
        theme(legend.position = "left",
              axis.text = element_text(face = "bold",size = 10),
              axis.title.y = element_text(face = "bold", size = 10),
              axis.title.x = element_text(face = "bold", size = 10),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      #density plot
      p4<- ggplot(data, aes(x = BMI, fill = Gender)) +
        geom_density(alpha = 0.5) +
        labs(title = "BMI Distribution by Gender",
             x = "Body Mass Index (BMI)",
             y = "Density") +
        theme_classic(base_size = 12) +
        theme(legend.position = "left",
              axis.text = element_text(face = "bold",size = 10),
              axis.title.y = element_text(face = "bold", size = 10),
              axis.title.x = element_text(face = "bold", size = 10),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
      
      p1 +p2 +p3 +p4
    } else if (input$analysis_type == "smoking_obesity") {
      ggplot(data, aes(x = Smoking, fill = Obesity)) +
        geom_bar(position = "dodge") +
        labs(title = "Smoking vs Obesity",
             x = "Smoking Status", y = "Count") +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
    } else if (input$analysis_type == "race_education") {
    ggplot(data, aes(x = Race, fill = Education)) +
        geom_bar(position = "dodge") +
        labs(title = "Race vs Education",
             x = "Race/Ethnicity", y = "Count") +
        theme(
          axis.text = element_text(face = "bold",size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.title.x = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12))
   
    }
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    data <- filtered_data()
    if (input$analysis_type == "bp_analysis") {
      data %>% 
        summarise(
          Mean_BP1 = mean(BPXOSY1, na.rm = TRUE),
          Mean_BP2 = mean(BPXOSY2, na.rm = TRUE),
          SD_BP1 = sd(BPXOSY1, na.rm = TRUE),
          SD_BP2 = sd(BPXOSY2, na.rm = TRUE),
          N = n()
        )
    } else if (input$analysis_type == "bmi_gender") {
      data %>% 
        group_by(Gender) %>% 
        summarise(
          Mean_BMI = mean(BMI, na.rm = TRUE),
          SD_BMI = sd(BMI, na.rm = TRUE),
          N = n()
        )
    } else if (input$analysis_type == "smoking_obesity") {
      data %>% 
        count(Smoking, Obesity) %>% 
        spread(Obesity, n, fill = 0)
    } else if (input$analysis_type == "race_education") {
      data %>% 
        count(Race, Education) %>% 
        spread(Education, n, fill = 0)
    }
  })
  
  # Statistical Results
  output$stat_results <- renderPrint({
    data <- filtered_data()
    if (input$analysis_type == "bp_analysis") {
      # Wilcoxon signed-rank test
      wilcox.test(data$BPXOSY1, data$BPXOSY2, paired = TRUE)
    } else if (input$analysis_type == "bmi_gender") {
      # Wilcoxon rank-sum test
      wilcox.test(BMI ~ Gender, data = data)
    } else if (input$analysis_type == "smoking_obesity") {
      # Chi-square test
      chisq.test(table(data$Smoking, data$Obesity))
    } else if (input$analysis_type == "race_education") {
      # Chi-square test
      chisq.test(table(data$Race, data$Education))
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

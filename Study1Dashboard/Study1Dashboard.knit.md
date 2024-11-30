---
title: "Interactive Dashboard: Exploring Insights  in Sociodemographic and Health Associations from the NHANES 2017–March 2020 Dataset"
author: "Bosco Bakwatanisa"
date: last-modified
format: 
  html:
    fig-responsive: true
    fig-align: center
    fig-width: 8
    fig-height: 8
    page-layout: full
    sidebar: true
    theme: flatly
    embed-resources: true
    number_sections: true
    date-format: iso
    code-fold: false
    toc: true
    toc-depth: 3
runtime: shiny
execute:
  echo: true
  warning: false
  message: false
editor: visual
---



# NHANES Interactive Dashboard

This dashboard allows you to interactively explore insights from the NHANES 2017–March 2020 dataset. Use the sidebar controls to customize the analysis and visualize the results.

------------------------------------------------------------------------







## Sidebar Inputs



::: {.cell}

```{.r .cell-code}
fluidPage( sidebarLayout( sidebarPanel( selectInput( inputId = "analysis_type", label = "Select Analysis Type:", choices = c("Systolic BP Analysis" = "bp_analysis", "BMI by Gender" = "bmi_gender", "Smoking and Obesity" = "smoking_obesity", "Race vs Education" = "race_education"), selected = "bp_analysis" ), conditionalPanel( condition = "input.analysis_type == 'bmi_gender'", sliderInput("bmi_range", "Filter BMI Range:", min = 10, max = 50, value = c(15, 35)) ), conditionalPanel( condition = "input.analysis_type == 'race_education'", selectInput( "race_filter", "Select Race/Ethnicity:", choices = c("All", "Mexican", "Hispanic", "White", "Black", "Asian"), selected = "All" ) ), conditionalPanel( condition = "input.analysis_type == 'smoking_obesity'", radioButtons( "smoking_filter", "Smoking Status:", choices = c("All", "Smoker", "Non-Smoker"), selected = "All" ) ) ), mainPanel( tabsetPanel( type = "tabs", tabPanel("Visualization", plotOutput("main_plot")), tabPanel("Data Summary", tableOutput("summary_table")) ) ) ) )
```

::: {.cell-output-display}

```{=html}
<div class="container-fluid">
<div class="row">
<div class="col-sm-4">
<form class="well" role="complementary">
<div class="form-group shiny-input-container">
<label class="control-label" id="analysis_type-label" for="analysis_type">Select Analysis Type:</label>
<div>
<select id="analysis_type" class="shiny-input-select"><option value="bp_analysis" selected>Systolic BP Analysis</option>
<option value="bmi_gender">BMI by Gender</option>
<option value="smoking_obesity">Smoking and Obesity</option>
<option value="race_education">Race vs Education</option></select>
<script type="application/json" data-for="analysis_type" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
<div class="shiny-panel-conditional" data-display-if="input.analysis_type == &#39;bmi_gender&#39;" data-ns-prefix="">
<div class="form-group shiny-input-container">
<label class="control-label" id="bmi_range-label" for="bmi_range">Filter BMI Range:</label>
<input class="js-range-slider" id="bmi_range" data-skin="shiny" data-type="double" data-min="10" data-max="50" data-from="15" data-to="35" data-step="1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
</div>
</div>
<div class="shiny-panel-conditional" data-display-if="input.analysis_type == &#39;race_education&#39;" data-ns-prefix="">
<div class="form-group shiny-input-container">
<label class="control-label" id="race_filter-label" for="race_filter">Select Race/Ethnicity:</label>
<div>
<select id="race_filter" class="shiny-input-select"><option value="All" selected>All</option>
<option value="Mexican">Mexican</option>
<option value="Hispanic">Hispanic</option>
<option value="White">White</option>
<option value="Black">Black</option>
<option value="Asian">Asian</option></select>
<script type="application/json" data-for="race_filter" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div class="shiny-panel-conditional" data-display-if="input.analysis_type == &#39;smoking_obesity&#39;" data-ns-prefix="">
<div id="smoking_filter" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="smoking_filter-label">
<label class="control-label" id="smoking_filter-label" for="smoking_filter">Smoking Status:</label>
<div class="shiny-options-group">
<div class="radio">
<label>
<input type="radio" name="smoking_filter" value="All" checked="checked"/>
<span>All</span>
</label>
</div>
<div class="radio">
<label>
<input type="radio" name="smoking_filter" value="Smoker"/>
<span>Smoker</span>
</label>
</div>
<div class="radio">
<label>
<input type="radio" name="smoking_filter" value="Non-Smoker"/>
<span>Non-Smoker</span>
</label>
</div>
</div>
</div>
</div>
</form>
</div>
<div class="col-sm-8" role="main">
<div class="tabbable">
<ul class="nav nav-tabs" data-tabsetid="8062">
<li class="active">
<a href="#tab-8062-1" data-toggle="tab" data-bs-toggle="tab" data-value="Visualization">Visualization</a>
</li>
<li>
<a href="#tab-8062-2" data-toggle="tab" data-bs-toggle="tab" data-value="Data Summary">Data Summary</a>
</li>
</ul>
<div class="tab-content" data-tabsetid="8062">
<div class="tab-pane active" data-value="Visualization" id="tab-8062-1">
<div class="shiny-plot-output html-fill-item" id="main_plot" style="width:100%;height:400px;"></div>
</div>
<div class="tab-pane" data-value="Data Summary" id="tab-8062-2">
<div id="summary_table" class="shiny-html-output"></div>
</div>
</div>
</div>
</div>
</div>
</div>
```

:::
:::

::: {.cell}

```{.r .cell-code}
analysisA_data <- readRDS("data/AnalysisA.rds")
analysisB_data <- readRDS("data/analysisB.rds")
analysisD_data <- readRDS("data/analysisD.rds")
analysisE_data <- readRDS("data/analysisE.rds")

shinyServer(function(input, output) {
  
  # Filter data based on user inputs
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
  
  # Main plot
  output$main_plot <- renderPlot({
    data <- filtered_data()
    if (input$analysis_type == "bp_analysis") {
      ggplot(data, aes(x = BPXOSY1, y = BPXOSY2)) +
        geom_point(alpha = 0.6) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Systolic BP: 1st vs 2nd Readings",
             x = "1st Reading (mmHg)", y = "2nd Reading (mmHg)") +
        theme_minimal()
    } else if (input$analysis_type == "bmi_gender") {
      ggplot(data, aes(x = Gender, y = BMI, fill = Gender)) +
        geom_boxplot() +
        coord_flip() +
        labs(title = "BMI Distribution by Gender",
             x = "Gender", y = "BMI") +
        theme_minimal()
    } else if (input$analysis_type == "smoking_obesity") {
      ggplot(data, aes(x = Smoking, fill = Obesity)) +
        geom_bar(position = "dodge") +
        labs(title = "Smoking vs Obesity",
             x = "Smoking Status", y = "Count") +
        theme_minimal()
    } else if (input$analysis_type == "race_education") {
      ggplot(data, aes(x = Race, fill = Education)) +
        geom_bar(position = "dodge") +
        labs(title = "Race vs Education",
             x = "Race/Ethnicity", y = "Count") +
        theme_minimal()
    }
  })
  
  # Summary table
  output$summary_table <- renderTable({
    data <- filtered_data()
    if (input$analysis_type == "bp_analysis") {
      summary(data)
    } else {
      data %>% 
        group_by(across(everything())) %>% 
        summarise(n = n(), .groups = "drop")
    }
  })
})
```
:::


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# Assuming 'merged_data' is your dataset loaded here



# BULLET chat greattttttttttttttt
library(shiny)
library(dplyr)
library(gt)
library(gtExtras)
library(DT)
library(ggstatsplot)

# Load your data
# merged_data <- read.csv("your_data.csv")  # Replace with your dataset

ui <- navbarPage(
  title = "Exploring Correlations Between Water Access and Disease Burden",
  
  # Correlation Tab
  tabPanel("📈 Correlation Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("selectedDisease",
                           "Select Disease:",
                           choices = c("Typhoid Rate" = "TyphoidRate",
                                       "Diarrhea Rate" = "DiarrheaRate",
                                       "Hepatitis Rate" = "HepatitisRate",
                                       "Unsafe Water Risk" = "UnsafeRisk")),
               selectInput("selectedIndicators",
                           "Select Water Indicator(s):",
                           choices = unique(merged_data$Series.Name),
                           selected = unique(merged_data$Series.Name)[1],
                           multiple = TRUE),
               sliderInput("selectedYear",
                           "Select Year:",
                           min = min(merged_data$Year),
                           max = max(merged_data$Year),
                           value = c(min(merged_data$Year), max(merged_data$Year)),
                           sep = ""),
               selectInput("selectedRegions",
                           "Select Region(s):",
                           choices = unique(merged_data$Region),
                           selected = unique(merged_data$Region)[1:2],
                           multiple = TRUE)
             ),
             mainPanel(
               plotOutput("correlationPlot"),
               h4("Filtered Data"),
               DT::dataTableOutput("dataTable"),
               h4("Summary Statistics"),
               tableOutput("summaryStatsTable")
             )
           )
  ),
  
  # ANOVA Tab
  tabPanel("📊 ANOVA Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("selectedDiseaseANOVA",
                           "Select Disease:",
                           choices = c("Typhoid Rate" = "TyphoidRate",
                                       "Diarrhea Rate" = "DiarrheaRate",
                                       "Hepatitis Rate" = "HepatitisRate",
                                       "Unsafe Water Risk" = "UnsafeRisk")),
               selectInput("selectedIndicatorsANOVA",
                           "Select Water Indicator(s):",
                           choices = unique(merged_data$Series.Name),
                           selected = unique(merged_data$Series.Name)[1],
                           multiple = TRUE),
               selectInput("selectedRegionsANOVA",
                           "Select Region(s):",
                           choices = unique(merged_data$Region),
                           selected = unique(merged_data$Region)[1:2],
                           multiple = TRUE)
             ),
             mainPanel(
               plotOutput("anovaPlot")
             )
           )
  ),
  
  # Bullet Dashboard Tab
  tabPanel("📌 Bullet Dashboard",
           sidebarLayout(
             sidebarPanel(
               selectInput("indicator_type", "Choose Indicator Type:",
                           choices = c("Water", "Disease")),
               uiOutput("dynamic_indicator"),
               uiOutput("bullet_year_slider")
             ),
             mainPanel(
               gt_output("bullet_table"),
               h4("Insights"),
               verbatimTextOutput("data_insights")
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Reactive filter for correlation tab
  filtered_data_correlation <- reactive({
    req(input$selectedDisease)
    merged_data %>%
      filter(Year >= input$selectedYear[1],
             Year <= input$selectedYear[2],
             Region %in% input$selectedRegions,
             Series.Name %in% input$selectedIndicators) %>%
      mutate(DiseaseRate = .data[[input$selectedDisease]])
  })
  
  # Reactive filter for ANOVA tab
  filtered_data_anova <- reactive({
    req(input$selectedDiseaseANOVA)
    merged_data %>%
      filter(Region %in% input$selectedRegionsANOVA,
             Series.Name %in% input$selectedIndicatorsANOVA) %>%
      mutate(DiseaseRate = .data[[input$selectedDiseaseANOVA]])
  })
  
  # Correlation Plot
  output$correlationPlot <- renderPlot({
    req(filtered_data_correlation())
    ggstatsplot::ggscatterstats(
      data = filtered_data_correlation(),
      x = AvgValue,
      y = DiseaseRate,
      xlab = "Water Indicator Value",
      ylab = input$selectedDisease,
      title = paste("Correlation between",
                    paste(input$selectedIndicators, collapse = ","),
                    "and", input$selectedDisease),
      marginal = TRUE
    )
  })
  
  # ANOVA Plot
  output$anovaPlot <- renderPlot({
    req(filtered_data_anova())
    anova_data <- filtered_data_anova()
    
    grouping_var <- NULL
    if (length(unique(anova_data$Region)) >= 2) {
      grouping_var <- "Region"
    } else if (length(unique(anova_data$Series.Name)) >= 2) {
      grouping_var <- "Series.Name"
    } else {
      showNotification("Please select at least two regions or indicators for ANOVA.", type = "error")
      return(NULL)
    }
    
    ggstatsplot::ggbetweenstats(
      data = anova_data,
      x = !!rlang::sym(grouping_var),
      y = !!rlang::sym("DiseaseRate"),
      xlab = grouping_var,
      ylab = input$selectedDiseaseANOVA,
      title = paste("ANOVA:", input$selectedDiseaseANOVA, "across", grouping_var)
    )
  })
  
  # Data Table for Correlation Tab
  output$dataTable <- DT::renderDataTable({
    filtered_data_correlation()
  })
  
  # Summary Statistics for Correlation Tab
  output$summaryStatsTable <- renderTable({
    df <- filtered_data_correlation()
    df %>%
      summarise(
        Count = n(),
        Mean_WaterValue = mean(AvgValue, na.rm = TRUE),
        Mean_DiseaseRate = mean(DiseaseRate, na.rm = TRUE),
        Correlation = cor(AvgValue, DiseaseRate, use = "complete.obs")
      )
  })
  
  # Reactive data preparation for bullet chart
  bullet_data <- reactive({
    req(input$selected_indicator_dashboard, input$bullet_year)
    
    if (input$indicator_type == "Water") {
      merged_data %>%
        filter(Series.Name == input$selected_indicator_dashboard,
               !is.na(AvgValue),
               Year <= input$bullet_year) %>%
        group_by(Region) %>%
        summarise(
          Min = min(AvgValue, na.rm = TRUE),
          Max = max(AvgValue, na.rm = TRUE),
          Average = round(mean(AvgValue, na.rm = TRUE), 1),
          Monthly = list(AvgValue[Year <= input$bullet_year]),
          Actual = mean(AvgValue[Year == input$bullet_year], na.rm = TRUE),
          Target = 100,
          .groups = "drop"
        )
    } else {
      merged_data %>%
        filter(!is.na(.data[[input$selected_indicator_dashboard]]),
               Year <= input$bullet_year) %>%
        group_by(Region) %>%
        summarise(
          Min = min(.data[[input$selected_indicator_dashboard]], na.rm = TRUE),
          Max = max(.data[[input$selected_indicator_dashboard]], na.rm = TRUE),
          Average = round(mean(.data[[input$selected_indicator_dashboard]], na.rm = TRUE), 1),
          Monthly = list(.data[[input$selected_indicator_dashboard]][Year <= input$bullet_year]),
          Actual = mean(.data[[input$selected_indicator_dashboard]][Year == input$bullet_year], na.rm = TRUE),
          Target = 0,
          .groups = "drop"
        )
    }
  })
  
  # Dynamic indicator select box 
  output$dynamic_indicator <- renderUI({
    if (input$indicator_type == "Water") {
      selectInput("selected_indicator_dashboard", "Select Water Indicator:",
                  choices = unique(merged_data$Series.Name))
    } else {
      selectInput("selected_indicator_dashboard", "Select Disease Indicator:",
                  choices = c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk"))
    }
  })
  
  # Bullet Chart table
  output$bullet_table <- render_gt({
    req(bullet_data())
    
    bullet_data() %>%
      gt() %>%
      fmt_number(columns = c(Min, Max, Average, Actual), decimals = 1) %>%
      gt_plt_sparkline(Monthly, type = "default") %>%
      gt_plt_bullet(column = Actual,
                    target = Target,
                    width = 108,
                    palette = if (input$indicator_type == "Water") c("lightblue", "black") else c("salmon", "black")) %>%
      cols_label(
        Min = "Min",
        Max = "Max",
        Average = "Avg",
        Actual = "Value",
        Monthly = "Trend",
        Target = "Target"
      ) %>%
      gt_theme_espn()
  })
  
  # Year slider
  output$bullet_year_slider <- renderUI({
    tagList(
      tags$label("📅 Select Year for Dashboard:", style = "font-weight: bold;"),
      sliderInput("bullet_year",
                  label = NULL,
                  min = min(merged_data$Year, na.rm = TRUE),
                  max = max(merged_data$Year, na.rm = TRUE),
                  value = max(merged_data$Year, na.rm = TRUE),
                  sep = "")
    )
  })
  
  # Additional insights output
  output$data_insights <- renderPrint({
    req(bullet_data())
    
    insights <- bullet_data() %>%
      summarise(
        total_regions = n(),
        overall_min = min(Min),
        overall_max = max(Max),
        overall_avg = mean(Average),
        highest_actual = max(Actual),
        lowest_actual = min(Actual)
      )
    
    cat("Data Insights:\n")
    cat("Total Regions:", insights$total_regions, "\n")
    cat("Overall Minimum Value:", round(insights$overall_min, 2), "\n")
    cat("Overall Maximum Value:", round(insights$overall_max, 2), "\n")
    cat("Overall Average:", round(insights$overall_avg, 2), "\n")
    cat("Highest Actual Value:", round(insights$highest_actual, 2), "\n")
    cat("Lowest Actual Value:", round(insights$lowest_actual, 2), "\n")
  })
}

shinyApp(ui = ui, server = server)
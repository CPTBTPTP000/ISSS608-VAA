#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#




# Assuming 'merged_data' is your dataset loaded here

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(gt)
library(gtExtras) 
library(DT)
library(rlang)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  # CSS for styling
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f8f9fa;
        padding: 0;
        min-height: 100vh;
        border-right: 1px solid #dee2e6;
      }
      .nav-item {
        padding: 15px;
        border-bottom: 1px solid #e9ecef;
        cursor: pointer;
      }
      .nav-item:hover {
        background-color: #e9ecef;
      }
      .nav-item.active {
        background-color: #e9ecef;
      }
      .dropdown-item {
        padding: 12px 25px;
        border-bottom: 1px solid #e9ecef;
        display: none;
        background-color: #f2f2f2;
      }
      .dropdown-item:hover {
        background-color: #e2e2e2;
      }
      .main-title {
        padding: 15px;
        font-size: 24px;
        border-bottom: 1px solid #dee2e6;
      }
      .caret-icon {
        float: right;
        margin-top: 3px;
      }
      .stat-box {
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: transform 0.3s ease;
      }
      .stat-box:hover {
        transform: translateY(-5px);
      }
      .stat-value {
        font-size: 28px;
        font-weight: bold;
        margin-bottom: 5px;
      }
      .stat-label {
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      .count-box {
        background-color: #e3f2fd;
        color: #1976d2;
      }
      .water-box {
        background-color: #e0f7fa;
        color: #0097a7;
      }
      .disease-box {
        background-color: #f1f8e9;
        color: #689f38;
      }
      .correlation-box {
        background-color: #fce4ec;
        color: #c2185b;
      }
      .filtered-data-section {
        background-color: #ffffff;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        margin-top: 20px;
      }
      .section-title {
        font-size: 18px;
        font-weight: 500;
        margin-bottom: 15px;
        color: #424242;
        border-bottom: 2px solid #f0f0f0;
        padding-bottom: 10px;
      }
      .analysis-panel {
        padding: 20px;
        background-color: #ffffff;
      }
      .anova-options {
        margin-bottom: 20px;
        padding: 15px;
        background-color: #f9f9f9;
        border-radius: 8px;
        border-left: 4px solid #2196F3;
      }
      .anova-results {
        margin-top: 20px;
        padding: 15px;
        background-color: #fff;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
      
       .select-pill {
        display: inline-block;
        background-color: #e9ecef;
        border-radius: 16px;
        padding: 4px 10px;
        margin: 2px;
        font-size: 14px;
      }
      .select-pill .remove-btn {
        margin-left: 6px;
        cursor: pointer;
        color: #666;
      }
      .select-pill .remove-btn:hover {
        color: #dc3545;
      }
      .selectize-input {
        overflow: auto;
        max-height: 120px;
      }
      
    ")),
    
    tags$script(HTML("
      $(document).on('click', '.remove-btn', function() {
        var selectId = $(this).data('id');
        var value = $(this).data('value');
        var selectInput = $('#' + selectId)[0].selectize;
        selectInput.removeItem(value);
      });
      
      // 自定义 selectize 渲染
      $(document).on('shiny:connected', function() {
        customizeSelectize = function(id) {
          if ($('#' + id).length > 0 && $('#' + id)[0].selectize) {
            var selectize = $('#' + id)[0].selectize;
           
            
          // Store the original render method
            var originalRender = selectize.settings.render.item;
            
            // Override the render method
            selectize.settings.render.item = function(data, escape) {
              // Make sure we have valid text by checking the options
              var text = data.text;
              
              // If text is undefined, try to get it from the options
              if (!text && selectize.options[data.value]) {
                text = selectize.options[data.value].text;
              }
              
              // If still undefined, use the value as fallback
              if (!text) {
                text = data.value;
              }
              
              var item = '<div class=\"select-pill\">' + escape(text) + 
                         '<span class=\"remove-btn\" data-id=\"' + id + '\" data-value=\"' + 
                         escape(data.value) + '\">×</span></div>';
              return item;
            };
            
            // Refresh items to apply the new rendering
            selectize.refreshItems();
          }
        };
        
        // 应用到区域选择器
        setTimeout(function() {
          customizeSelectize('selectedRegions');
          customizeSelectize('anovaRegions');
          customizeSelectize('anovaRegions2');
          customizeSelectize('anovaIndicators');
        }, 1000);
      });
      
       // Re-apply customization when inputs change
      $(document).on('shiny:inputchanged', function(event) {
        if(['selectedRegions', 'anovaRegions', 'anovaRegions2', 'anovaIndicators'].includes(event.name)) {
          setTimeout(function() {
            customizeSelectize(event.name);
          }, 200);
        }
      });
      
      
      
    "))
    
  ),
  
  # Header with title
  div(class = "main-title", 
      
      "Water & Disease Analysis Dashboard"
  ),
  
  # Main layout
  fluidRow(
    # Sidebar Menu - Left 20% of screen (making it smaller)
    column(2, class = "sidebar",
           div(id = "data-exploration", class = "nav-item", "Data Exploration"),
           div(id = "confirmatory-analysis", class = "nav-item", 
               "Confirmatory Analysis ", 
               tags$span(class = "caret-icon", icon("caret-down", lib = "font-awesome"))
           ),
           div(id = "correlation-analysis", class = "dropdown-item", "Correlation Analysis"),
           div(id = "anova-analysis", class = "dropdown-item", "ANOVA Analysis"),
           div(id = "bullet-charts", class = "dropdown-item", "Bullet Charts")
    ),
    
    # Main Content Area - Right 80% of screen (now wider)
    column(10,
           # Content panels for each analysis type
           # Default empty panel
           div(id = "default-panel", class = "analysis-panel",
               h3("Welcome to the Water & Disease Analysis Dashboard"),
               p("Select an analysis option from the sidebar to begin.")
           ),
           
           # Correlation Analysis Panel
           # Correlation Analysis Panel
           # Correlation Analysis Panel
           div(id = "correlation-panel", class = "analysis-panel", style = "display: none;",
               titlePanel("Correlation Analysis"),
               
               # Summary Statistics at the top in separate boxes
               fluidRow(
                 column(3, 
                        div(class = "stat-box count-box",
                            div(class = "stat-value", textOutput("countStat")),
                            div(class = "stat-label", "Total Records")
                        )
                 ),
                 column(3, 
                        div(class = "stat-box water-box",
                            div(class = "stat-value", textOutput("waterValueStat")),
                            div(class = "stat-label", "Avg Water Value")
                        )
                 ),
                 column(3, 
                        div(class = "stat-box disease-box",
                            div(class = "stat-value", textOutput("diseaseRateStat")),
                            div(class = "stat-label", "Avg Disease Rate")
                        )
                 ),
                 column(3, 
                        div(class = "stat-box correlation-box",
                            div(class = "stat-value", textOutput("correlationStat")),
                            div(class = "stat-label", "Correlation")
                        )
                 )
               ),
               
               # Replace sidebarLayout with fluidRow for two-column layout
               fluidRow(
                 # Left column for filters
                 column(3,
                        div(class = "well", style = "background-color: #f9f9f9; border-radius: 8px; padding: 15px;",
                            selectInput("selectedDisease",
                                        "Select Disease:",
                                        choices = c("Typhoid Rate" = "TyphoidRate",
                                                    "Diarrhea Rate" = "DiarrheaRate",
                                                    "Hepatitis Rate" = "HepatitisRate",
                                                    "Unsafe Water Risk" = "UnsafeRisk")),
                            selectInput("selectedIndicators",
                                        "Select Water Indicator(s):",
                                        choices = unique(merged_data$`Series.Name`),
                                        selected = unique(merged_data$`Series.Name`)[1],
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
                        )
                 ),
                 
                 # Right column with tabbed interface
                 column(9,
                        # Add tabsetPanel for tabbed interface
                        tabsetPanel(
                          tabPanel("Correlation visulization",
                                   plotOutput("correlationPlot", height = "500px")
                          ),
                         
                          tabPanel("Data Table",
                                   # Improved Filtered Data section
                                   div(class = "filtered-data-section",
                                       div(class = "section-title", "Filtered Data"),
                                       fluidRow(
                                         column(8, 
                                                selectInput("pageSize", "Rows per page:", 
                                                            choices = c(5, 10, 15, 20, 50), 
                                                            selected = 10)
                                         ),
                                         column(4,
                                                textInput("searchText", "Search:", "")
                                         )
                                       ),
                                       DT::dataTableOutput("dataTable")
                                   )
                          )
                        )
                 )
               )
           ),
           
           # Enhanced ANOVA Analysis Panel - Now with consistent sidebar layout
           div(id = "anova-panel", class = "analysis-panel", style = "display: none;",
               titlePanel("ANOVA Analysis"),
               
               # Summary Statistics at the top in separate boxes (similar to correlation panel)
               fluidRow(
                 column(3, 
                        div(class = "stat-box count-box",
                            div(class = "stat-value", textOutput("anovaCountStat")),
                            div(class = "stat-label", "Total Records")
                        )
                 ),
                 column(3, 
                        div(class = "stat-box water-box",
                            div(class = "stat-value", textOutput("anovaFStat")),
                            div(class = "stat-label", "F Statistic")
                        )
                 ),
                 column(3, 
                        div(class = "stat-box disease-box",
                            div(class = "stat-value", textOutput("anovaPValue")),
                            div(class = "stat-label", "P Value")
                        )
                 ),
                 column(3, 
                        div(class = "stat-box correlation-box",
                            div(class = "stat-value", textOutput("anovaRSquared")),
                            div(class = "stat-label", "R Squared")
                        )
                 )
               ),
               
               # Main content with smaller sidebar like other panels
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   div(class = "well", style = "background-color: #f9f9f9; border-radius: 8px; padding: 15px;",
                       selectInput("anovaAnalysisType", "Analysis Type:",
                                   choices = c(
                                     "Water Indicator Across Diseases" = "indicator_across_diseases",
                                     "Disease Rates by Region" = "disease_by_region",
                                     "Disease Rates by Water Indicator" = "disease_by_indicator",
                                     "Water Indicators by Region" = "indicator_by_region",
                                     "Compare All Diseases" = "all_diseases",
                                     "Compare All Water Indicators" = "all_indicators"
                                   ),
                                   selected = "indicator_across_diseases"),
                       uiOutput("dynamicAnovaSelector1"),
                       uiOutput("dynamicAnovaSelector2"),
                       selectInput("anovaYearRange", "Year Range:",
                                   choices = c(
                                     "All Years" = "all",
                                     "Last 5 Years" = "last5",
                                     "Last 10 Years" = "last10",
                                     "Custom Range" = "custom"
                                   ),
                                   selected = "all"),
                       conditionalPanel(
                         condition = "input.anovaYearRange == 'custom'",
                         sliderInput("customAnovaYearRange", "Select Custom Year Range:",
                                     min = min(merged_data$Year),
                                     max = max(merged_data$Year),
                                     value = c(min(merged_data$Year), max(merged_data$Year)),
                                     sep = "")
                       ),
                       actionButton("runAnovaBtn", "Run ANOVA Analysis", 
                                    class = "btn-primary", 
                                    style = "margin-top: 15px; background-color: #2196F3; color: white; width: 100%;")
                   )
                 ),
                 mainPanel(
                   width = 9,
                   # ANOVA Results
                   plotOutput("anovaPlot", height = "500px"),
                   
                   
                 )
               )
           ),
           
           
           # Bullet Charts Panel
           div(id = "bullet-panel", class = "analysis-panel", style = "display: none;",
               titlePanel("Auto-Play Bullet Chart"),
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   div(class = "well", style = "background-color: #f9f9f9; border-radius: 8px; padding: 15px;",
                       selectInput("indicator_type_auto", "Choose Indicator Type:",
                                   choices = c("Water", "Disease")),
                       uiOutput("dynamic_indicator_auto"),
                       sliderInput("auto_year", "Select Year:", 
                                   min = min(merged_data$Year), 
                                   max = max(merged_data$Year), 
                                   value = min(merged_data$Year))
                   )
                 ),
                 mainPanel(
                   width = 9,
                   gt::gt_output("bullet_table_auto"),  # Make sure to use gt::gt_output
                   h4("Insights"),
                   verbatimTextOutput("data_insights")
                 )
               )
           )
    )
  )
)

server <- function(input, output, session) {
  # Define hideAllPanels function inside the server function
  hideAllPanels <- function() {
    shinyjs::hide("default-panel")
    shinyjs::hide("correlation-panel")
    shinyjs::hide("anova-panel")
    shinyjs::hide("bullet-panel")
  }
  
  # JavaScript for dropdown menu
  observe({
    # Toggle dropdown on click
    shinyjs::onclick("confirmatory-analysis", {
      shinyjs::toggle("correlation-analysis")
      shinyjs::toggle("anova-analysis")
      shinyjs::toggle("bullet-charts")
    })
    
    
    shinyjs::onclick("data-exploration", {
      hideAllPanels()
      shinyjs::show("default-panel")
    })
    
    shinyjs::onclick("correlation-analysis", {
      hideAllPanels()
      shinyjs::show("correlation-panel")
    })
    
    shinyjs::onclick("anova-analysis", {
      hideAllPanels()
      shinyjs::show("anova-panel")
    })
    
    shinyjs::onclick("bullet-charts", {
      hideAllPanels()
      shinyjs::show("bullet-panel")
    })
    
    input$selectedRegions
    input$anovaRegions
    input$anovaRegions2
    input$anovaIndicators
    
    shinyjs::runjs("
      customizeSelectize('selectedRegions');
      customizeSelectize('anovaRegions');
      customizeSelectize('anovaRegions2');
      customizeSelectize('anovaIndicators');
      ")
    
    
    
  })
  
  # Dynamic ANOVA selectors based on analysis type
  output$dynamicAnovaSelector1 <- renderUI({
    switch(input$anovaAnalysisType,
           "indicator_across_diseases" = {
             selectInput("anovaWaterIndicator", "Select Water Indicator:",
                         choices = unique(merged_data$`Series.Name`),
                         selected = unique(merged_data$`Series.Name`)[1])
           },
           "disease_by_region" = {
             selectInput("anovaDisease", "Select Disease:",
                         choices = c("All Diseases" = "all",
                                     "Typhoid Rate" = "TyphoidRate",
                                     "Diarrhea Rate" = "DiarrheaRate",
                                     "Hepatitis Rate" = "HepatitisRate",
                                     "Unsafe Water Risk" = "UnsafeRisk"),
                         selected = "TyphoidRate")
           },
           "disease_by_indicator" = {
             selectInput("anovaDisease2", "Select Disease:",
                         choices = c("All Diseases" = "all",
                                     "Typhoid Rate" = "TyphoidRate",
                                     "Diarrhea Rate" = "DiarrheaRate",
                                     "Hepatitis Rate" = "HepatitisRate",
                                     "Unsafe Water Risk" = "UnsafeRisk"),
                         selected = "TyphoidRate")
           },
           
           "indicator_by_region" = {
             # No "All Indicators" option for this analysis type
             selectInput("anovaIndicator", "Select Water Indicator:",
                         choices = unique(merged_data$`Series.Name`),
                         selected = unique(merged_data$`Series.Name`)[1])
             
             
           },
           "all_diseases" = {
             selectInput("anovaRegionForDiseases", "Select Region:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = "all")
           },
           "all_indicators" = {
             selectInput("anovaRegionForIndicators", "Select Region:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = "all")
           }
    )
  })
  
  output$dynamicAnovaSelector2 <- renderUI({
    switch(input$anovaAnalysisType,
           "indicator_across_diseases" = {
             selectInput("anovaRegionForCrossDiseases", "Select Region:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = "all")
           },
           "disease_by_region" = {
             selectInput("anovaRegions", "Select Regions:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = c("all"),
                         multiple = TRUE)
           },
           "disease_by_indicator" = {
             selectInput("anovaIndicators", "Select Water Indicators:",
                         choices = c("All Indicators" = "all",
                                     unique(merged_data$`Series.Name`)),
                         selected = c("all"),
                         multiple = TRUE)
           },
           "indicator_by_region" = {
             selectInput("anovaRegions2", "Select Regions:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = c("all"),
                         multiple = TRUE)
           }
    )
  })
  
  # Reactive filter for correlation tab
  filtered_data_correlation <- reactive({
    req(input$selectedDisease)
    merged_data %>%
      filter(Year >= input$selectedYear[1],
             Year <= input$selectedYear[2],
             Region %in% input$selectedRegions,
             `Series.Name` %in% input$selectedIndicators) %>%
      mutate(DiseaseRate = .data[[input$selectedDisease]],
             AvgValue = round(AvgValue, 2))  # Round AvgValue to 2 decimal places
  })
  
  # Get year range for ANOVA
  get_anova_years <- reactive({
    if (input$anovaYearRange == "all") {
      return(c(min(merged_data$Year), max(merged_data$Year)))
    } else if (input$anovaYearRange == "last5") {
      max_year <- max(merged_data$Year)
      return(c(max_year - 4, max_year))
    } else if (input$anovaYearRange == "last10") {
      max_year <- max(merged_data$Year)
      return(c(max_year - 9, max_year))
    } else if (input$anovaYearRange == "custom") {
      return(input$customAnovaYearRange)
    }
  })
  
  # Reactive data for ANOVA analysis
  anova_data <- eventReactive(input$runAnovaBtn, {
    year_range <- get_anova_years()
    
    # Filter by year range
    data <- merged_data %>%
      filter(Year >= year_range[1], Year <= year_range[2])
    
    # Apply specific filters based on analysis type
    switch(input$anovaAnalysisType,
           "indicator_across_diseases" = {
             # Water indicator across all diseases
             diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
             
             # Filter by selected water indicator
             data <- data %>%
               filter(`Series.Name` == input$anovaWaterIndicator)
             
             # Filter by region if specified
             if (input$anovaRegionForCrossDiseases != "all") {
               data <- data %>% filter(Region == input$anovaRegionForCrossDiseases)
             }
             
             # Reshape to long format for comparing diseases
             data_long <- data %>%
               select(Year, Region, all_of(diseases)) %>%
               tidyr::pivot_longer(
                 cols = all_of(diseases),
                 names_to = "Disease",
                 values_to = "Rate"
               ) %>%
               filter(!is.na(Rate))
             
             return(list(
               data = data_long,
               x_var = "Disease",
               y_var = "Rate",
               title = paste("ANOVA:", input$anovaWaterIndicator, "Across Diseases")
             ))
           },
           "disease_by_region" = {
             if (input$anovaDisease != "all") {
               # Single disease across regions
               data <- data %>%
                 mutate(DiseaseRate = .data[[input$anovaDisease]]) %>%
                 filter(!is.na(DiseaseRate))
               
               if (!"all" %in% input$anovaRegions) {
                 data <- data %>% filter(Region %in% input$anovaRegions)
               }
               
               return(list(
                 data = data,
                 x_var = "Region",
                 y_var = "DiseaseRate",
                 title = paste("ANOVA:", input$anovaDisease, "across Regions")
               ))
             } else {
               # All diseases across regions - reshape data
               diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
               
               if (!"all" %in% input$anovaRegions) {
                 data <- data %>% filter(Region %in% input$anovaRegions)
               }
               
               # Reshape to long format
               data_long <- data %>%
                 select(Region, Year, all_of(diseases)) %>%
                 tidyr::pivot_longer(
                   cols = all_of(diseases),
                   names_to = "Disease",
                   values_to = "Rate"
                 ) %>%
                 filter(!is.na(Rate))
               
               return(list(
                 data = data_long,
                 x_var = "Region",
                 y_var = "Rate",
                 facet_var = "Disease",
                 title = "ANOVA: All Diseases across Regions"
               ))
             }
           },
           "disease_by_indicator" = {
             if (input$anovaDisease2 != "all") {
               # Single disease across indicators
               data <- data %>%
                 mutate(DiseaseRate = .data[[input$anovaDisease2]]) %>%
                 filter(!is.na(DiseaseRate))
               
               if (!"all" %in% input$anovaIndicators) {
                 data <- data %>% filter(`Series.Name` %in% input$anovaIndicators)
               }
               
               return(list(
                 data = data,
                 x_var = "Series.Name",
                 y_var = "DiseaseRate",
                 title = paste("ANOVA:", input$anovaDisease2, "across Water Indicators")
               ))
             } else {
               # All diseases across indicators - reshape data
               diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
               
               if (!"all" %in% input$anovaIndicators) {
                 data <- data %>% filter(`Series.Name` %in% input$anovaIndicators)
               }
               
               # Reshape to long format
               data_long <- data %>%
                 select(`Series.Name`, Year, all_of(diseases)) %>%
                 tidyr::pivot_longer(
                   cols = all_of(diseases),
                   names_to = "Disease",
                   values_to = "Rate"
                 ) %>%
                 filter(!is.na(Rate))
               
               return(list(
                 data = data_long,
                 x_var = "Series.Name",
                 y_var = "Rate",
                 facet_var = "Disease",
                 title = "ANOVA: All Diseases across Water Indicators"
               ))
             }
           },
           "indicator_by_region" = {
             if (input$anovaIndicator != "all") {
               # Single indicator across regions
               data <- data %>%
                 filter(`Series.Name` == input$anovaIndicator, !is.na(AvgValue))
               
               if (!"all" %in% input$anovaRegions2) {
                 data <- data %>% filter(Region %in% input$anovaRegions2)
               }
               
               return(list(
                 data = data,
                 x_var = "Region",
                 y_var = "AvgValue",
                 title = paste("ANOVA:", input$anovaIndicator, "across Regions")
               ))
             } else {
               # All indicators across regions
               if (!"all" %in% input$anovaRegions2) {
                 data <- data %>% filter(Region %in% input$anovaRegions2)
               }
               
               data <- data %>% filter(!is.na(AvgValue))
               
               return(list(
                 data = data,
                 x_var = "Region",
                 y_var = "AvgValue",
                 facet_var = "Series.Name",
                 title = "ANOVA: All Water Indicators across Regions"
               ))
             }
           },
           "all_diseases" = {
             # Compare all diseases
             diseases <- c("TyphoidRate", "DiarrheaRate", "HepatitisRate", "UnsafeRisk")
             
             if (input$anovaRegionForDiseases != "all") {
               data <- data %>% filter(Region == input$anovaRegionForDiseases)
             }
             
             # Reshape to long format
             data_long <- data %>%
               select(Year, all_of(diseases)) %>%
               tidyr::pivot_longer(
                 cols = all_of(diseases),
                 names_to = "Disease",
                 values_to = "Rate"
               ) %>%
               filter(!is.na(Rate))
             
             return(list(
               data = data_long,
               x_var = "Disease",
               y_var = "Rate",
               title = "Comparison of All Disease Rates"
             ))
           },
           "all_indicators" = {
             # Compare all water indicators
             if (input$anovaRegionForIndicators != "all") {
               data <- data %>% filter(Region == input$anovaRegionForIndicators)
             }
             
             data <- data %>% filter(!is.na(AvgValue))
             
             return(list(
               data = data,
               x_var = "Series.Name",
               y_var = "AvgValue",
               title = "Comparison of All Water Indicators"
             ))
           }
    )
  })
  
  # ANOVA Summary
  output$anovaSummary <- renderPrint({
    req(anova_data())
    
    anova_result <- anova_data()
    data <- anova_result$data
    x_var <- anova_result$x_var
    y_var <- anova_result$y_var
    
    # Run ANOVA test
    formula <- as.formula(paste(y_var, "~", x_var))
    anova_test <- aov(formula, data = data)
    summary(anova_test)
  })
  
  # New outputs for ANOVA summary statistics boxes
  output$anovaCountStat <- renderText({
    req(anova_data())
    nrow(anova_data()$data)
  })
  
  output$anovaFStat <- renderText({
    req(anova_data())
    
    anova_result <- anova_data()
    data <- anova_result$data
    x_var <- anova_result$x_var
    y_var <- anova_result$y_var
    
    formula <- as.formula(paste(y_var, "~", x_var))
    anova_test <- aov(formula, data = data)
    anova_summary <- summary(anova_test)
    
    # Extract F value from the summary
    f_value <- anova_summary[[1]]["F value"][[1]][1]
    round(f_value, 2)
  })
  
  output$anovaPValue <- renderText({
    req(anova_data())
    
    anova_result <- anova_data()
    data <- anova_result$data
    x_var <- anova_result$x_var
    y_var <- anova_result$y_var
    
    formula <- as.formula(paste(y_var, "~", x_var))
    anova_test <- aov(formula, data = data)
    anova_summary <- summary(anova_test)
    
    # Extract p-value from the summary
    p_value <- anova_summary[[1]]["Pr(>F)"][[1]][1]
    
    # Format p-value for display
    if (p_value < 0.001) {
      return("<0.001")
    } else {
      return(as.character(round(p_value, 3)))
    }
  })
  
  output$anovaRSquared <- renderText({
    req(anova_data())
    
    anova_result <- anova_data()
    data <- anova_result$data
    x_var <- anova_result$x_var
    y_var <- anova_result$y_var
    
    # Calculate R-squared
    model <- lm(as.formula(paste(y_var, "~", x_var)), data = data)
    r_squared <- summary(model)$r.squared
    round(r_squared, 2)
  })
  
  # ANOVA Plot
  # ANOVA Plot
  output$anovaPlot <- renderPlot({
    req(anova_data())
    
    anova_result <- anova_data()
    data <- anova_result$data
    x_var <- anova_result$x_var
    y_var <- anova_result$y_var
    title <- anova_result$title
    
    validate(
      need(nrow(data) > 1, "No data available for the selected criteria."),
      need(x_var %in% colnames(data), "Invalid X variable."),
      need(y_var %in% colnames(data), "Invalid Y variable.")
    )
    
    # Set plot parameters for better appearance
    y_min <- min(data[[y_var]], na.rm = TRUE)
    y_max <- max(data[[y_var]], na.rm = TRUE)
    y_breaks <- pretty(c(y_min, y_max), n = 10)
    
    # Enhanced ggplot components for better styling
    custom_components <- list(
      theme_minimal(),
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")
      ),
      scale_y_continuous(
        limits = c(max(0, y_min - (y_max - y_min) * 0.1), y_max + (y_max - y_min) * 0.1),
        breaks = y_breaks
      ),
      scale_fill_brewer(palette = "Set2")
    )
    
    # Use ggstatsplot with enhanced parameters to match the second image style
    if (!is.null(anova_result$facet_var)) {
      ggstatsplot::grouped_ggbetweenstats(
        data = data,
        x = !!rlang::sym(x_var),
        y = !!rlang::sym(y_var),
        grouping.var = !!rlang::sym(anova_result$facet_var),
        xlab = x_var,
        ylab = y_var,
        title = title,
        type = "parametric", # Use parametric tests like in the example
        pairwise.comparisons = TRUE,
        pairwise.display = "significant",
        p.adjust.method = "fdr", # FDR correction as shown in the example
        bf.message = TRUE, # Show Bayes Factor
        results.subtitle = TRUE,
        violin.args = list(width = 0.8, alpha = 0.6),
        point.args = list(size = 2, alpha = 0.6),
        centrality.point.args = list(size = 4, color = "darkred"),
        ggplot.component = custom_components
      )
    } else {
      ggstatsplot::ggbetweenstats(
        data = data,
        x = !!rlang::sym(x_var),
        y = !!rlang::sym(y_var),
        xlab = x_var,
        ylab = y_var,
        title = title,
        type = "parametric", # Use parametric tests like in the example
        pairwise.comparisons = TRUE,
        pairwise.display = "significant",
        p.adjust.method = "fdr", # FDR correction as shown in the example
        bf.message = TRUE, # Show Bayes Factor
        results.subtitle = TRUE,
        violin.args = list(width = 0.8, alpha = 0.6),
        point.args = list(size = 2, alpha = 0.6),
        centrality.point.args = list(size = 4, color = "darkred"),
        ggplot.component = custom_components
      )
    }
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
                    paste(input$selectedIndicators, collapse = ", "),
                    "and", input$selectedDisease),
      marginal = TRUE
    )
  })
  
  # Data Table for Correlation Tab
  output$dataTable <- DT::renderDataTable({
    df <- filtered_data_correlation()
    # Apply search filter if provided
    if (!is.null(input$searchText) && input$searchText != "") {
      search_text <- tolower(input$searchText)
      df <- df %>%
        filter_all(any_vars(grepl(search_text, tolower(as.character(.)))))
    }
    
    DT::datatable(
      df,
      options = list(
        pageLength = as.numeric(input$pageSize),
        lengthMenu = c(5, 10, 15, 20, 50),
        searching = FALSE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover',
      filter = 'top',
      extensions = c('Buttons')
    ) %>%
      DT::formatRound(columns = c("AvgValue", "DiseaseRate"), digits = 2) %>%
      DT::formatStyle(
        columns = colnames(df),
        backgroundColor = DT::styleEqual(c(NA), c('rgba(255, 0, 0, 0.1)')),
        valueColumns = colnames(df)
      )
  })
  
  # Summary Statistics for Correlation Tab - Individual outputs
  output$countStat <- renderText({
    df <- filtered_data_correlation()
    nrow(df)
  })
  
  output$waterValueStat <- renderText({
    df <- filtered_data_correlation()
    round(mean(df$AvgValue, na.rm = TRUE), 2)
  })
  
  output$diseaseRateStat <- renderText({
    df <- filtered_data_correlation()
    round(mean(df$DiseaseRate, na.rm = TRUE), 2)
  })
  
  output$correlationStat <- renderText({
    df <- filtered_data_correlation()
    round(cor(df$AvgValue, df$DiseaseRate, use = "complete.obs"), 2)
  })
  
  
  
  # Dynamic UI for bullet charts
  output$dynamic_indicator_auto <- renderUI({
    if (input$indicator_type_auto == "Water") {
      # Filter out excluded indicators
      excluded_indicators <- c(
        "Population density (people per sq. km of land area)", 
        "Population, total",
        "People with basic handwashing facilities including soap and water (% of population)"
      )
      
      available_indicators <- unique(merged_data$`Series.Name`)[!unique(merged_data$`Series.Name`) %in% excluded_indicators]
      
      selectInput("selected_indicator_auto", "Select Water Indicator:",
                  choices = available_indicators,
                  selected = available_indicators[1])
    } else {
      selectInput("selected_disease_auto", "Select Disease:",
                  choices = c("Typhoid Rate" = "TyphoidRate",
                              "Diarrhea Rate" = "DiarrheaRate",
                              "Hepatitis Rate" = "HepatitisRate",
                              "Unsafe Water Risk" = "UnsafeRisk"),
                  selected = "TyphoidRate")
    }
  })
  
  # Reactive data preparation for bullet chart
  # Reactive data preparation for bullet chart
  bullet_data <- reactive({
    req(input$indicator_type_auto, input$auto_year)
    
    if (input$indicator_type_auto == "Water") {
      req(input$selected_indicator_auto)
      
      # Set appropriate target based on indicator
      target_value <- if(input$selected_indicator_auto == "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)") {
        NA  # No target for water productivity
      } else if(input$selected_indicator_auto == "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources") {
        0  # Target for water stress is 0
      } else if(input$selected_indicator_auto == "People practicing open defecation (% of population)") {
        0  # Target for open defecation is 0
      } else {
        100  # Default target for other indicators
      }
      
      # First collect and sort the data
      water_data <- merged_data %>%
        filter(Series.Name == input$selected_indicator_auto,
               !is.na(AvgValue),
               Year <= input$auto_year) %>%
        arrange(Year)
      
      # Then group and summarize
      water_data %>%
        group_by(Region) %>%
        summarise(
          Min = min(AvgValue, na.rm = TRUE),
          Max = max(AvgValue, na.rm = TRUE),
          Average = round(mean(AvgValue, na.rm = TRUE), 1),
          # Create a vector of values for each year in chronological order
          Monthly = list(AvgValue),
          Actual = if(any(Year == input$auto_year)) mean(AvgValue[Year == input$auto_year], na.rm = TRUE) else NA,
          Target = if(is.na(target_value)) max(AvgValue, na.rm = TRUE) else target_value,
          .groups = "drop"
        )
    } else {
      req(input$selected_disease_auto)
      
      # First collect and sort the data
      disease_data <- merged_data %>%
        filter(!is.na(.data[[input$selected_disease_auto]]),
               Year <= input$auto_year) %>%
        arrange(Year)
      
      # Then group and summarize
      disease_data %>%
        group_by(Region) %>%
        summarise(
          Min = min(.data[[input$selected_disease_auto]], na.rm = TRUE),
          Max = max(.data[[input$selected_disease_auto]], na.rm = TRUE),
          Average = round(mean(.data[[input$selected_disease_auto]], na.rm = TRUE), 1),
          # Create a vector of values for each year in chronological order
          Monthly = list(.data[[input$selected_disease_auto]]),
          Actual = if(any(Year == input$auto_year)) mean(.data[[input$selected_disease_auto]][Year == input$auto_year], na.rm = TRUE) else NA,
          Target = if(input$selected_disease_auto %in% c("UnsafeRisk", "DiarrheaRate","HepatitisRate","TyphoidRate")) 0 else max(.data[[input$selected_disease_auto]], na.rm = TRUE),
          .groups = "drop"
        )
    }
  })
  
  # Bullet Chart table
  output$bullet_table_auto <- gt::render_gt({
    req(bullet_data())
    
    # Make sure we have data
    validate(
      need(nrow(bullet_data()) > 0, "No data available for the selected criteria.")
    )
    
    # Determine if we should show the target column
    show_target <- TRUE
    if(input$indicator_type_auto == "Water" && 
       input$selected_indicator_auto == "Water productivity, total (constant 2015US$ GDP per cubic meter of total fresh water withdrawal)") {
      show_target <- FALSE
    }
    
    # Create the bullet chart
    chart <- bullet_data() %>%
      arrange(desc(Actual)) %>%  # Sort by actual value descending
      head(10) %>% # Show top 10 regions
      gt() %>%
      fmt_number(columns = c("Min", "Max", "Average", "Actual"), decimals = 1)
    
    # Add sparkline
    chart <- chart %>% gtExtras::gt_plt_sparkline(column = "Monthly", type = "default")
    
    # Add bullet chart if target should be shown
    if(show_target) {
      chart <- chart %>% gtExtras::gt_plt_bullet(
        column = "Actual",
        target = "Target",
        width = 108,
        palette = if (input$indicator_type_auto == "Water") c("lightblue", "black") else c("salmon", "black")
      )
    }
    
    # Set column labels
    chart <- chart %>% cols_label(
      Min = "Min",
      Max = "Max",
      Average = "Avg",
      Actual = "Value",
      Monthly = "Trend",
      Target = "Target"
    )
    
    # Add header
    chart <- chart %>% tab_header(
      title = if(input$indicator_type_auto == "Water") {
        md(paste("**Top Regions by", input$selected_indicator_auto, "in", input$auto_year, "**"))
      } else {
        md(paste("**Top Regions by", gsub("Rate", "", input$selected_disease_auto), "Rate in", input$auto_year, "**"))
      }
    )
    
    # Apply theme
    chart %>% gtExtras::gt_theme_espn()
  })
  
  # Additional insights output
  output$data_insights <- renderPrint({
    req(bullet_data())
    
    insights <- bullet_data() %>%
      summarise(
        total_regions = n(),
        overall_min = min(Min, na.rm = TRUE),
        overall_max = max(Max, na.rm = TRUE),
        overall_avg = mean(Average, na.rm = TRUE),
        highest_actual = max(Actual, na.rm = TRUE),
        lowest_actual = min(Actual, na.rm = TRUE)
      )
    
    cat("Data Insights:\n")
    cat("Total Regions:", insights$total_regions, "\n")
    cat("Overall Minimum Value:", round(insights$overall_min, 2), "\n")
    cat("Overall Maximum Value:", round(insights$overall_max, 2), "\n")
    cat("Overall Average:", round(insights$overall_avg, 2), "\n")
    cat("Highest Actual Value:", round(insights$highest_actual, 2), "\n")
    cat("Lowest Actual Value:", round(insights$lowest_actual, 2), "\n")
  })
  
  
  
  
  
  
  
  # Dynamic ANOVA selectors based on analysis type
  output$dynamicAnovaSelector1 <- renderUI({
    switch(input$anovaAnalysisType,
           "indicator_across_diseases" = {
             selectInput("anovaWaterIndicator", "Select Water Indicator:",
                         choices = unique(merged_data$`Series.Name`),
                         selected = unique(merged_data$`Series.Name`)[1])
           },
           "disease_by_region" = {
             selectInput("anovaDisease", "Select Disease:",
                         choices = c("All Diseases" = "all",
                                     "Typhoid Rate" = "TyphoidRate",
                                     "Diarrhea Rate" = "DiarrheaRate",
                                     "Hepatitis Rate" = "HepatitisRate",
                                     "Unsafe Water Risk" = "UnsafeRisk"),
                         selected = "TyphoidRate")
           },
           "disease_by_indicator" = {
             selectInput("anovaDisease2", "Select Disease:",
                         choices = c("All Diseases" = "all",
                                     "Typhoid Rate" = "TyphoidRate",
                                     "Diarrhea Rate" = "DiarrheaRate",
                                     "Hepatitis Rate" = "HepatitisRate",
                                     "Unsafe Water Risk" = "UnsafeRisk"),
                         selected = "TyphoidRate")
           },
           "indicator_by_region" = {
             selectInput("anovaIndicator", "Select Water Indicator:",
                         choices = c("All Indicators" = "all",
                                     unique(merged_data$`Series.Name`)),
                         selected = unique(merged_data$`Series.Name`)[1])
           },
           "all_diseases" = {
             selectInput("anovaRegionForDiseases", "Select Region:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = "all")
           },
           "all_indicators" = {
             selectInput("anovaRegionForIndicators", "Select Region:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = "all")
           }
    )
  })
  
  output$dynamicAnovaSelector2 <- renderUI({
    switch(input$anovaAnalysisType,
           "indicator_across_diseases" = {
             selectInput("anovaRegionForCrossDiseases", "Select Region:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = "all")
           },
           "disease_by_region" = {
             selectInput("anovaRegions", "Select Regions:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = c("all"),
                         multiple = TRUE)
           },
           "disease_by_indicator" = {
             selectInput("anovaIndicators", "Select Water Indicators:",
                         choices = c("All Indicators" = "all",
                                     unique(merged_data$`Series.Name`)),
                         selected = c("all"),
                         multiple = TRUE)
           },
           "indicator_by_region" = {
             selectInput("anovaRegions2", "Select Regions:",
                         choices = c("All Regions" = "all",
                                     unique(merged_data$Region)),
                         selected = c("all"),
                         multiple = TRUE)
           }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
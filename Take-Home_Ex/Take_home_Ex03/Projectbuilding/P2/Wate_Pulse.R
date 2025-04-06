library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(plm)
library(ggplot2)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(ggstatsplot) 

pdata <- read_csv("final_merged_data.csv") %>%
  rename(
    country = Country,
    region = `World Bank Regions`,
    population = `Population, total`,
    water_stress = `Level of water stress: freshwater withdrawal as a proportion of available freshwater resources`,
    drinking_water = `People using at least basic drinking water services (% of population)`,
    sanitation = `People using at least basic sanitation services (% of population)`,
    open_defecation = `People practicing open defecation (% of population)`,
    water_productivity = `Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)`,
    Population_density = `Population density (people per sq. km of land area)`,
    hepatitis = `Acute hepatitis A`,
    diarrhea = `Diarrheal diseases`,
    typhoid = `Typhoid fever`
  )


pdata <- pdata %>%
  mutate(year_group = case_when(
    year <= 2010 ~ "2006-2010",
    year <= 2015 ~ "2011-2015",
    TRUE ~ "2016-2021"
  ))


indicator_choices <- c(
  "Open Defecation" = "open_defecation",
  "Drinking Water Access" = "drinking_water",
  "Sanitation Access" = "sanitation",
  "Water Productivity" = "water_productivity",
  "Water Stress" = "water_stress",
  "Population Density" = "Population_density" ,
  "Diarrheal Rate" = "diarrhea",
  "Hepatitis A Rate" = "hepatitis",
  "Typhoid Fever Rate" = "typhoid"
)


disease_indicators <- c(
  "Diarrheal Rate" = "diarrhea",
  "Hepatitis A Rate" = "hepatitis",
  "Typhoid Fever Rate" = "typhoid"
)

名
water_indicators <- c(
  "Open Defecation" = "open_defecation",
  "Drinking Water Access" = "drinking_water",
  "Sanitation Access" = "sanitation", 
  "Water Productivity" = "water_productivity",
  "Water Stress" = "water_stress",
  "Population Density" = "Population_density"  
)

# ================== UI ==================================================
ui <- dashboardPage(
  dashboardHeader(title = "💧 Blue Pulse"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Indicator Dictionary", tabName = "dictionary", icon = icon("book")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("search")),
      menuItem("Panel Data Modeling", tabName = "plm", icon = icon("wrench"),
               menuSubItem("Model Builder", tabName = "model"),
               menuSubItem("Model Comparison", tabName = "comparison"),
               menuSubItem("Year Analysis", tabName = "yearanalysis"),
               menuSubItem("Diagnostics", tabName = "diagnostics"),
               menuSubItem("About PLM", tabName = "about")),
      menuItem("Confirmatory Data Analysis", tabName = "cda", icon = icon("chart-line"),
               menuSubItem("Time Trend Analysis", tabName = "bubble"),
               menuSubItem("Correlation Analysis", tabName = "correlation"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .dictionary-entry h5 {
          color: #2c3e50;
          margin-top: 20px;
        }
        .dictionary-entry p {
          margin-left: 15px;
        }
        .dictionary-entry hr {
          margin-top: 15px;
          margin-bottom: 15px;
          border-top: 1px solid #eee;
        }
      "))
    ),
    tabItems(
      # =======================================================
      # Exploratory Data Analysis
      # =======================================================
      tabItem(tabName = "eda",
              fluidRow(
                box(width = 3, title = "Select and Visualize Indicators",
                    selectInput("eda_indicator", "Indicator:", choices = indicator_choices),
                    
                    selectInput("eda_year", "Select Year:", 
                                choices = as.character(min(pdata$year):max(pdata$year)),
                                selected = as.character(max(pdata$year))),
                    
                    selectInput("eda_group", "Group By:", choices = c("Region" = "region", "Country" = "country")),
                    
                    conditionalPanel(
                      condition = "input.eda_group == 'region'",
                      selectizeInput("eda_regions", "Select Regions:", 
                                     choices = unique(pdata$region), 
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')),
                                     selected = NULL)
                    ),
）
                    conditionalPanel(
                      condition = "input.eda_group == 'country'",
                      selectInput("eda_region_filter", "Filter by Region:", 
                                  choices = c("All Regions" = "all", unique(pdata$region))),
                      selectizeInput("eda_countries", "Select Countries:", 
                                     choices = unique(pdata$country), 
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')),
                                     selected = NULL)
                    ),

                    radioButtons("eda_plot_type", "Plot Type:", 
                                 choices = c("Box Plot" = "box", "Violin Chart" = "time"),
                                 selected = "box"),
                    
                    actionButton("update_eda", "Update Visualization", class = "btn-primary")
                ),
                box(width = 9, title = textOutput("eda_box_plot_title"),
                    conditionalPanel(
                      condition = "input.eda_plot_type == 'box'",
                      plotlyOutput("eda_box_plot", height = "450px")
                    ),
                    conditionalPanel(
                      condition = "input.eda_plot_type == 'time'",
                      plotOutput("eda_time_plot", height = "450px") 
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Data Table",
                    DT::dataTableOutput("eda_data_table")
                )
              )
      ),
      
      # =======================================================
      # Model Builder
      # =======================================================
      tabItem(tabName = "model",

              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  box(title = "Model Configuration", width = 12, status = "primary",
                      selectInput("disease", "Dependent Variable:",
                                  choices = disease_indicators,
                                  selected = "diarrhea"),
                      selectInput("model_type", "Model Type:",
                                  choices = c("Pooled OLS", "Fixed Effects", "Random Effects", 
                                              "Fixed Effects with Time", "Dynamic Panel"),
                                  selected = "Fixed Effects"),
                      checkboxGroupInput("variables", "Independent Variables:",
                                         choices = water_indicators,
                                         selected = c("open_defecation", "drinking_water", "sanitation")),
                      sliderInput("year_range", "Select Year Range:",
                                  min = min(pdata$year), max = max(pdata$year),
                                  value = c(min(pdata$year), max(pdata$year)),
                                  step = 1, sep = ""),
                      selectInput("region", "Select Region:",
                                  choices = c("All Regions", unique(pdata$region)),
                                  selected = "All Regions"),
                      selectizeInput("countries", "Select Countries:",
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')))
                  )
                ),
                mainPanel(
                  width = 9,
                  fluidRow(
                    infoBoxOutput("model_type_box", width = 4),
                    infoBoxOutput("observations_box", width = 4),
                    infoBoxOutput("countries_box", width = 4)
                  ),
                  fluidRow(
                    box(title = "Coefficient Plot", width = 6,
                        plotOutput("coef_plot")),
                    box(title = "Model Statistics", width = 6,
                        tableOutput("model_stats"),
                        tags$hr(),
                        verbatimTextOutput("r_code"))
                  ),
                  fluidRow(
                    box(title = "Model Summary", width = 12, status = "primary",
                        htmlOutput("model_info"),
                        verbatimTextOutput("model_summary")
                    )
                  )
                )
              )
      ),
      
      # =======================================================
      # Model Comparison
      # =======================================================
      tabItem(tabName = "comparison",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  box(title = "Model Configuration", width = 12, status = "primary",
                      selectInput("comp_disease", "Dependent Variable:",
                                  choices = disease_indicators,
                                  selected = "diarrhea"),
                      

                      selectizeInput("compare_models", "Models to Compare:",
                                     choices = c("Pooled OLS", "Fixed Effects", "Random Effects", 
                                                 "Fixed Effects with Time", "Dynamic Panel"),
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')),
                                     selected = c("Fixed Effects", "Random Effects")),
                      
                      checkboxGroupInput("comp_variables", "Independent Variables:",
                                         choices = water_indicators,
                                         selected = c("open_defecation", "drinking_water", "sanitation")),
                      
                      sliderInput("comp_year_range", "Select Year Range:",
                                  min = min(pdata$year), max = max(pdata$year),
                                  value = c(min(pdata$year), max(pdata$year)),
                                  step = 1, sep = ""),
                      
                      selectInput("comp_region", "Region Filter:",
                                  choices = c("All Regions", unique(pdata$region)),
                                  selected = "All Regions"),
                      
                      selectizeInput("comp_countries", "Country Filter:",
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')))
                  )
                ),
                mainPanel(
                  width = 9,
                  fluidRow(
                    box(title = "Coefficient Comparison", width = 12,
                        plotOutput("model_comparison_plot", height = "500px"))
                  ),
                  fluidRow(
                    box(title = "Model Statistics Comparison", width = 12,
                        DT::dataTableOutput("model_comparison_stats"))
                  )
                )
              )
      ),
      
      # =======================================================
      # Year Analysis
      # =======================================================
      tabItem(tabName = "yearanalysis",

              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  box(title = "Model Configuration", width = 12, status = "primary",

                      selectInput("year_disease", "Dependent Variable:",
                                  choices = disease_indicators,
                                  selected = "diarrhea"),
                      
                      sliderInput("year_range_analysis", "Years to Analyze:",
                                  min = min(pdata$year), max = max(pdata$year),
                                  value = c(min(pdata$year), max(pdata$year)),
                                  step = 1, sep = ""),
                      
                      checkboxGroupInput("year_variables", "Independent Variables:",
                                         choices = water_indicators,
                                         selected = c("open_defecation", "drinking_water", "sanitation")),
                      
                      selectInput("year_region", "Region Filter:",
                                  choices = c("All Regions", unique(pdata$region)),
                                  selected = "All Regions"),
                      
                      selectizeInput("year_countries", "Country Filter:",
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button'))),
                      
                      p("This tab shows how coefficient values change across different years.")
                  )
                ),
                mainPanel(
                  width = 9,
                  fluidRow(
                    box(title = "Yearly Coefficient Plot", width = 12,
                        plotOutput("yearly_coef_plot", height = "500px"))
                  ),
                  fluidRow(
                    box(title = "Yearly Model Statistics", width = 12,
                        DT::dataTableOutput("yearly_stats_table"))
                  ),

                  fluidRow(
                    conditionalPanel(
                      condition = "input.year_region == 'North America'",
                      box(title = "Data Availability for North America", width = 12,
                          uiOutput("data_availability")
                      )
                    )
                  )
                )
              )
      ),
      
      # =======================================================
      # Diagnostics
      # =======================================================
      tabItem(tabName = "diagnostics",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  box(title = "Model Configuration", width = 12, status = "primary",

                      selectInput("diag_disease", "Dependent Variable:",
                                  choices = disease_indicators,
                                  selected = "diarrhea"),
                      
                      selectInput("diag_model_type", "Model Type:",
                                  choices = c("Pooled OLS", "Fixed Effects", "Random Effects", 
                                              "Fixed Effects with Time", "Dynamic Panel"),
                                  selected = "Fixed Effects"),
                      
                      checkboxGroupInput("diag_variables", "Independent Variables:",
                                         choices = water_indicators,
                                         selected = c("open_defecation", "drinking_water", "sanitation")),
                      
                      sliderInput("diag_year_range", "Select Year Range:",
                                  min = min(pdata$year), max = max(pdata$year),
                                  value = c(min(pdata$year), max(pdata$year)),
                                  step = 1, sep = ""),
                      
                      selectInput("diag_region", "Region Filter:",
                                  choices = c("All Regions", unique(pdata$region)),
                                  selected = "All Regions"),
                      
                      selectizeInput("diag_countries", "Country Filter:",
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')))
                  )
                ),
                mainPanel(
                  width = 9,
                  fluidRow(
                    box(title = "Residual Plot", width = 6,
                        plotOutput("residual_plot")),
                    box(title = "Normal Q-Q Plot", width = 6,
                        plotOutput("qq_plot"))
                  ),
                  fluidRow(
                    box(title = "Hausman Test (Fixed vs Random Effects)", width = 6, status = "info",
                        htmlOutput("hausman_info"),
                        verbatimTextOutput("hausman_test")),
                    box(title = "Serial Correlation Test", width = 6, status = "info",
                        htmlOutput("serial_info"),
                        verbatimTextOutput("serial_test"))
                  )
                )
              )
      ),      
      
      # =======================================================
      # Panel Modeling - About PLM
      # =======================================================
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About Panel Data Analysis with PLM", width = 12,
                    tags$div(
                      tags$h3("What is Panel Data?"),
                      p("Panel data (also known as longitudinal data) consists of observations on the same cross-sectional units (e.g., countries, firms, individuals) over multiple time periods. This two-dimensional structure allows researchers to control for time-invariant unobserved characteristics."),
                      
                      tags$h3("Key Panel Data Models:"),
                      tags$ul(
                        tags$li(tags$b("Pooled OLS:"), "Treats panel data as one big cross-section, ignoring individual/time effects. Works with both panel and cross-sectional data."),
                        tags$li(tags$b("Fixed Effects:"), "Controls for time-invariant individual differences. Requires multiple time periods and entities."),
                        tags$li(tags$b("Random Effects:"), "Assumes individual effects are random variables. Requires multiple time periods and entities."),
                        tags$li(tags$b("Fixed Effects with Time:"), "Controls for both individual and time effects. Requires multiple time periods and entities."),
                        tags$li(tags$b("Dynamic Panel:"), "Includes lagged dependent variable for time dependencies. Requires multiple time periods and entities.")
                      ),
                      
                      tags$h3("Key Diagnostic Tests:"),
                      tags$ul(
                        tags$li(tags$b("Hausman Test:"), "Tests whether Fixed or Random Effects is more appropriate"),
                        tags$li(tags$b("Breusch-Godfrey/Wooldridge Test:"), "Tests for serial correlation in panel models"),
                        tags$li(tags$b("F Test:"), "Tests joint significance of the individual effects")
                      ),
                      
                      tags$h3("Key Model Statistics:"),
                      tags$ul(
                        tags$li(tags$b("R-squared:"), "Proportion of variance explained by the model"),
                        tags$li(tags$b("Adjusted R-squared:"), "R-squared adjusted for the number of predictors"),
                        tags$li(tags$b("F-statistic:"), "Test of overall model significance"),
                        tags$li(tags$b("p-value:"), "Probability of observing the data if the null hypothesis is true")
                      )
                    )
                )
              )
      ),
      
      # =======================================================
      # Bubble Chart (Time Trend Analysis)
      # =======================================================
      tabItem(tabName = "bubble",
              fluidRow(
                box(width = 3, title = "Select Parameters",
                    selectInput("bubble_region", "Region", c("All Regions" = "all", unique(pdata$region))),
                    selectizeInput("bubble_countries", "Countries", 
                                   choices = NULL, 
                                   multiple = TRUE,
                                   options = list(plugins = list('remove_button'))),
                    sliderInput("bubble_year_range", "Year Range", 
                                min = min(pdata$year),
                                max = max(pdata$year),
                                value = c(min(pdata$year), max(pdata$year))),
                    selectInput("xvar", "X-axis", choices = indicator_choices, selected = "drinking_water"),
                    selectInput("yvar", "Y-axis", choices = indicator_choices, selected = "diarrhea"),
                    selectInput("colorvar", "Color by", choices = c("None" = "none", indicator_choices), selected = "none"),
                    checkboxInput("show_trend", "Show Trend Line", FALSE),
                    sliderInput("animation_speed", "Animation Speed", min = 0.1, max = 2, value = 1, step = 0.1)
                ),
                box(width = 9, title = "Animated Bubble Chart",
                    fluidRow(
                      valueBoxOutput("correlation_box", width = 6),
                      valueBoxOutput("data_count_box", width = 6)
                    ),
                    plotlyOutput("bubblePlot", height = "500px"),
                    htmlOutput("plot_interpretation")
                )
              )
      ),
      
      # =======================================================
      # Correlation Analysis
      # =======================================================
      tabItem(tabName = "correlation",
              fluidRow(
                column(width = 3,
                       box(width = 12, title = "Correlation Settings",
                           selectInput("corr_region", "Region", c("All Regions" = "all", unique(pdata$region))),
                           selectizeInput("corr_countries", "Countries", 
                                          choices = NULL, 
                                          multiple = TRUE,
                                          options = list(plugins = list('remove_button'))),
                           selectInput("corr_year", "Year", choices = NULL),
                           selectInput("research_direction", "Research Direction", 
                                       choices = c("Water Impact on Disease" = "water_to_disease", 
                                                   "Disease Impact on Water" = "disease_to_water"),
                                       selected = "water_to_disease"),
                           conditionalPanel(
                             condition = "input.research_direction == 'water_to_disease'",
                             selectInput("corr_disease", "Disease Indicator", 
                                         choices = disease_indicators),
                             selectizeInput("corr_water_indicators", "Water Indicators to Include", 
                                            choices = water_indicators, 
                                            multiple = TRUE,
                                            options = list(plugins = list('remove_button')),
                                            selected = c("drinking_water", "sanitation"))
                           ),
                           conditionalPanel(
                             condition = "input.research_direction == 'disease_to_water'",
                             selectInput("corr_water", "Water Indicator", 
                                         choices = water_indicators,
                                         selected = "drinking_water"),
                             selectizeInput("corr_disease_indicators", "Disease Indicators to Include", 
                                            choices = disease_indicators,
                                            multiple = TRUE,
                                            options = list(plugins = list('remove_button')),
                                            selected = c("diarrhea", "typhoid"))
                           ),
                           actionButton("update_corr", "Update", class = "btn-primary")
                       )
                ),
                column(width = 9,
                       fluidRow(
                         column(width = 6, valueBoxOutput("max_corr_box", width = NULL)),
                         column(width = 6, valueBoxOutput("min_corr_box", width = NULL))
                       ),
                       fluidRow(
                         box(width = 12, title = "Correlation Chart", plotlyOutput("corrPlot", height = "400px"))
                       ),
                       fluidRow(
                         box(width = 12, htmlOutput("corr_interpretation"))
                       )
                )
              )
      ),
      
      # =======================================================
      # Indicator Dictionary 
      # =======================================================
      tabItem(tabName = "dictionary",
              fluidRow(
                box(width = 12, title = "Water and Health Indicators Dictionary",
                    status = "primary", solidHeader = TRUE,
                    
                    h4("Water Indicators"),
                    
                    # Water Stress
                    div(class = "dictionary-entry",
                        h5("Water Stress"),
                        p(strong("Definition:"), "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources"),
                        p("The level of water stress is the ratio between total freshwater withdrawn by all major sectors and total renewable freshwater resources, after taking into account environmental water requirements. Main sectors, as defined by ISIC standards, include agriculture; forestry and fishing; manufacturing; electricity industry; and services. This indicator is also known as water withdrawal intensity."),
                        hr()
                    ),
                    
                    # Open Defecation
                    div(class = "dictionary-entry",
                        h5("Open Defecation"),
                        p(strong("Definition:"), "People practicing open defecation (% of population)"),
                        p("This metric refers to the percentage of the population defecating in the open, such as in fields, forest, bushes, open bodies of water, on beaches, in other open spaces or disposed of with solid waste."),
                        hr()
                    ),
                    
                    # Basic Drinking Water Services
                    div(class = "dictionary-entry",
                        h5("Basic Drinking Water Services"),
                        p(strong("Definition:"), "People using at least basic drinking water services (% of population)"),
                        p("The percentage of people using at least basic water services. This indicator encompasses both people using basic water services as well as those using safely managed water services."),
                        p("Basic drinking water services is defined as drinking water from an improved source, provided collection time is not more than 30 minutes for a round trip. Improved water sources include piped water, boreholes or tubewells, protected dug wells, protected springs, and packaged or delivered water."),
                        hr()
                    ),
                    
                    # Basic Sanitation Services
                    div(class = "dictionary-entry",
                        h5("Basic Sanitation Services"),
                        p(strong("Definition:"), "People using at least basic sanitation services (% of population)"),
                        p("The percentage of people using at least basic sanitation services, that is, improved sanitation facilities that are not shared with other households. This indicator encompasses both people using basic sanitation services as well as those using safely managed sanitation services."),
                        p("Improved sanitation facilities include flush/pour flush to piped sewer systems, septic tanks or pit latrines; ventilated improved pit latrines, compositing toilets or pit latrines with slabs."),
                        hr()
                    ),
                    
                    # Water Productivity
                    div(class = "dictionary-entry",
                        h5("Water Productivity"),
                        p(strong("Definition:"), "Water productivity, total (constant 2015 US$ GDP per cubic meter of total freshwater withdrawal)"),
                        p("Water productivity is calculated as GDP in constant prices divided by annual total water withdrawal."),
                        hr()
                    ),
                    
                    
                    h4("Health Indicators"),
                    
                    # Hepatitis A
                    div(class = "dictionary-entry",
                        h5("Hepatitis A"),
                        p(strong("Definition:"), "Acute hepatitis A incidence rate"),
                        p("Hepatitis A is a liver disease caused by the hepatitis A virus. The disease is closely associated with unsafe water, inadequate sanitation and poor personal hygiene."),
                        hr()
                    ),
                    
                    # Diarrheal Diseases
                    div(class = "dictionary-entry",
                        h5("Diarrheal Diseases"),
                        p(strong("Definition:"), "Diarrheal diseases incidence rate"),
                        p("Diarrheal diseases are a group of conditions caused by infection and inflammation of the gastrointestinal tract. These diseases are strongly linked to conditions of poor water quality, sanitation, and hygiene."),
                        hr()
                    ),
                    
                    # Typhoid Fever
                    div(class = "dictionary-entry",
                        h5("Typhoid Fever"),
                        p(strong("Definition:"), "Typhoid fever incidence rate"),
                        p("Typhoid fever is a bacterial infection caused by Salmonella Typhi. It is transmitted through the ingestion of food or water contaminated by the feces or urine of infected people."),
                        hr()
                         ),
                    
                    
                    h4("Others"),
                    
                    div(class = "dictionary-entry",
                        h5("Population Density"),
                        p(strong("Definition:"), "Population density (people per sq. km of land area)"),
                        p("Population density is midyear population divided by land area in square kilometers. Population is based on the de facto definition of population, which counts all residents regardless of legal status or citizenship."),
                        hr()
                    ),
                    
                )
              )
      )
    )
  )
)

# =========================== Server ======================================
server <- function(input, output, session) {
  
  observe({
    req(input$comp_region)
    countries_to_show <- if(input$comp_region == "All Regions") {
      sort(unique(pdata$country))
    } else {
      sort(unique(pdata$country[pdata$region == input$comp_region]))
    }
    
    print(paste("Found", length(countries_to_show), "countries for comparison region:", input$comp_region))
    
    updateSelectizeInput(session, "comp_countries", 
                         choices = countries_to_show,
                         options = list(plugins = list('remove_button')))
  })
  
  observe({
    req(input$year_region)
    countries_to_show <- if(input$year_region == "All Regions") {
      sort(unique(pdata$country))
    } else {
      sort(unique(pdata$country[pdata$region == input$year_region]))
    }
    
    print(paste("Found", length(countries_to_show), "countries for year analysis region:", input$year_region))
    
    updateSelectizeInput(session, "year_countries", 
                         choices = countries_to_show,
                         options = list(plugins = list('remove_button')))
  })
  
  observe({
    req(input$diag_region)
    countries_to_show <- if(input$diag_region == "All Regions") {
      sort(unique(pdata$country))
    } else {
      sort(unique(pdata$country[pdata$region == input$diag_region]))
    }
    
    print(paste("Found", length(countries_to_show), "countries for diagnostics region:", input$diag_region))
    
    updateSelectizeInput(session, "diag_countries", 
                         choices = countries_to_show,
                         options = list(plugins = list('remove_button')))
  })
  
  observe({
    req(input$eda_region_filter)
    region_filter <- input$eda_region_filter
    countries_to_show <- if(region_filter == "all") {
      sort(unique(pdata$country))
    } else {
      sort(unique(pdata$country[pdata$region == region_filter]))
    }
    
    print(paste("Found", length(countries_to_show), "countries for EDA region:", region_filter))
    
    updateSelectizeInput(session, "eda_countries", 
                         choices = countries_to_show,
                         options = list(plugins = list('remove_button')))
  })
  
  observe({
    req(input$bubble_region)
    region <- input$bubble_region
    countries_to_show <- if(region == "all") {
      sort(unique(pdata$country))
    } else {
      sort(unique(pdata$country[pdata$region == region]))
    }
    
    print(paste("Found", length(countries_to_show), "countries for bubble region:", region))
    
    updateSelectizeInput(session, "bubble_countries", 
                         choices = countries_to_show,
                         options = list(plugins = list('remove_button')))
  })
  
  observe({
    req(input$corr_region)
    region <- input$corr_region
    countries_to_show <- if(region == "all") {
      sort(unique(pdata$country))
    } else {
      sort(unique(pdata$country[pdata$region == region]))
    }
    
    print(paste("Found", length(countries_to_show), "countries for correlation region:", region))
    
    updateSelectizeInput(session, "corr_countries", 
                         choices = countries_to_show,
                         options = list(plugins = list('remove_button')))
    
    years <- sort(unique(pdata$year))
    updateSelectInput(session, "corr_year", 
                      choices = c("All Years" = "all_years", years), 
                      selected = max(years))
  })
  
  filter_data_by_region_country <- function(data, region_input, countries_input) {
    filtered_data <- data
    
    if(region_input != "All Regions" && region_input != "all") {
      filtered_data <- filtered_data %>% filter(region == region_input)
    }
    
    if(!is.null(countries_input) && length(countries_input) > 0) {
      filtered_data <- filtered_data %>% filter(country %in% countries_input)
    }
    
    return(filtered_data)
  }
  
  filtered_data <- reactive({
    data <- pdata
    
    data <- data %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    data <- filter_data_by_region_country(data, input$region, input$countries)
    
    print(paste("Filtered PLM data to", nrow(data), "rows with", length(unique(data$country)), "countries"))
    
    return(data)
  })
  
  comp_filtered_data <- reactive({

    data <- pdata
    
    data <- data %>% filter(year >= input$comp_year_range[1] & year <= input$comp_year_range[2])
    
    data <- filter_data_by_region_country(data, input$comp_region, input$comp_countries)

    print(paste("Filtered Comparison data to", nrow(data), "rows with", length(unique(data$country)), "countries"))
    
    return(data)
  })
  
  year_filtered_data <- reactive({
    data <- pdata
    
    print("Columns in original dataset:")
    print(names(data))
    
    data <- data %>% filter(year >= input$year_range_analysis[1] & year <= input$year_range_analysis[2])
    
    data <- filter_data_by_region_country(data, input$year_region, input$year_countries)
    
    print(paste("Filtered Year Analysis data to", nrow(data), "rows with", length(unique(data$country)), "countries"))
    
    if(input$year_region == "North America") {
      print("North America data:")
      print(paste("Total rows:", nrow(data)))
      print(paste("Countries:", paste(unique(data$country), collapse=", ")))
      print(paste("Years:", paste(unique(data$year), collapse=", ")))
      
      for(var in water_indicators) {
        missing_count <- sum(is.na(data[[var]]))
        total_count <- nrow(data)
        if(total_count > 0) {
          print(paste(var, "- missing values:", missing_count, "/", total_count, 
                      "(", round(missing_count/total_count*100, 1), "%)"))
        } else {
          print(paste(var, "- no data available"))
        }
      }
    }
    
    return(data)
  })
  
  diag_filtered_data <- reactive({
    data <- pdata
    
    data <- data %>% filter(year >= input$diag_year_range[1] & year <= input$diag_year_range[2])

    data <- filter_data_by_region_country(data, input$diag_region, input$diag_countries)

    print(paste("Filtered Diagnostics data to", nrow(data), "rows with", length(unique(data$country)), "countries"))
    
    return(data)
  })

  bubble_filtered_data <- reactive({
    data <- if (input$bubble_region == "all") pdata else pdata %>% filter(region == input$bubble_region)
    data <- data %>% filter(year >= input$bubble_year_range[1], year <= input$bubble_year_range[2])
    if (!is.null(input$bubble_countries) && length(input$bubble_countries) > 0) {
      data <- data %>% filter(country %in% input$bubble_countries)
    }
    data
  })
  
  # ================================================================
  # EDA
  # ================================================================
  
  eda_data <- reactive({
    req(input$eda_indicator, input$eda_year)
    
    filtered_data <- pdata %>%
      filter(year == input$eda_year)

    if (input$eda_group == "region" && !is.null(input$eda_regions) && length(input$eda_regions) > 0) {
      filtered_data <- filtered_data %>% filter(region %in% input$eda_regions)
    }

    if (input$eda_group == "country" && !is.null(input$eda_countries) && length(input$eda_countries) > 0) {
      filtered_data <- filtered_data %>% filter(country %in% input$eda_countries)
    }

    filtered_data %>%
      select(year, country, region, !!sym(input$eda_indicator)) %>%
      drop_na()
  })

  eda_time_data <- reactive({
    req(input$eda_indicator)
    
    filtered_data <- pdata
    

    if (input$eda_group == "region" && !is.null(input$eda_regions) && length(input$eda_regions) > 0) {
      filtered_data <- filtered_data %>% filter(region %in% input$eda_regions)
    } else if (input$eda_group == "country" && !is.null(input$eda_countries) && length(input$eda_countries) > 0) {
      filtered_data <- filtered_data %>% filter(country %in% input$eda_countries)
    } else if (input$eda_group == "region" && (is.null(input$eda_regions) || length(input$eda_regions) == 0)) {
      filtered_data <- filtered_data %>% filter(region == unique(pdata$region)[1])
    } else if (input$eda_group == "country" && (is.null(input$eda_countries) || length(input$eda_countries) == 0)) {
      filtered_data <- filtered_data %>% filter(region == unique(pdata$region)[1])
    }

    filtered_data %>%
      select(year, country, region, year_group, !!sym(input$eda_indicator)) %>%
      drop_na()
  })
  
  # Correctly modified eda_box_plot output to include country information in tooltips
  # Modified eda_box_plot output to include ONLY country information tooltips (no duplicates)
  output$eda_box_plot <- renderPlotly({
    df <- eda_data()

    if(nrow(df) == 0) {
      return(NULL)
    }
    
    # Get the indicator variable name
    indicator_var <- input$eda_indicator
    
    # For region grouping, identify outliers and handle tooltips
    if(input$eda_group == "region") {
      # Calculate outlier thresholds for each region group
      outlier_data <- df %>%
        group_by(region) %>%
        mutate(
          q1 = quantile(.data[[indicator_var]], 0.25),
          q3 = quantile(.data[[indicator_var]], 0.75),
          iqr = q3 - q1,
          lower_bound = q1 - 1.5 * iqr,
          upper_bound = q3 + 1.5 * iqr,
          is_outlier = .data[[indicator_var]] < lower_bound | .data[[indicator_var]] > upper_bound,
          # Add a tooltip column to ALL data points
          tooltip = ifelse(is_outlier, 
                           paste0("Country: ", country, "<br>Value: ", round(.data[[indicator_var]], 2)),
                           NA_character_)
        ) %>%
        ungroup()
      
      # First create a plot with boxplots only (no tooltips)
      p <- ggplot() +
        # Use geom_boxplot with data but without aesthetic mappings for tooltips
        geom_boxplot(data = outlier_data, 
                     aes(x = region, y = .data[[indicator_var]]),
                     fill = "skyblue", color = "black", outlier.shape = NA) +
        theme_minimal() +
        labs(x = NULL, y = names(indicator_choices)[indicator_choices == indicator_var]) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Add outlier points with tooltips
      outliers <- outlier_data %>% filter(is_outlier)
      if(nrow(outliers) > 0) {
        p <- p + geom_point(data = outliers, 
                            aes(x = region, y = .data[[indicator_var]], text = tooltip),
                            color = "black", size = 2)
      }
      
      ggplotly(p, tooltip = "text") %>%
        style(hoverinfo = "none", traces = 1)  # Trace 1 is the boxplot
      
    } else {
      
      df$tooltip <- paste0("Country: ", df$country, "<br>Value: ", round(df[[indicator_var]], 2))
      
      if(nrow(df) > 30) {
        p <- ggplot(df, aes(x = country, y = .data[[indicator_var]], text = tooltip)) +
          geom_point(color = "black", alpha = 0.7) +
          theme_minimal() +
          labs(x = NULL, y = names(indicator_choices)[indicator_choices == indicator_var]) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))
      } else {
        p <- ggplot(df, aes(x = country, y = .data[[indicator_var]])) +
          geom_boxplot(fill = "skyblue", color = "black", outlier.shape = NA) +
          theme_minimal() +
          labs(x = NULL, y = names(indicator_choices)[indicator_choices == indicator_var]) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_jitter(aes(text = tooltip), width = 0.2, alpha = 0.7, color = "black")
      }
      
      ggplotly(p, tooltip = "text") %>%
        style(hoverinfo = "none", traces = 1)  # Hide hover info for the boxplot trace
    }
  })
  
  output$eda_time_plot <- renderPlot({
    df <- eda_time_data()
    
    if(nrow(df) == 0) {
      return(NULL)
    }
    
    indicator_name <- names(indicator_choices)[indicator_choices == input$eda_indicator]
    
    if (input$eda_group == "region") {
      selected_region <- if(length(input$eda_regions) > 0) input$eda_regions[1] else unique(pdata$region)[1]
      
      plot_data <- df %>% 
        filter(region == selected_region) %>%
        select(year_group, !!sym(input$eda_indicator))
      
      p <- ggstatsplot::ggbetweenstats(
        data = plot_data,
        x = year_group,
        y = !!sym(input$eda_indicator),
        type = "parametric",
        pairwise.comparisons = TRUE,
        pairwise.display = "significant",
        mean.ci = TRUE,
        messages = FALSE,
        title = paste("Comparison of", indicator_name, "across Year Groups\nRegion:", selected_region),
        xlab = "Year Group",
        ylab = indicator_name,
        ggtheme = ggplot2::theme_minimal()
      )
      
      return(p)
    } else {
      selected_country <- if(length(input$eda_countries) > 0) input$eda_countries[1] else unique(df$country)[1]
      
      plot_data <- df %>% 
        filter(country == selected_country) %>%
        select(year_group, !!sym(input$eda_indicator))
      
      p <- ggstatsplot::ggbetweenstats(
        data = plot_data,
        x = year_group,
        y = !!sym(input$eda_indicator),
        type = "parametric",
        pairwise.comparisons = TRUE,
        pairwise.display = "significant",
        mean.ci = TRUE,
        messages = FALSE,
        title = paste("Comparison of", indicator_name, "across Year Groups\nCountry:", selected_country),
        xlab = "Year Group",
        ylab = indicator_name,
        ggtheme = ggplot2::theme_minimal()
      )
      
      return(p)
    }
  })
  
  output$eda_data_table <- DT::renderDataTable({
    df <- eda_data()
    DT::datatable(df, options = list(pageLength = 10))
  })
  
  output$eda_box_plot_title <- renderText({
    if(input$eda_plot_type == "box") {
      paste("Distribution of", names(indicator_choices)[indicator_choices == input$eda_indicator], 
            "by", ifelse(input$eda_group == "region", "Region", "Country"), 
            "(", input$eda_year, ")")
    } else {
      paste("Time Trend of", names(indicator_choices)[indicator_choices == input$eda_indicator], 
            "by", ifelse(input$eda_group == "region", "Region", "Country"))
    }
  })
  
  # ================================================================
  # PLM
  # ================================================================
  
  has_panel_data <- reactive({
    dt <- filtered_data()
    if(nrow(dt) == 0) return(FALSE)
    yrs <- length(unique(dt$year))
    ctry <- length(unique(dt$country))
    (yrs > 1 && ctry > 1)
  })
  
  has_comp_panel_data <- reactive({
    dt <- comp_filtered_data()
    if(nrow(dt) == 0) return(FALSE)
    yrs <- length(unique(dt$year))
    ctry <- length(unique(dt$country))
    (yrs > 1 && ctry > 1)
  })
  
  has_year_panel_data <- reactive({
    dt <- year_filtered_data()
    if(nrow(dt) == 0) return(FALSE)
    yrs <- length(unique(dt$year))
    ctry <- length(unique(dt$country))
    (yrs > 1 && ctry > 1)
  })
  
  has_diag_panel_data <- reactive({
    dt <- diag_filtered_data()
    if(nrow(dt) == 0) return(FALSE)
    yrs <- length(unique(dt$year))
    ctry <- length(unique(dt$country))
    (yrs > 1 && ctry > 1)
  })
  
  dependent_var <- reactive({
    input$disease
  })
  
  comp_dependent_var <- reactive({
    input$comp_disease
  })
  
  year_dependent_var <- reactive({
    input$year_disease
  })
  
  diag_dependent_var <- reactive({
    input$diag_disease
  })
  
  selected_vars <- reactive({
    if(is.null(input$variables) || length(input$variables)==0) {
      return(character(0))
    }
    input$variables
  })
  
  comp_selected_vars <- reactive({
    if(is.null(input$comp_variables) || length(input$comp_variables)==0) {
      return(character(0))
    }
    input$comp_variables
  })
  
  year_selected_vars <- reactive({
    if(is.null(input$year_variables) || length(input$year_variables)==0) {
      return(character(0))
    }
    input$year_variables
  })
  
  diag_selected_vars <- reactive({
    if(is.null(input$diag_variables) || length(input$diag_variables)==0) {
      return(character(0))
    }
    input$diag_variables
  })
  
  base_formula <- reactive({
    sv <- selected_vars()
    if(length(sv)==0) {
      paste(dependent_var(), "~ 1")
    } else {
      paste(dependent_var(), "~", paste(sv, collapse = " + "))
    }
  })
  
  comp_base_formula <- reactive({
    sv <- comp_selected_vars()
    if(length(sv)==0) {
      paste(comp_dependent_var(), "~ 1")
    } else {
      paste(comp_dependent_var(), "~", paste(sv, collapse = " + "))
    }
  })
  
  year_base_formula <- reactive({
    sv <- year_selected_vars()
    if(length(sv)==0) {
      paste(year_dependent_var(), "~ 1")
    } else {
      paste(year_dependent_var(), "~", paste(sv, collapse = " + "))
    }
  })
  
  diag_base_formula <- reactive({
    sv <- diag_selected_vars()
    if(length(sv)==0) {
      paste(diag_dependent_var(), "~ 1")
    } else {
      paste(diag_dependent_var(), "~", paste(sv, collapse = " + "))
    }
  })
  
  model_formula <- reactive({
    bf <- base_formula()
    if(input$model_type == "Fixed Effects with Time") {
      paste(bf, "+ factor(year)")
    } else if(input$model_type == "Dynamic Panel") {
      # 动态面板
      if(length(selected_vars())==0) {
        paste(dependent_var(), "~ lag(", dependent_var(), ")")
      } else {
        paste(dependent_var(), "~ lag(", dependent_var(), ") +",
              paste(selected_vars(), collapse=" + "))
      }
    } else {
      bf
    }
  })
  
  diag_model_formula <- reactive({
    bf <- diag_base_formula()
    if(input$diag_model_type == "Fixed Effects with Time") {
      paste(bf, "+ factor(year)")
    } else if(input$diag_model_type == "Dynamic Panel") {
      if(length(diag_selected_vars())==0) {
        paste(diag_dependent_var(), "~ lag(", diag_dependent_var(), ")")
      } else {
        paste(diag_dependent_var(), "~ lag(", diag_dependent_var(), ") +",
              paste(diag_selected_vars(), collapse=" + "))
      }
    } else {
      bf
    }
  })
  
  output$model_info <- renderUI({
    if(!has_panel_data() && input$model_type!="Pooled OLS") {
      tags$div(
        class="alert alert-warning",
        strong("Note: "),
        "Not enough panel structure. Using OLS instead of ", input$model_type
      )
    }
  })
  
  model_results <- reactive({
    data <- filtered_data()
    if(nrow(data)==0) return(NULL)
    
    is_panel <- has_panel_data()
    mt <- input$model_type
    
    out <- tryCatch({
      if(!is_panel && mt!="Pooled OLS") {
        lm(as.formula(base_formula()), data=data)
      } else {
        if(mt=="Pooled OLS") {
          plm(as.formula(model_formula()), data=data, model="pooling", index=c("country", "year"))
        } else if(mt=="Fixed Effects") {
          plm(as.formula(model_formula()), data=data, model="within", index=c("country", "year"))
        } else if(mt=="Random Effects") {
          plm(as.formula(model_formula()), data=data, model="random", index=c("country", "year"))
        } else if(mt=="Fixed Effects with Time") {
          plm(as.formula(model_formula()), data=data, model="within", index=c("country", "year"))
        } else if(mt=="Dynamic Panel") {
          plm(as.formula(model_formula()), data=data, model="within", index=c("country", "year"))
        }
      }
    }, error=function(e) {
      print(paste("Model Error:", e$message))
      NULL
    })
    out
  })
  
  diag_model_results <- reactive({
    data <- diag_filtered_data()
    if(nrow(data)==0) return(NULL)
    
    is_panel <- has_diag_panel_data()
    mt <- input$diag_model_type
    
    out <- tryCatch({
      if(!is_panel && mt!="Pooled OLS") {
        lm(as.formula(diag_base_formula()), data=data)
      } else {
        if(mt=="Pooled OLS") {
          plm(as.formula(diag_model_formula()), data=data, model="pooling", index=c("country", "year"))
        } else if(mt=="Fixed Effects") {
          plm(as.formula(diag_model_formula()), data=data, model="within", index=c("country", "year"))
        } else if(mt=="Random Effects") {
          plm(as.formula(diag_model_formula()), data=data, model="random", index=c("country", "year"))
        } else if(mt=="Fixed Effects with Time") {
          plm(as.formula(diag_model_formula()), data=data, model="within", index=c("country", "year"))
        } else if(mt=="Dynamic Panel") {
          plm(as.formula(diag_model_formula()), data=data, model="within", index=c("country", "year"))
        }
      }
    }, error=function(e) {
      print(paste("Diagnostics Model Error:", e$message))
      NULL
    })
    out
  })
  
  output$model_summary <- renderPrint({
    mod <- model_results()
    if(is.null(mod)) {
      return("No model fitted. Possibly no data or model fit failure.")
    }
    summary(mod)
  })
  
  model_statistics <- reactive({
    mod <- model_results()
    if(is.null(mod)) return(NULL)
    sm <- tryCatch(summary(mod), error=function(e)NULL)
    if(is.null(sm) || is.null(sm$r.squared)) return(NULL)
    if(inherits(mod,"plm")) {
      fstat <- "N/A"; pval <- "N/A"
      if(!is.null(sm$fstatistic) && !is.null(sm$fstatistic$statistic)) {
        all_fvals <- sm$fstatistic$statistic
        main_f    <- all_fvals[1]  
        df1 <- sm$fstatistic$parameter[1]
        df2 <- sm$fstatistic$parameter[2]
        
        fstat <- formatC(main_f, digits=2, format="f")
        pval  <- format.pval(pf(main_f, df1, df2, lower.tail=FALSE), digits=4)
      }
      data.frame(
        Statistic = c("R-squared","Adj. R-squared","F-statistic","p-value","Observations","Countries"),
        Value = c(
          formatC(sm$r.squared, digits=4, format="f"),
          formatC(sm$adj.r.squared, digits=4, format="f"),
          fstat,
          pval,
          nrow(mod$model),
          length(unique(filtered_data()$country))
        )
      )
    } else {
      fstat <- "N/A"; pval <- "N/A"
      if(!is.null(sm$fstatistic)) {
        main_f <- sm$fstatistic[1] 
        df1    <- sm$fstatistic[2]
        df2    <- sm$fstatistic[3]
        fstat <- formatC(main_f, digits=2, format="f")
        pval  <- format.pval(pf(main_f, df1, df2, lower.tail=FALSE), digits=4)
      }
      data.frame(
        Statistic = c("R-squared","Adj. R-squared","F-statistic","p-value","Observations","Countries"),
        Value = c(
          formatC(sm$r.squared, digits=4, format="f"),
          formatC(sm$adj.r.squared, digits=4, format="f"),
          fstat,
          pval,
          length(mod$residuals),
          length(unique(filtered_data()$country))
        )
      )
    }
  })
  
  output$model_stats <- renderTable({
    ms <- model_statistics()
    if(is.null(ms)) {
      data.frame(Message="No model statistics available (model fit may have failed).")
    } else {
      ms
    }
  })

  r_code <- reactive({
    is_panel <- has_panel_data()
    mt <- input$model_type
    if(!is_panel && mt!="Pooled OLS") {
      paste0(
        "# Not enough panel structure => using lm() instead of ", mt,"\n",
        "model <- lm(", base_formula(), ", data=your_data)\nsummary(model)"
      )
    } else {
      model_type_param <- switch(mt,
                                 "Pooled OLS"             = "pooling",
                                 "Fixed Effects"          = "within",
                                 "Random Effects"         = "random",
                                 "Fixed Effects with Time"= "within",
                                 "Dynamic Panel"          = "within")
      paste0(
        "library(plm)\n",
        "model <- plm(", model_formula(), ", data=your_data, model='", model_type_param, 
        "', index=c('country', 'year'))\n",
        "summary(model)"
      )
    }
  })
  
  output$r_code <- renderPrint({
    cat(r_code())
  })

  output$coef_plot <- renderPlot({
    mod <- model_results()
    if(is.null(mod)) return()
    coefs <- tryCatch(coef(mod), error=function(e)NULL)
    if(is.null(coefs) || length(coefs)==0) return()
    
    df <- data.frame(Variable=names(coefs), Coefficient=as.numeric(coefs))
    df <- df[!grepl("factor\\(year\\)", df$Variable), ]

    df$Variable <- ifelse(df$Variable %in% names(indicator_choices), 
                          names(indicator_choices)[match(df$Variable, indicator_choices)], 
                          df$Variable)
    
    ggplot(df, aes(x=reorder(Variable, Coefficient), y=Coefficient)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title=paste("Model Coefficients -", names(disease_indicators)[disease_indicators == input$disease]),
           subtitle=paste("Model Type:", input$model_type),
           x="", y="Coefficient")
  })
  
  compare_models <- reactive({
    req(input$compare_models)
    dat <- comp_filtered_data()
    if(nrow(dat)==0) return(NULL)
    isp <- has_comp_panel_data()
    
    bf <- comp_base_formula()
    out_list <- list()
    for(mt in input$compare_models) {
      mm <- tryCatch({
        if(!isp && mt!="Pooled OLS") {
          lm(as.formula(bf), data=dat)
        } else {
          curr <- bf
          if(mt=="Fixed Effects with Time") {
            curr <- paste(bf, "+ factor(year)")
          }
          if(mt=="Dynamic Panel") {
            if(length(comp_selected_vars())==0) {
              curr <- paste(comp_dependent_var(), "~ lag(", comp_dependent_var(),")")
            } else {
              curr <- paste(comp_dependent_var(), "~ lag(", comp_dependent_var(), ") +",
                            paste(comp_selected_vars(), collapse=" + "))
            }
          }
          plm(as.formula(curr), data=dat, 
              model=switch(mt,
                           "Pooled OLS"             = "pooling",
                           "Fixed Effects"          = "within",
                           "Random Effects"         = "random",
                           "Fixed Effects with Time"= "within",
                           "Dynamic Panel"          = "within"),
              index=c("country", "year"))
        }
      }, error=function(e) {
        cat(" -> Error fitting model:", mt, ":", e$message,"\n")
        NULL
      })
      out_list[[mt]] <- mm
    }
    out_list
  })
  
  model_comparison_stats <- reactive({
    mods <- compare_models()
    if(is.null(mods) || length(mods) == 0) {
      return(NULL)
    }
    
    result_list <- list()

    for(model_name in names(mods)) {
      model <- mods[[model_name]]
      if(is.null(model)) next
      sm <- tryCatch(summary(model), error=function(e) NULL)
      if(is.null(sm) || is.null(sm$r.squared)) next

      result_list[[model_name]] <- c(
        "Model Type" = ifelse(inherits(model, "plm"), "Panel", "Cross-sectional"),
        "R-squared" = formatC(sm$r.squared, digits=4, format="f"),
        "Adj. R-squared" = formatC(sm$adj.r.squared, digits=4, format="f"),
        "Observations" = as.character(ifelse(inherits(model, "plm"), 
                                             nrow(model$model), 
                                             length(model$residuals)))
      )
    }
    
    if(length(result_list) == 0) {
      return(NULL)
    }
    
    result_df <- as.data.frame(do.call(rbind, result_list))

    rownames(result_df) <- names(result_list)
    
    return(result_df)
  })
  
  output$model_comparison_stats <- DT::renderDataTable({
    stats <- model_comparison_stats()
    
    if(is.null(stats)) {
      return(datatable(data.frame(
        Message = "No model statistics available"
      )))
    }
    
    stats_with_names <- cbind(
      Model = rownames(stats),
      stats
    )
    
    rownames(stats_with_names) <- NULL
    
    # 返回表格
    return(datatable(stats_with_names, options = list(pageLength = 10)))
  })

  output$model_comparison_plot <- renderPlot({
    mods <- compare_models()
    if(is.null(mods) || length(mods)==0) {
      return()
    }

    cdf <- data.frame(Model=character(), Variable=character(), Coefficient=numeric(), stringsAsFactors=FALSE)
    
    for(mn in names(mods)) {
      m <- mods[[mn]]
      if(is.null(m)) {
        next
      }

      cc <- tryCatch({
        coef(m)
      }, error=function(e){
        NULL
      })
      
      if(is.null(cc) || length(cc)==0) {
        next
      }

      cc <- cc[!grepl("factor\\(year\\)", names(cc))]

      for(vn in names(cc)) {
        if(vn=="(Intercept)" && mn %in% c("Fixed Effects","Fixed Effects with Time", "Dynamic Panel")) {
          next
        }

        pretty_name <- ifelse(vn %in% names(indicator_choices), 
                              names(indicator_choices)[match(vn, indicator_choices)], 
                              vn)

        cdf <- rbind(cdf, data.frame(
          Model=mn, 
          Variable=pretty_name, 
          Coefficient=as.numeric(cc[vn]), 
          stringsAsFactors=FALSE
        ))
      }
    }
    
    if(nrow(cdf)==0) {
      return()
    }

    ggplot(cdf, aes(x=Variable, y=Coefficient, fill=Model)) +
      geom_col(position="dodge") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      labs(title=paste("Model Comparison -", names(disease_indicators)[disease_indicators == input$comp_disease]),
           x="", y="Coefficient")
  })
  
  yearly_models <- reactive({
    dt <- year_filtered_data()
    
    yrmin <- input$year_range_analysis[1]
    yrmax <- input$year_range_analysis[2]
    yrs   <- yrmin:yrmax
    
    out <- list()
    models_fitted <- 0
    
    for(y in yrs) {
      year_data <- dt[dt$year==y,]

      if(nrow(year_data) < 10) {  
        print(paste("Skipping year", y, "due to insufficient data (", nrow(year_data), "rows)"))
        next
      }
    
      selVars <- year_selected_vars()
      var_na_counts <- sapply(selVars, function(v) sum(is.na(year_data[[v]])))
      var_available_counts <- nrow(year_data) - var_na_counts

      print(paste("Year", y, "- rows:", nrow(year_data)))
      for(v in selVars) {
        print(paste(" -", v, ":", var_available_counts[v], "non-NA values"))
      }

      min_required <- 5  
      if(any(var_available_counts < min_required)) {
        print(paste("Skipping year", y, "due to insufficient non-NA values in variables"))
        next
      }
      
      mm <- tryCatch({
        model <- lm(as.formula(year_base_formula()), data=year_data)
        
        coefs <- tryCatch(coef(model), error=function(e) NULL)
        if(is.null(coefs) || length(coefs) <= 1) {
          print(paste("Skipping year", y, "- model did not estimate coefficients"))
          NULL
        } else {
          models_fitted <- models_fitted + 1
          model
        }
      }, error=function(e) {
        print(paste("Error fitting model for year", y, ":", e$message))
        NULL
      })
      
      if(!is.null(mm)) {
        out[[as.character(y)]] <- mm
      }
    }
    
    print(paste("Total models fitted:", models_fitted, "out of", length(yrs), "years"))
    
    if(models_fitted == 0 && length(yrs) > 0) {
      print("WARNING: No models could be fitted. Check data availability for the selected region and variables.")
    }
    
    return(out)
  })
  
  yearly_stats <- reactive({
    mods <- yearly_models()
    if(length(mods)==0) return(NULL)

    selVars <- year_selected_vars()
    if(length(selVars)==0) return(NULL)

    base_cols <- c("Year","R_squared","Adj_R_squared","Countries","Observations")
    all_cols  <- c(base_cols, selVars)
    stats_df  <- setNames(data.frame(matrix(nrow=0,ncol=length(all_cols))), all_cols)

    for(yr in names(mods)) {
      m <- mods[[yr]]
      if(is.null(m)) next
      sm <- tryCatch(summary(m), error=function(e)NULL)
      if(is.null(sm)) next
      coefs <- tryCatch(coef(m), error=function(e)NULL)
      if(is.null(coefs)) next

      data_filter <- year_filtered_data()
      ydata <- data_filter[data_filter$year==as.numeric(yr),]

      newrow <- as.list(rep(NA, length(all_cols)))
      names(newrow) <- all_cols

      newrow$Year = as.integer(yr)
      newrow$R_squared = if(!is.null(sm$r.squared)){
        formatC(sm$r.squared, digits=4, format="f")
      } else "NA"
      newrow$Adj_R_squared = if(!is.null(sm$adj.r.squared)){
        formatC(sm$adj.r.squared, digits=4, format="f")
      } else "NA"
      newrow$Countries    = length(unique(ydata$country))
      newrow$Observations = length(m$residuals)

      for(sv in selVars) {
        print(paste("Processing variable:", sv))
        print(paste("Available coefficients:", paste(names(coefs), collapse=", ")))
        
        if(sv %in% names(coefs)) {
          newrow[[sv]] <- coefs[sv]
        } else {
          newrow[[sv]] <- NA
        }
      }

      stats_df <- rbind(stats_df, data.frame(newrow, stringsAsFactors=FALSE))
    }

    if(nrow(stats_df)==0) return(NULL)

    stats_df[order(stats_df$Year), ]
  })
  
  output$yearly_stats_table <- DT::renderDataTable({
    st <- yearly_stats()
    if(is.null(st)) {
      data.frame(Message="No yearly statistics available (all yearly fits failed or no data).")
    } else {
      selVars <- year_selected_vars()
      for(sv in selVars) {
        if(sv %in% names(st)) {
          numeric_vals <- suppressWarnings(as.numeric(st[[sv]]))
          if(!all(is.na(numeric_vals))) {
            st[[sv]] <- formatC(numeric_vals, digits=4, format="f")
          }
        }
      }
      datatable(st, options=list(pageLength=16))
    }
  })

  output$yearly_coef_plot <- renderPlot({
    st <- yearly_stats()
    if(is.null(st) || nrow(st)<=1) {
      empty_plot <- ggplot() + 
        theme_minimal() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data available for the selected variables \nin this region and time period", 
                 size = 6, hjust = 0.5) +
        labs(title = "Data Not Available",
             subtitle = paste("Region:", input$year_region)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
        ) +
        xlim(0, 1) + ylim(0, 1)
      
      return(empty_plot)
    }

    selVars <- year_selected_vars()

    exist_cols <- intersect(names(st), selVars)
    if(length(exist_cols)<1) {
      empty_plot <- ggplot() + 
        theme_minimal() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected variables", 
                 size = 6, hjust = 0.5) +
        labs(title = "Data Not Available",
             subtitle = paste("Region:", input$year_region)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
        ) +
        xlim(0, 1) + ylim(0, 1)
      
      return(empty_plot)
    }

    plot_data <- st %>%
      pivot_longer(cols=all_of(exist_cols),
                   names_to="Variable", values_to="Coefficient") %>%
      mutate(Coefficient = as.numeric(Coefficient))

    print("Variables in yearly plot:")
    print(selVars)
    print("Variables found in data:")
    print(exist_cols)
    print("Number of rows in plot data:")
    print(nrow(plot_data))
    print("Sample of plot data:")
    print(head(plot_data))
    
    var_labels <- setNames(
      names(water_indicators)[match(exist_cols, water_indicators)],
      exist_cols
    )

    plot_data$Variable <- factor(plot_data$Variable, levels=exist_cols)
  
    plot_data_clean <- plot_data %>% filter(!is.na(Coefficient))
    
    if(nrow(plot_data_clean) == 0) {
      empty_plot <- ggplot() + 
        theme_minimal() +
        annotate("text", x = 0.5, y = 0.5, label = "No valid coefficient data available", 
                 size = 6, hjust = 0.5) +
        labs(title = "Data Not Available",
             subtitle = paste("Region:", input$year_region)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
        ) +
        xlim(0, 1) + ylim(0, 1)
      
      return(empty_plot)
    }
    
    n_colors <- length(exist_cols)
    color_palette <- if(n_colors <= 8) {
      "Set2" 
    } else {
      "Spectral" 
    }
  
    p <- ggplot(plot_data_clean, aes(x=Year, y=Coefficient, color=Variable, group=Variable)) +
      geom_line(size=1) +
      geom_point(size=3) +
      scale_color_brewer(palette=color_palette) + 
      theme_minimal() +
      labs(title="Yearly Coefficient Changes",
           subtitle=paste("Dependent variable:", names(disease_indicators)[disease_indicators == input$year_disease]),
           x="Year", y="Coefficient") +
      theme(legend.position="bottom",
            legend.title=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1))
    
    print(p)
    return(p)
  })

  output$data_availability <- renderUI({
    if(input$year_region != "North America") return(NULL)

    dt <- year_filtered_data()
    selVars <- c(input$year_disease, year_selected_vars())

    years <- sort(unique(dt$year))
    avail_info <- lapply(selVars, function(v) {
      year_avail <- sapply(years, function(y) {
        year_data <- dt[dt$year == y, ]
        non_na_count <- sum(!is.na(year_data[[v]]))
        total_count <- nrow(year_data)
        paste0(non_na_count, "/", total_count)
      })
      data.frame(Variable = rep(v, length(years)), 
                 Year = years, 
                 Availability = year_avail,
                 stringsAsFactors = FALSE)
    })
    
    avail_df <- do.call(rbind, avail_info)

    var_display_names <- sapply(avail_df$Variable, function(v) {
      if(v %in% water_indicators) {
        names(water_indicators)[water_indicators == v]
      } else if(v %in% disease_indicators) {
        names(disease_indicators)[disease_indicators == v]
      } else {
        v
      }
    })
    avail_df$Variable <- var_display_names

    tagList(
      h4("Data Availability for North America"),
      p("Number of non-missing values / total observations per year"),
      renderTable(avail_df)
    )
  })

  output$residual_plot <- renderPlot({
    md <- diag_model_results()
    if(is.null(md)) return()
    rr <- tryCatch(residuals(md), error=function(e)NULL)
    ff <- tryCatch(fitted(md), error=function(e)NULL)
    if(is.null(rr)||is.null(ff)) return()
    dfr <- data.frame(Fitted=ff, Residuals=rr)
    ggplot(dfr, aes(x=Fitted, y=Residuals)) +
      geom_point(alpha=0.5) +
      geom_hline(yintercept=0, linetype="dashed") +
      geom_smooth(method="loess", se=FALSE) +
      theme_minimal() +
      labs(title="Residual Plot", x="Fitted", y="Residuals")
  })
  
  output$qq_plot <- renderPlot({
    md <- diag_model_results()
    if(is.null(md)) return()
    rr <- tryCatch(residuals(md), error=function(e)NULL)
    if(is.null(rr)) return()
    qqd <- data.frame(
      Theoretical = qqnorm(rr, plot.it=FALSE)$x,
      Sample      = qqnorm(rr, plot.it=FALSE)$y
    )
    ggplot(qqd, aes(x=Theoretical, y=Sample)) +
      geom_point(alpha=0.5) +
      geom_abline(intercept=0, slope=1, linetype="dashed") +
      theme_minimal() +
      labs(title="Normal Q-Q Plot", x="Theoretical", y="Sample")
  })
  
  output$hausman_test <- renderPrint({
    if(!has_diag_panel_data()) {
      return("Hausman test requires panel data with multiple time periods and countries.")
    }
    dt <- diag_filtered_data()
    if(nrow(dt)==0) return("No data after filtering.")
    bfm <- diag_base_formula()
    fe <- tryCatch(plm(as.formula(bfm), data=dt, model="within", index=c("country", "year")), error=function(e)NULL)
    re <- tryCatch(plm(as.formula(bfm), data=dt, model="random", index=c("country", "year")), error=function(e)NULL)
    if(is.null(fe) || is.null(re)) {
      return("Unable to fit both FE and RE.")
    }
    tryCatch({
      phtest(fe, re)
    }, error=function(e){
      paste("Error in phtest:", e$message)
    })
  })
  
  output$serial_test <- renderPrint({
    if(!has_diag_panel_data()) {
      return("Serial correlation test requires panel data with multiple time periods & countries.")
    }
    md <- diag_model_results()
    if(is.null(md)) return("No model for testing.")
    if(!inherits(md, "plm")) return("Serial test requires a plm model.")
    tryCatch({
      pbgtest(md)
    }, error=function(e){
      paste("Error in pbgtest:", e$message)
    })
  })
  
  output$model_type_box <- renderInfoBox({
    eff_type <- if(has_panel_data() || input$model_type=="Pooled OLS") {
      input$model_type
    } else {
      "OLS (Cross-sectional)"
    }
    infoBox("Model Type", eff_type, icon=icon("calculator"), color="blue")
  })
  
  output$observations_box <- renderInfoBox({
    cnt <- nrow(filtered_data())
    infoBox("Observations", cnt, icon=icon("table"), color="green")
  })
  
  output$countries_box <- renderInfoBox({
    dat <- filtered_data()
    cc <- if(nrow(dat)==0) 0 else length(unique(dat$country))
    infoBox("Countries", cc, icon=icon("flag"), color="red")
  })
  
  # ================================================================
  # CDA
  # ================================================================

  output$bubblePlot <- renderPlotly({
    req(bubble_filtered_data())
    df <- bubble_filtered_data()
    x <- input$xvar
    y <- input$yvar
    color <- input$colorvar
    
    df <- df %>% filter(complete.cases(.data[[x]], .data[[y]], population))
    if (color == "none") {
      df$color_var <- "lightblue"
      p <- ggplot(df, aes_string(x = x, y = y, size = "population", text = "country", frame = "year")) +
        geom_point(color = "lightblue", alpha = 0.7)
    } else {
      p <- ggplot(df, aes_string(x = x, y = y, size = "population", color = color, text = "country", frame = "year")) +
        geom_point(alpha = 0.7)
    }
    
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red", 
                           aes(frame = year, group = 1), size = 1)
    }
    
    p <- p + labs(title = paste("Trend:", names(indicator_choices)[indicator_choices == x], "vs", names(indicator_choices)[indicator_choices == y])) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y", "size")) %>%
      animation_opts(frame = 1000 / input$animation_speed, redraw = TRUE)
  })
  
  output$correlation_box <- renderValueBox({
    df <- bubble_filtered_data()
    valid <- df[complete.cases(df[[input$xvar]], df[[input$yvar]]), ]
    if (nrow(valid) < 2) return(valueBox("NA", "Correlation Coefficient", icon = icon("times"), color = "red"))
    corr <- cor(valid[[input$xvar]], valid[[input$yvar]])
    valueBox(round(corr, 2), "Correlation Coefficient", 
             icon = icon("chart-line"), 
             color = ifelse(corr > 0, "blue", "red"))
  })
  
  output$data_count_box <- renderValueBox({
    count <- nrow(bubble_filtered_data())
    valueBox(count, "Total Data Points", 
             icon = icon("database"), 
             color = "blue")
  })
  
  output$plot_interpretation <- renderUI({
    df <- bubble_filtered_data()
    valid <- df[complete.cases(df[[input$xvar]], df[[input$yvar]]), ]
    if (nrow(valid) < 5) return(HTML("<p>Not enough data points for meaningful interpretation.</p>"))
    
    x_name <- names(indicator_choices)[indicator_choices == input$xvar]
    y_name <- names(indicator_choices)[indicator_choices == input$yvar]
    
    corr <- cor(valid[[input$xvar]], valid[[input$yvar]])
    corr_strength <- case_when(
      abs(corr) > 0.7 ~ "strong",
      abs(corr) > 0.3 ~ "moderate",
      TRUE ~ "weak"
    )
    
    direction <- if(corr > 0) "positive" else "negative"
    
    HTML(paste0(
      "<div style='padding:10px; background:#f9f9f9; border-left:4px solid #3c8dbc;'>",
      "<h4>Interpretation:</h4>",
      "<li>There is a ", corr_strength, " ", direction, " correlation (r = ", round(corr, 2), ") ",
      "between ", x_name, " and ", y_name, ".</li>",
      if(direction == "positive") {
        paste0("<li>This suggests that as ", x_name, " increases, ", y_name, " tends to increase as well.</li>")
      } else {
        paste0("<li>This suggests that as ", x_name, " increases, ", y_name, " tends to decrease.</li>")
      },
      "<li><em>Note: Correlation does not imply causation. Other variables may influence this relationship.</em></li>",
      "</div>"
    ))
  })
  
  # 相关性分析
  corr_data <- eventReactive(input$update_corr, {
    data <- pdata
    if (input$corr_region != "all") data <- data %>% filter(region == input$corr_region)
    if (length(input$corr_countries) > 0) data <- data %>% filter(country %in% input$corr_countries)
    if (input$corr_year != "all_years") data <- data %>% filter(year == as.numeric(input$corr_year))
    
    if (input$research_direction == "water_to_disease") {
      disease_var <- input$corr_disease
      water_vars <- input$corr_water_indicators
      
      corrs <- sapply(water_vars, function(wvar) {
        d <- data[complete.cases(data[[disease_var]], data[[wvar]]), ]
        if (nrow(d) >= 2) cor(d[[disease_var]], d[[wvar]]) else NA
      })
      
      result <- data.frame(
        Indicator = names(water_indicators)[match(water_vars, water_indicators)],
        Correlation = corrs,
        Variable = water_vars
      )
      
      attr(result, "direction") <- "Water Impact on Disease"
      attr(result, "target") <- names(disease_indicators)[disease_indicators == disease_var]
      
    } else {
      water_var <- input$corr_water
      disease_vars <- input$corr_disease_indicators
      
      corrs <- sapply(disease_vars, function(dvar) {
        d <- data[complete.cases(data[[dvar]], data[[water_var]]), ]
        if (nrow(d) >= 2) cor(d[[dvar]], d[[water_var]]) else NA
      })
      
      result <- data.frame(
        Indicator = names(disease_indicators)[match(disease_vars, disease_indicators)],
        Correlation = corrs,
        Variable = disease_vars
      )
      
      attr(result, "direction") <- "Disease Impact on Water"
      attr(result, "target") <- names(water_indicators)[water_indicators == water_var]
    }
    
    return(result)
  })
  
  output$corrPlot <- renderPlotly({
    df <- corr_data()
    req(nrow(df) > 0)
    
    df <- df %>% mutate(color = ifelse(Correlation >= 0, "darkblue", "darkred"))
    
    direction <- attr(df, "direction")
    target <- attr(df, "target")
    
    x_axis_label <- ifelse(input$research_direction == "water_to_disease", 
                           "Water Indicator", 
                           "Disease Indicator")
    
    p <- ggplot(df, aes(x = reorder(Indicator, Correlation), y = Correlation, fill = color)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Correlation with", target), 
           x = x_axis_label, 
           y = "Correlation Coefficient") +
      scale_fill_identity() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$corr_interpretation <- renderUI({
    df <- corr_data()
    if (all(is.na(df$Correlation))) return(HTML("<p>No valid correlations found.</p>"))
    
    direction <- attr(df, "direction")
    target <- attr(df, "target")
    
    pos <- df[which.max(df$Correlation), ]
    neg <- df[which.min(df$Correlation), ]
    
    HTML(paste0(
      "<div style='padding:10px; background:#f9f9f9; border-left:4px solid #3c8dbc;'>",
      "<h4>Correlation Summary for ", target, "</h4>",
      "<ul>",
      "<li><strong>Strongest Positive Correlation:</strong> ", pos$Indicator, " (", round(pos$Correlation, 2), ")</li>",
      "<li><strong>Strongest Negative Correlation:</strong> ", neg$Indicator, " (", round(neg$Correlation, 2), ")</li>",
      "</ul>",
      "<p>Note: Correlation does not imply causation.</p>",
      "</div>"
    ))
  })
  
  output$min_corr_box <- renderValueBox({
    df <- corr_data()
    min_val <- df[which.min(df$Correlation), ]
    target <- attr(df, "target")
    
    valueBox(paste0(min_val$Indicator, ": ", round(min_val$Correlation, 2)), 
             paste("Min Correlation with", target), 
             icon = icon("arrow-down"), 
             color = "red")
  })
  
  output$max_corr_box <- renderValueBox({
    df <- corr_data()
    max_val <- df[which.max(df$Correlation), ]
    target <- attr(df, "target")
    
    valueBox(paste0(max_val$Indicator, ": ", round(max_val$Correlation, 2)), 
             paste("Max Correlation with", target), 
             icon = icon("arrow-up"), 
             color = "blue")
  })
  
  observe({
    print("Selected variables in year_variables:")
    print(input$year_variables)
    
    print("year_selected_vars() output:")
    print(year_selected_vars())

    if("Population_density" %in% names(pdata)) {
      print("Population_density column exists in pdata")
    } else {
      print("Population_density column DOES NOT exist in pdata")
      print("Available columns in pdata:")
      print(names(pdata))
    }
  })
}

shinyApp(ui, server)
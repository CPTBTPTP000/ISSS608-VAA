# R Formula

# library(shiny)

# ui <- fluidPage()
   
# server <- function(input, output) {}

# shinyApp(ui = ui, server = server)

#can either use library or pacman::p_load[prefer]

# pacman::p_load(shiny,tidyverse)

library(shiny)
library(tidyverse)

exam <- read_csv("Exam_data.csv")

ui <- fluidPage(
  titlePanel("Pupils Exam Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Subject: ",
        choices = c(
          "English" = "ENGLISH",
          "Maths" = "MATHS",
          "Science" = "SCIENCE"),
        selected =  "ENGLISH"),
      
      
      
      sliderInput(inputId = "bin",
      label =  "Number of Bins",
       min = 5,
       max = 20,
      value = 10)
     
    ),
     
    mainPanel(
      plotOutput("distPlot")
    )
    
  )
 )

server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(exam,
           aes_string(x= input$variable)) +
      geom_histogram(bins = input$bin,
                     color = "black",
                     fill = "light blue")
    
  })
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  

shinyApp(ui = ui, server = server)

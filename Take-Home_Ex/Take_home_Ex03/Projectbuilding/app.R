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

# 在 runApp() 之前添加
if(exists("pdata")) {
  print("pdata exists")
  print(dim(pdata))
  print(head(pdata))
} else {
  print("pdata does not exist!")
}

library(shiny)
ui <- fluidPage("Hello World")
server <- function(input, output) {}
shinyApp(ui, server)
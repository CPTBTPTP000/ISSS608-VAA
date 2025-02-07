#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    titlePanel("Interactive Table"),
    actionButton("show_table", "Show Table"),
    DTOutput("data_table")
  ),
  server = function(input, output, session) {
    observeEvent(input$show_table, {
      output$data_table <- renderDT({
        datatable(
          mtcars,         # Use the `mtcars` dataset as an example
          selection = "single",   # Allow single-row selection
          options = list(pageLength = 5)  # Display 5 rows per page
        )
      })
    })
  }
)
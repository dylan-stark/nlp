#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(tidytext)
library(stringr)

recommend <- function(user_str) {
  user_words <- user_str %>%
    str_trim() %>%
    str_split("\\s+") %>%
    unlist()

  data.frame(input = user_words)
}

################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("YM-1"),
  verticalLayout(
    tableOutput("recs"),
    textInput("user_input", "User input:")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$recs <- renderTable({
    recommend(input$user_input)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


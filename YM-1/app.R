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
library(pryr)

# TODO: figure out if there's a better pla
ym_1b <- read_rds("../vignettes/ym_1b.rds")
mem_usage_msg <- paste0("Memory est.: ", prettyNum(object_size(ym_1b), big.mark = ","), " B")

recommend <- function(user_str) {
  user_words <- user_str %>%
    str_extract_all("\\w+") %>%
    unlist()

  if (length(user_words) == 0) {
    data.frame(input = "")
  } else {
    last_word <- user_words[[length(user_words)]]
    ym_1b %>%
      filter(item1 == last_word)
  }
}

################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("YM-1"),
  verticalLayout(
    tableOutput("recs"),
    textInput("user_input", "User input:"),
    textOutput("usage_stats")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  rv <- reactiveValues()
  rv$running_time_msg <- "(not run)"

  output$recs <- renderTable({
    start <- proc.time()
    recs <- recommend(input$user_input)
    total <- proc.time() - start
    user_time <- total[[1]] + total[[4]]
    rv$running_time_msg <- paste0("Time est.: ", format(user_time, digits = 3), " sec")

    recs
  })

  output$usage_stats <- renderText({
    paste0(mem_usage_msg, rv$running_time_msg, collapse = "; ")
  })
}

# Run the application
shinyApp(ui = ui, server = server)


################################################################################
# YM-1D

library(shiny)

library(tidyverse)
library(tidytext)
library(wordcloud)
library(stringr)
library(pryr)

library(courseraswiftkey)

################################################################################

recommend_bigram <- function(model, user_str, n = 1) {
  user_words <- user_str %>%
    str_extract_all("\\w+") %>%
    unlist()

  word <- ifelse(length(user_words) == 0, "",
                 user_words[[length(user_words)]])

  exact_match <- model %>%
    filter(w1 == word & w2 != "_s_unk") %>%
    arrange(desc(prob))
  if (nrow(exact_match) > 0) {
    return(head(exact_match, n))
  }

  unk_match <- model %>%
    filter(w1 == '_s_unk' & w2 != "_s_unk") %>%
    arrange(desc(prob))
  return(head(unk_match, n))
}

################################################################################
# TODO: figure out if there's a better pla
ym_1 <- read_rds("../vignettes/ym_1b_news.rds")
mem_usage_msg <- paste0("Memory est.: ", prettyNum(object_size(ym_1), big.mark = ","), " B")

recommend <- recommend_bigram

# TODO: find a better place for this
this_str <- ""
last_str <- ""
last_output <- ""

################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("YM-1"),
  verticalLayout(
    #tableOutput("rec_table"),
    plotOutput("recs"),
    textInput("user_input", "User input:"),
    textOutput("usage_stats")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  rv <- reactiveValues()
  rv$running_time_msg <- "(not run)"
  rv$update_cloud <- TRUE
  rv$curr_str <- ""

  get_recs <- function(model, input, n = 1) {
    recs <- recommend(model, input, n = n)

    recs %>%
      mutate(prob_scale = scale(prob, center = FALSE),
             prob_min = min(prob_scale),
             prob_adj = 1 + abs(prob_min) + prob_scale,
             prob_norm = (prob - min(prob)) / (max(prob) - min(prob)),
             rank_num = n() - row_number(),
             rank_adj = 1 + ifelse(rank_num >= n() - 3,  rank_num * 1.25, rank_num),
             rank_scale = scale(rank_adj),
             rank = prob_norm,
             rank = ifelse(is.na(rank), 1, rank))
  }

  output$rec_table <- renderTable({
    start <- proc.time()
    recs <- get_recs(ym_1, input$user_input, n = 20)
    total <- proc.time() - start
    user_time <- total[[1]] + total[[4]]
    rv$running_time_msg <- paste0("Time est.: ", format(user_time, digits = 3), " sec")

    recs
  })

  user_str <- observeEvent(input$user_input, {
    user_str <- input$user_input

    last_str <<- this_str
    this_str <<- str_trim(user_str)
  
    print(paste0("last_str: ", last_str))
    print(paste0("this_str: ", this_str))
    
    if (this_str != last_str) {
      rv$curr_str <- this_str
    }
  })
  
  output$recs <- renderPlot({
    start <- proc.time()
    recs <- get_recs(ym_1, rv$curr_str, n = 20)
    total <- proc.time() - start
    user_time <- total[[1]] + total[[4]]
    rv$running_time_msg <- paste0("Time est.: ", format(user_time, digits = 3), " sec")

    last_output <- recs %>%
      with(wordcloud(w2, rank, scale = c(4,1), random.order = FALSE, rot.per = 0.10, colors=brewer.pal(8, "Dark2")))
    
    last_output
  })

  output$usage_stats <- renderText({
    paste0(mem_usage_msg, "; ", rv$running_time_msg)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


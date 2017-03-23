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
  user_bi <- word(user_str, -1)
  user_prefix <- word(user_str, -2, -1)

  if (is.na(user_bi) | user_bi == "") {
    user_bi <- "_s_unk"
  }
  if (is.na(user_prefix)) {
    user_prefix <- paste("_s_unk", user_bi)
  }

  unk_prefix <- paste("_s_unk", user_bi)

  model %>%
    filter((prefix == user_prefix | prefix == unk_prefix | prefix == "_s_unk _s_unk") & (word != "_s_unk")) %>%
    arrange(desc(prefix), desc(p_kn_tri)) %>%
    select(word, p_kn_tri) %>%
    rename(prob = p_kn_tri) %>%
    #arrange(desc(p_kn_tri), desc(p_kn_bi), desc(p_kn_uni)) %>%
    head(n)

  #exact_match <- model %>%
  #  filter(w1 == word & w2 != "_s_unk") %>%
  #  arrange(desc(prob))
  #if (nrow(exact_match) > 0) {
  #  return(head(exact_match, n))
  #}
#
  #unk_match <- model %>%
  #  filter(w1 == '_s_unk' & w2 != "_s_unk") %>%
  #  arrange(desc(prob))
  #return(head(unk_match, n))
}

################################################################################
# TODO: figure out if there's a better pla
#ym_1 <- read_rds("../vignettes/ym_1b_news.rds")
ym_1 <- read_rds("../vignettes/p_kn_tri.rds")
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
    textInput("user_input", "Just keep typing!"),
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

    #recs %>%
    #  mutate(prob_scale = scale(prob, center = FALSE),
    #         prob_min = min(prob_scale),
    #         prob_adj = 1 + abs(prob_min) + prob_scale,
    #         prob_norm = (prob - min(prob)) / (max(prob) - min(prob)),
    #         rank_num = n() - row_number(),
    #         rank_adj = 1 + ifelse(rank_num >= n() - 3,  rank_num * 1.25, rank_num),
    #         rank_scale = scale(rank_adj),
    #         rank = prob_norm,
    #         rank = ifelse(is.na(rank), 1, rank))

    recs %>%
      mutate(rank = prob)
      #mutate(rank = row_number()) # OK, but too big!
      #mutate(rank = row_number() / n()) # OK, but too big!
      #mutate(rank = row_number() / n()^10) # OK, but too big!
      #mutate(rank = row_number()^(-2/3)) # semi-jibberish
      #mutate(rank = row_number() / n() * prob) # jibberish
      #mutate(rank = n() - row_number()) # jibberish
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

    #print(paste0("last_str: ", last_str))
    #print(paste0("this_str: ", this_str))

    if (this_str != last_str) {
      rv$curr_str <- this_str
    }
  })

  output$recs <- renderPlot({
    start <- proc.time()
    recs <- get_recs(ym_1, rv$curr_str, n = 100)
    total <- proc.time() - start
    user_time <- total[[1]] + total[[4]]
    rv$running_time_msg <- paste0("Time est.: ", format(user_time, digits = 3), " sec")

    last_output <- recs %>%
      with(wordcloud(word, rank, scale = c(4,1), random.order = FALSE, rot.per = 0.10, colors=brewer.pal(8, "Dark2")))

    last_output
  })

  output$usage_stats <- renderText({
    paste0(mem_usage_msg, "; ", rv$running_time_msg)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


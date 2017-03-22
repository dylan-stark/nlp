################################################################################

add_unknowns <- function(data, symbol = "_s_unk") {
  data %>%
    unnest_tokens(word, text) %>%
    group_by(word) %>%
    mutate(word2 = lag(word, n = 1, default = symbol)) %>%
    group_by(line) %>%
    summarize(text = paste(word2, collapse = " "))
}

################################################################################
# Unigram model functions

count_words <- function(data) {
  data %>%
    add_unknowns() %>%
    unnest_tokens(w1, text) %>%
    count(w1)
}

build_unigram_model <- function(data, k = 1.0) {
  data %>%
    add_unknowns() %>%
    unnest_tokens(w1, text) %>%
    count(w1) %>%
    mutate(prob = (n + k) / (k * sum(n)))
}

################################################################################
# Bigram model functions



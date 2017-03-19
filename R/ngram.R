################################################################################
# Unigram model functions

count_words <- function(data) {
  data %>%
    unnest_tokens(w1, text) %>%
    count(w1)
}

build_unigram_model <- function(data) {
  data %>%
    unnest_tokens(w1, text) %>%
    count(w1) %>%
    mutate(prob = n / sum(n))
}

################################################################################
# Bigram model functions



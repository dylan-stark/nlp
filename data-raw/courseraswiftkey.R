library(tidyverse)
library(stringr)

set.seed(42)

# Sample some fraction of text
text_sample_frac <- function(filename, size = 1, replace = FALSE) {
  chunk_size <- 1e6
  previous_lines_seen <- 0

  lines <- integer(0)
  text <- character(0)

  while(length(chunk <- read_lines(filename, skip = previous_lines_seen, n_max = chunk_size, progress = TRUE)) > 0) {
    chunk_lines <- sample(length(chunk), length(chunk) * size, replace = replace)

    lines <- c(lines, previous_lines_seen + c(chunk_lines))
    text <- c(text, chunk[chunk_lines])

    previous_lines_seen <- previous_lines_seen + length(chunk)
  }

  data.frame(line = lines, text = text, stringsAsFactors = FALSE)
}

################################################################################

if(!file.exists("data-raw/final")) {
  temp_file <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", temp_file)
  data <- unzip(temp_file, exdir = "data-raw/.")
  unlink(temp_file)
}

# Sample ~10,000 records from each file
text <- tribble(
  ~filename, ~size,
  "data-raw/final/en_US/en_US.blogs.txt", 0.02,
  "data-raw/final/en_US/en_US.news.txt", 0.02,
  "data-raw/final/en_US/en_US.twitter.txt", 0.02
) %>%
  mutate(raw_text = map2(filename, size, text_sample_frac))

en_us_blogs <- text[text$filename == "data-raw/final/en_US/en_US.blogs.txt", ][["raw_text"]][[1]]
en_us_news <- text[text$filename == "data-raw/final/en_US/en_US.news.txt", ][["raw_text"]][[1]]
en_us_twitter <- text[text$filename == "data-raw/final/en_US/en_US.twitter.txt", ][["raw_text"]][[1]]

devtools::use_data(en_us_blogs, overwrite = TRUE)
devtools::use_data(en_us_news, overwrite = TRUE)
devtools::use_data(en_us_twitter, overwrite = TRUE)

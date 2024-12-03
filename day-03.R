library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)

input <- read_file("03-input.txt")

# day 1
input |>
  str_match_all("mul\\((?<x>\\d+),(?<y>\\d+)\\)") |>
  pluck(1) |>
  as_tibble(.name_repair = "unique") |>
  mutate(z = as.numeric(x) * as.numeric(y), .keep = "none") |>
  sum()

# day 2
mul_filter <- function(m) {
  do <- TRUE
  t <- tibble(x = numeric(), y = numeric())
  for (i in seq_len(nrow(m))) {
    if (m[i, 1] == "don't()") {
      do <- FALSE
      next
    } else if (m[i, 1] == "do()") {
      do <- TRUE
      next
    }
    if (!do) {
      next
    }
    t <- add_row(t, x = as.numeric(m[i, 2]), y = as.numeric(m[i, 3]))
  }

  return(t)
}

input |>
  str_match_all("mul\\((?<x>\\d+),(?<y>\\d+)\\)|do(n't)?\\(\\)") |>
  pluck(1) |>
  mul_filter() |>
  mutate(z = as.numeric(x) * as.numeric(y), .keep = "none") |>
  sum()

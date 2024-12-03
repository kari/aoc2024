library(readr)
library(purrr)
library(stringr)

input <- read_lines("03-input.txt")
input |>
  map(\(x) str_match_all(x, "mul\\((?<x>\\d+),(?<y>\\d+)\\)")) |>
  list_flatten() |>
  map_vec(\(x) as_tibble(x[, -1]) |> mutate(z = as.numeric(x) * as.numeric(y)) |> summarise(sum = sum(z))) |>
  sum()
  

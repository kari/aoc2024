library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

input <- read_lines("11-input.txt") |>
  str_split_fixed(" ", n = Inf) |>
  as.numeric() |>
  as_tibble() |>
  count(value)

blink <- function(set) {
  set |>
    mutate(
      next_1 = case_when(
        value == 0 ~ 1,
        floor(log10(value) + 1) %% 2 == 0 ~ value %/% 10^(floor(log10(value) + 1) / 2),
        .default = value * 2024
      ),
      next_2 = case_when(
        floor(log10(value) + 1) %% 2 == 0 ~ value %% 10^(floor(log10(value) + 1) / 2),
        .default = NA
      ),
      .keep = "unused"
    ) |>
    pivot_longer(
      cols = c(next_1, next_2),
      names_to = NULL,
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    summarise(n = sum(n), .by = value)
}

print(reduce(1:25, \(x, acc) blink(x), .init = input) |> summarise(sum(n)) |> pull())

format(reduce(1:75, \(x, acc) blink(x), .init = input) |> summarise(sum(n)) |> pull(), scientific = FALSE)

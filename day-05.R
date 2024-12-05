library(readr)
library(tibble)
library(tidyr)
library(purrr)
library(stringr)
library(dplyr)

input <- read_lines("05-input.txt")

rules <- input[1:(match("", input) - 1)] |>
  as_tibble() |>
  separate_wider_delim(value, delim = "|", names = c("before", "after")) |>
  mutate_all(as.numeric)

updates <- input[(match("", input) + 1):length(input)] |>
  str_split(pattern = ",") |>
  map(as.numeric)

# part 1
mids <- 0
fail <- list()
for (u in updates) {
  u_rules <- rules |> filter(before %in% u & after %in% u)
  correct <- TRUE
  for (i in seq_along(u)) {
    page <- u[i]
    before_rules <- u_rules |>
      filter(after == page) |>
      pull("before")
    after_rules <- u_rules |>
      filter(before == page) |>
      pull("after")
    if (i > 1) {
      if (!every(before_rules, \(x) x %in% u[1:(i - 1)])) {
        correct <- FALSE
      }
    }
    if (i < length(u)) {
      if (!every(after_rules, \(x) x %in% u[(i + 1):length(u)])) {
        correct <- FALSE
      }
    }
  }
  if (correct) {
    mids <- mids + u[(1 + length(u)) / 2]
  } else {
    fail <- append(fail, list(u))
  }
}
print(mids)

# part 2
mids <- 0
for (u in fail) {
  order <- numeric(0)
  u_rules <- rules |> filter(before %in% u & after %in% u)
  while (nrow(u_rules) > 0) {
    nopreds <- u_rules |>
      filter(!(before %in% after)) |>
      pull(before) |>
      unique()
    u_rules <- u_rules |> filter(!(before %in% nopreds))
    order <- append(order, nopreds)
  }
  order <- append(order, u[!(u %in% order)])
  mids <- mids + order[(1 + length(order)) / 2]
}
print(mids)

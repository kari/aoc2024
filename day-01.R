library(readr)
library(dplyr)
library(tibble)

input <- read_table("01-input.txt", col_names = FALSE)

# part 1
sum(abs(sort(input$X1) - sort(input$X2)))

# part 2

lookup <- input |>
  count(X2) |>
  mutate(score = X2 * n)

input |>
  select(X1) |>
  inner_join(lookup, by = join_by(X1 == X2)) |>
  summarise(sum = sum(score, na.rm = TRUE))

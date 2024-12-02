library(readr)
library(purrr)
library(dplyr)
library(tibble)

input <- read_table("01-input.txt", col_names = FALSE)

# part 1
sum(abs(sort(input$X1) - sort(input$X2)))

# part 2

x2_tbl <- input$X2 |>
  as_tibble() |>
  count(value) |>
  mutate(score = n * value)
input$X1 |>
  map_int(\(x) x2_tbl$score[match(x, x2_tbl$value)]) |>
  sum(na.rm = TRUE)

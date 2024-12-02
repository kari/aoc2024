library(readr)
library(meltr)
library(purrr)
library(dplyr)

input <- melt_delim("02-input.txt", delim = " ")

# part 1
safety <- function(vec) {
  vec |>
    diff() |>
    (\(x) min(abs(x)) >= 1 & max(abs(x)) <= 3 & (all(sign(x) < 0) | all(sign(x) > 0)))()
}

input |>
  mutate(value = as.integer(value)) |>
  unstack(value ~ row) |>
  map_lgl(safety) |>
  sum(na.rm = TRUE)

# part 2

dampener <- function(vec) {
  if (safety(vec)) {
    return(TRUE)
  } else {
    for (i in seq_along(vec)) {
      vec2 <- vec[-c(i)]
      if (safety(vec2)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

input |>
  mutate(value = as.integer(value)) |>
  unstack(value ~ row) |>
  map_lgl(dampener) |>
  sum(na.rm = TRUE)

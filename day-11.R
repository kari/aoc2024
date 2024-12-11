library(readr)
library(stringr)

input <- read_lines("11-input.txt") |>
  str_split_fixed(" ", n = Inf) |>
  as.numeric()

# part 1
for (i in 1:25) {
  i <- 1
  new_input <- rep(NA, 2 * length(input))
  for (j in seq_along(input)) {
    if (input[j] == 0) {
      new_input[i] <- 1
      i <- i + 1
    } else if (input[j] > 9 && floor(log10(input[j]) + 1) %% 2 == 0) {
      m <- 10^(floor(log10(input[j]) + 1) / 2)
      new_input[i] <- input[j] %/% m
      new_input[i + 1] <- input[j] %% m
      i <- i + 2
    } else {
      new_input[i] <- 2024 * input[j]
      i <- i + 1
    }
  }
  input <- new_input[1:(i - 1)]
}
print(length(input))

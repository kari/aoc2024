library(readr)
library(stringr)

input <- read_lines("11-input.txt") |>
  str_split_fixed(" ", n = Inf) |>
  as.numeric()

# part 1
for (i in 1:75) {
  print(i)
  print(length(input))
  i <- 1
  new_input <- rep(NA, 2 * length(input))
  for (v in input) {
    if (v == 0) {
      new_input[i] <- 1
      i <- i + 1
    } else if (str_length(v) %% 2 == 0) {
      m <- 10^(str_length(v) / 2)
      l <- v %/% m
      r <- v %% m
      new_input[i] <- l
      new_input[i + 1] <- r
      i <- i + 2
    } else {
      new_input[i] <- 2024 * v
      i <- i + 1
    }
  }
  # print(new_input)
  input <- new_input[1:(i - 1)]
}
print(length(input))

library(readr)
library(stringr)
library(profvis)

input <- read_lines("11-input.txt") |>
  str_split_fixed(" ", n = Inf) |>
  as.numeric()

# part 1
# profvis({
for (i in 1:25) {
  print(i)
  print(length(input))
  i <- 1
  new_input <- rep(NA, 2 * length(input))
  for (v in input) {
    if (v == 0) {
      new_input[i] <- 1
      i <- i + 1
    } else if (v > 9 && floor(log10(v) + 1) %% 2 == 0) {
      m <- 10^(floor(log10(v) + 1) / 2)
      new_input[i] <- v %/% m
      new_input[i + 1] <- v %% m
      i <- i + 2
    } else {
      new_input[i] <- 2024 * v
      i <- i + 1
    }
  }
  # print(new_input)
  input <- new_input[1:(i - 1)]
}
# })
print(length(input))

library(readr)
library(stringr)

input <- read_lines("11-input.txt") |>
  str_split_fixed(" ", n = Inf) |>
  as.numeric()

# part 1
for (i in 1:25) {
  print(i)
  i <- 1
  new_input <- rep(NA, 2 * length(input))
  for (v in input) {
    if (v == 0) {
      new_input[i] <- 1
      i <- i + 1
    } else if (str_length(v) %% 2 == 0) {
      l <- as.numeric(str_sub(as.character(v), 1, str_length(v) / 2))
      r <- as.numeric(str_sub(as.character(v), str_length(v) / 2 + 1, str_length(v)))
      new_input[i] <- l
      new_input[i + 1] <- r
      i <- i + 2
    } else {
      new_input[i] <- 2024 * v
      i <- i + 1
    }
  }
  input <- new_input[1:(i - 1)]
}
print(length(input))

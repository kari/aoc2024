library(meltr)
library(dplyr)
library(stringr)
library(purrr)
library(arrangements)

input <- melt_delim("07-input.txt", delim = " ") |>
  mutate(value = case_when(
                           data_type == "character" ~ as.numeric(str_sub(value, end = -2)),
                           .default = as.numeric(value))
  ) |>
  unstack(value ~ row)

f <- function(acc, val,  op) {
  if (op == "*") {
    return(acc * val)
  } else if (op == "+") {
    return(acc + val)
  } else if (op == "||") {
    return(acc * 10^str_length(val) + val)
  }
}

# part 1
acc <- 0
for (i in input) {
  result <- i[1]
  values <- i[2:length(i)]
  ops <- c("*", "+")
  n <- length(ops) ^ (length(values) - 1)
  for (i in seq_len(n)) {
    perm <- rev(as.numeric(intToBits(i)))[-(1:(32 - (length(values) - 1)))] + 1
    if (result == reduce2(values, ops[perm], f)) {
      acc <- acc + result
      break
    }
  }
}
format(acc, scientific = FALSE)

# part 2
acc <- 0
for (i in input) {
  result <- i[1]
  values <- i[2:length(i)]
  ops <- c("*", "+", "||")
  for (perm in permutations(ops, replace = TRUE, k = length(values) - 1, layout = "list")) {
    if (result == reduce2(values, perm, f)) {
      acc <- acc + result
      break
    }
  }
}
format(acc, scientific = FALSE)

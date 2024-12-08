library(meltr)
library(dplyr)
library(stringr)
library(purrr)

input <- melt_delim("07-input.txt", delim = " ") |>
  mutate(value = case_when(
                           data_type == "character" ~ as.numeric(str_sub(value, end = -2)),
                           .default = as.numeric(value))
  ) |>
  unstack(value ~ row)

f <- function(acc, val,  op) {
  if (op == "*") {
    return(acc * val)
  } else {
    return(acc + val)
  }
}

acc <- 0
for (i in input) {
  result <- i[1]
  values <- i[2:length(i)]
  n <- 2 ^ (length(values) - 1)
  ops <- c("*", "+")
  for (i in seq_len(n)) {
    perm <- rev(as.numeric(intToBits(i)))[-(1:(32 - (length(values) - 1)))] + 1
    if (result == reduce2(values, ops[perm], f)) {
      acc <- acc + result
      break
    }
  }
}
format(acc, scientific = FALSE)

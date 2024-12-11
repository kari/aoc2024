library(readr)
library(stringr)
library(collections)
library(purrr)

input <- read_lines("11-input.txt") |>
  str_split_fixed(" ", n = Inf) |>
  as.numeric()
input2 <- input

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

# part 2
cache <- dict()

score <- function(value, blinks) {

  if (length(value) > 1) {
    return(sum(map_vec(value, \(x) score(x, blinks))))
  }

  if (blinks == 0) {
    return(1)
  }

  if (cache$has(c(value, blinks))) {
    return(cache$get(c(value, blinks)))
  }

  if (value == 0) {
    res <- Recall(1, blinks - 1)
  } else if (floor(log10(value) + 1) %% 2 == 0) {
    m <- 10^(floor(log10(value) + 1) / 2)
    res <- Recall(value %/% m, blinks - 1) + Recall(value %% m, blinks - 1)
  } else {
    res <- Recall(value * 2024, blinks - 1)
  }

  cache$set(c(value, blinks), res)

  return(res)
}

format(score(input2, 75), scientific = FALSE)

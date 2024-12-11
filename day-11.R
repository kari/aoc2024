library(readr)
library(stringr)

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
score <- local({
  cache <- list()

  function(value, blinks) {
    if (length(value) > 1) {
      acc <- 0
      for (v in value) {
        acc <- acc + Recall(v, blinks)
      }
      return(acc)
    }

    if (blinks == 0) {
      return(1)
    }

    cache_key <- paste(value, blinks, sep = "-")
    if (!is.null(cache[[cache_key]])) {
      # print(paste("cache hit", cache_key))
      return(cache[[cache_key]])
    }

    if (value == 0) {
      res <- Recall(1, blinks - 1)
    } else if (floor(log10(value) + 1) %% 2 == 0) {
      m <- 10^(floor(log10(value) + 1) / 2)
      res <- Recall(value %/% m, blinks - 1) + Recall(value %% m, blinks - 1)
    } else {
      res <- Recall(value * 2024, blinks - 1)
    }

    cache[[cache_key]] <<- res

    if (length(cache) %% 5000 == 0) {
      print(length(cache))
      print(cache_hits / (cache_hits + cache_misses))
    }

    return(res)
  }
})

format(score(input2, 75), scientific = FALSE)

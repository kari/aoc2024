library(readr)
library(stringr)

input <- read_lines("04-input.txt") |>
  str_split_fixed("", n = Inf)

# day 1
cnt <- 0
for (i in seq_len(nrow(input))) {
  for (j in seq_len(ncol(input))) {
    if (input[i, j] == "X") { # might start a sequence
      if (j + 3 <= ncol(input)) { # to right
        if (paste(input[i, j:(j + 3)], collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (j >= 4) { # to left
        if (paste(input[i, j:(j - 3)], collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (i + 3 <= nrow(input)) { # down
        if (paste(input[i:(i + 3), j], collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (i >= 4) { # up
        if (paste(input[i:(i - 3), j], collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (j + 3 <= ncol(input) && (i + 3) <= nrow(input)) { # diagonal down-right
        if (paste(diag(input[i:(i + 3), j:(j + 3)]), collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (j >= 4 && i >= 4) { # diagonal up-left
        if (paste(diag(input[i:(i - 3), j:(j - 3)]), collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (j >= 4 && (i + 3) <= nrow(input)) { # diagonal down-left
        if (paste(diag(input[i:(i + 3), j:(j - 3)]), collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }
      if (j + 3 <= ncol(input) && i >= 4) { # diagonal up-right
        if (paste(diag(input[i:(i - 3), j:(j + 3)]), collapse = "") == "XMAS") {
          cnt <- cnt + 1
        }
      }

    }
  }
}
cnt

# part 2
cnt <- 0
for (i in 2:(nrow(input) - 1)) {
  for (j in 2:(ncol(input) - 1)) {
    if (input[i, j] == "A") { # might start a sequence
      if (paste(diag(input[(i - 1):(i + 1), (j - 1):(j + 1)]), collapse = "") %in% c("MAS", "SAM") && paste(diag(input[(i - 1):(i + 1), (j + 1):(j - 1)]), collapse = "") %in% c("MAS", "SAM")) {
        cnt <- cnt + 1
      }
    }
  }
}
cnt

library(readr)
library(stringr)
library(purrr)

input <- read_lines("09-input.txt") |>
  str_split_fixed("", n = Inf) |>
  as.numeric()

# part 1
id <- 0
idx <- 1
file <- TRUE
blocks <- rep(NA, sum(input))
for (digit in input) {
  if (file) {
    blocks[idx:(idx + digit - 1)] <- id
    id <- id + 1
  }
  file <- !file
  idx <- idx + digit
}
# print(blocks)

# FIXME: empty places do not change, so no need to check them every iteration
while (length(which(is.na(blocks[1:max(which(!is.na(blocks)))])))) {
  idx <- max(which(!is.na(blocks)))
  blocks[min(which(is.na(blocks)))] <- blocks[idx]
  blocks[idx] <- NA
  # print(blocks)
}

acc <- reduce2(blocks[1:max(which(!is.na(blocks)))], seq_along(blocks[1:max(which(!is.na(blocks)))]), \(acc, x, y) return(acc + x*(y-1)), .init = 0)

format(acc, scientific = FALSE)

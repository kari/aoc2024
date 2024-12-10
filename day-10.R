library(readr)
library(stringr)
library(purrr)

input <- read_lines("10-input.txt") |>
  str_split_fixed("", n = Inf)

input <- input |>
  as.numeric() |>
  matrix(ncol = ncol(input), nrow(input))

trailheads <- which(input == 0, arr.ind = TRUE)
# trailheads <- trailheads[1:2, ]
acc <- 0
for (t in seq_len(nrow(trailheads))) {
  # print(paste("at trailhead", trailheads[t, 1], trailheads[t, 2]))
  cursors <- list(trailheads[t, ])
  while (!every(cursors, \(x) input[x[1], x[2]] == 9) && length(cursors) > 0) {
    new_cursors <- list()

    for (c in seq_along(cursors)) {
      row <- cursors[[c]][1]
      col <- cursors[[c]][2]
      z <- input[row, col]
      # print(paste("at", row, col, z))

      if (row > 1 && input[row - 1, col] == z + 1) {
        # print(paste("can climb to", row - 1, col, z + 1))
        new_cursors <- append(new_cursors, list(c(row - 1, col)))
      }
      if (row < nrow(input) && input[row + 1, col] == z + 1) {
        # print(paste("can climb to", row + 1, col, z + 1))
        new_cursors <- append(new_cursors, list(c(row + 1, col)))
      }
      if (col > 1 && input[row, col - 1] == z + 1) {
        # print(paste("can climb to", row, col - 1, z + 1))
        new_cursors <- append(new_cursors, list(c(row, col - 1)))
      }
      if (col < ncol(input) && input[row, col + 1] == z + 1) {
        # print(paste("can climb to", row, col + 1, z + 1))
        new_cursors <- append(new_cursors, list(c(row, col + 1)))
      }
    }
    cursors <- new_cursors
  }
  # print(cursors)
  acc <- acc + length(unique(cursors))
}
print(acc)

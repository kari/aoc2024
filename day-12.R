library(readr)
library(stringr)

input <- read_lines("12-input.txt") |>
  str_split_fixed("", n = Inf)

mapped <- matrix(nrow = nrow(input), ncol = ncol(input))

regions <- list()

while (length(which(is.na(mapped))) > 0) {
  idx <- which(is.na(mapped), arr.ind = TRUE)[1, ]
  letter <- input[idx[1], idx[2]]
  cursors <- list(idx)
  area <- 0
  perimeter <- 0

  while (length(cursors) > 0) {
    new_cursors <- list()

    for (i in seq_along(cursors)) {
      row <- cursors[[i]][1]
      col <- cursors[[i]][2]
      mapped[row, col] <- TRUE
      perimeter <- perimeter + 4
      area <- area + 1

      # print(paste(letter, row, col))
      if (row > 1 && input[row - 1, col] == letter) {
        perimeter <- perimeter - 1
        if (is.na(mapped[row - 1, col])) {
          mapped[row - 1, col] <- TRUE
          new_cursors <- append(new_cursors, list(c(row - 1, col)))
        }
      }
      if (col > 1 && input[row, col - 1] == letter) {
        perimeter <- perimeter - 1
        if (is.na(mapped[row, col - 1])) {
          mapped[row, col - 1] <- TRUE
          new_cursors <- append(new_cursors, list(c(row, col - 1)))
        }
      }
      if (row < nrow(input) && input[row + 1, col] == letter) {
        perimeter <- perimeter - 1
        if (is.na(mapped[row + 1, col])) {
          mapped[row + 1, col] <- TRUE
          new_cursors <- append(new_cursors, list(c(row + 1, col)))
        }
      }
      if (col < ncol(input) && input[row, col + 1] == letter) {
        perimeter <- perimeter - 1
        if (is.na(mapped[row, col + 1])) {
          mapped[row, col + 1] <- TRUE
          new_cursors <- append(new_cursors, list(c(row, col + 1)))
        }
      }
    }
    cursors <- new_cursors
  }
  regions <- append(regions, list(list(letter = letter, area = area, perimeter = perimeter)))
}

print(sum(map_vec(regions, \(x) x$area * x$perimeter)))

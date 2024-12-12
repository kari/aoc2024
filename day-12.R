library(readr)
library(stringr)
library(purrr)

input <- read_lines("12-input.txt") |>
  str_split_fixed("", n = Inf)

# part 1
mapped <- matrix(nrow = nrow(input), ncol = ncol(input))
regions <- list()

while (length(which(is.na(mapped))) > 0) {
  idx <- which(is.na(mapped), arr.ind = TRUE)[1, ]
  letter <- input[idx[1], idx[2]]
  cursors <- list(idx)
  area <- 0
  perimeter <- 0
  mask <- matrix(nrow = nrow(input), ncol = ncol(input))

  while (length(cursors) > 0) {
    new_cursors <- list()

    for (i in seq_along(cursors)) {
      row <- cursors[[i]][1]
      col <- cursors[[i]][2]
      mapped[row, col] <- TRUE
      mask[row, col] <- TRUE
      perimeter <- perimeter + 4
      area <- area + 1

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

  regions <- append(regions, list(list(
    letter = letter,
    area = area,
    perimeter = perimeter,
    mask = mask
  )))
}

print(sum(map_vec(regions, \(x) x$area * x$perimeter)))

# part 2
for (i in seq_along(regions)) {
  r <- regions[[i]]
  sides <- 0

  for (j in which(regions[[i]]$mask)) {
    idx <- arrayInd(j, .dim = dim(input))
    row <- idx[1]
    col <- idx[2]

    # top sides
    if (!(row > 1 && input[row - 1, col] == r$letter) && !(col > 1 && input[row, col - 1] == r$letter && (row == 1 || input[row - 1, col - 1] != r$letter))) {
      sides <- sides + 1
    }

    # left sides
    if (!(col > 1 && input[row, col - 1] == r$letter) && !(row < nrow(input) && input[row + 1, col] == r$letter && (col == 1 || input[row + 1, col - 1] != r$letter))) {
      sides <- sides + 1
    }

    # right sides
    if (!(col < ncol(input) && input[row, col + 1] == r$letter) && !(row > 1 && input[row - 1, col] == r$letter && (col == ncol(input) || input[row - 1, col + 1] != r$letter))) {
      sides <- sides + 1
    }

    # bottom sides
    if (!(row < nrow(input) && input[row + 1, col] == r$letter) && !(col > 1 && input[row, col - 1] == r$letter && (row == nrow(input) || input[row + 1, col - 1] != r$letter))) {
      sides <- sides + 1
    }
  }
  regions[[i]]$sides <- sides
}

print(sum(map_vec(regions, \(x) x$area * x$sides)))

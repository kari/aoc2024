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
  rows <- min(which(regions[[i]]$mask == TRUE, arr.ind = TRUE)[, "row"]):max(which(regions[[i]]$mask == TRUE, arr.ind = TRUE)[, "row"])
  cols <- min(which(regions[[i]]$mask == TRUE, arr.ind = TRUE)[, "col"]):max(which(regions[[i]]$mask == TRUE, arr.ind = TRUE)[, "col"])

  # top sides
  for (row in rows) {
    for (col in cols) {
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (row > 1 && input[row - 1, col] == r$letter) { # skip if there's same letter on top
        next
      }
      # if there's a same letter on left, can't start a side if
      # it's top row of data frame or
      # left was empty on top
      if (col > 1 && input[row, col - 1] == r$letter && (row == 1 || input[row - 1, col - 1] != r$letter)) {
        next
      }
      sides <- sides + 1
    }
  }

  # left sides
  for (col in cols) {
    for (row in rev(rows)) { # note down-to-up
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (col > 1 && input[row, col - 1] == r$letter) { # skip if there's same letter on left
        next
      }
      # if there's a same letter on below, can't start a side if
      # it's first col of data frame or
      # bottom was empty on left
      if (row < nrow(input) && input[row + 1, col] == r$letter && (col == 1 || input[row + 1, col - 1] != r$letter)) {
        next
      }
      sides <- sides + 1
    }
  }

  # right sides
  for (col in rev(cols)) { # right to left
    for (row in rows) {
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (col < ncol(input) && input[row, col + 1] == r$letter) { # skip if there's same letter on right
        next
      }
      # if there's a same letter up, can't start a side if
      # it's last col of data frame or
      # up was empty on right
      if (row > 1 && input[row - 1, col] == r$letter && (col == ncol(input) || input[row - 1, col + 1] != r$letter)) {
        next
      }
      sides <- sides + 1
    }
  }

  # bottom sides
  for (row in rev(rows)) {
    for (col in cols) {
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (row < nrow(input) && input[row + 1, col] == r$letter) { # skip if there's same letter on below
        next
      }
      # if there's a same letter on left, can't start a side if
      # it's last row of data frame or
      # left was empty on bottom
      if (col > 1 && input[row, col - 1] == r$letter && (row == nrow(input) || input[row + 1, col - 1] != r$letter)) {
        next
      }
      sides <- sides + 1
    }
  }
  regions[[i]]$sides <- sides
}

print(sum(map_vec(regions, \(x) x$area * x$sides)))

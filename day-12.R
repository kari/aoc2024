library(readr)
library(stringr)

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
  bounding_box <- c(idx[1], idx[2], idx[1], idx[2]) # top, left, bottom, right. can be inferred from mask.
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

      if (row < bounding_box[1]) {
        bounding_box[1] <- row
      }
      if (col < bounding_box[2]) {
        bounding_box[2] <- col
      }
      if (row > bounding_box[3]) {
        bounding_box[3] <- row
      }
      if (col > bounding_box[4]) {
        bounding_box[4] <- col
      }

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
  regions <- append(regions, list(list(letter = letter, area = area, perimeter = perimeter, bounding_box = bounding_box, mask = mask)))
}

print(sum(map_vec(regions, \(x) x$area * x$perimeter)))

# part 2
for (i in seq_along(regions)) {
  r <- regions[[i]]
  sides <- 0
  # print(r$letter)

  # top sides
  for (row in r$bounding_box[1]:r$bounding_box[3]) {
    for (col in r$bounding_box[2]:r$bounding_box[4]) {
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (row > 1 && input[row - 1, col] == r$letter) { # skip if there's same letter on top
        next
      }
      # if there's a same letter on left, can't start a side if
      # it's top row of data frame or
      # it's top row of bounding box or
      # left was empty on top
      if (col > 1 && input[row, col - 1] == r$letter && (row == 1 || row == r$bounding_box[1] || input[row - 1, col - 1] != r$letter)) {
        next
      }
      # print(paste("top side starts at", row, col))
      sides <- sides + 1
    }
  }

  # left sides
  for (col in r$bounding_box[2]:r$bounding_box[4]) {
    for (row in r$bounding_box[3]:r$bounding_box[1]) { # note down-to-up
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (col > 1 && input[row, col - 1] == r$letter) { # skip if there's same letter on left
        next
      }
      # if there's a same letter on below, can't start a side if
      # it's first col of data frame or
      # it's first col of bounding box or
      # bottom was empty on left
      if (row < nrow(input) && input[row + 1, col] == r$letter && (col == 1 || col == r$bounding_box[2] || input[row + 1, col - 1] != r$letter)) {
        # print(paste(row, col, col == 1, row == r$bounding_box[2], input[row + 1, col - 1] != r$letter))
        next
      }
      # print(paste("left side starts at", row, col))
      sides <- sides + 1
    }
  }

  # right sides
  for (col in r$bounding_box[4]:r$bounding_box[2]) { # right to left
    for (row in r$bounding_box[1]:r$bounding_box[3]) {
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (col < ncol(input) && input[row, col + 1] == r$letter) { # skip if there's same letter on right
        next
      }
      # if there's a same letter up, can't start a side if
      # it's last col of data frame or
      # it's last col of bounding box or
      # up was empty on right
      if (row > 1 && input[row - 1, col] == r$letter && (col == ncol(input) || col == r$bounding_box[4] || input[row - 1, col + 1] != r$letter)) {
        next
      }
      # print(paste("right side starts at", row, col))
      sides <- sides + 1
    }
  }

  # bottom sides
  for (row in r$bounding_box[3]:r$bounding_box[1]) {
    for (col in r$bounding_box[2]:r$bounding_box[4]) {
      if (is.na(r$mask[row, col])) { # skip if not in region
        next
      }
      if (row < nrow(input) && input[row + 1, col] == r$letter) { # skip if there's same letter on below
        next
      }
      # if there's a same letter on left, can't start a side if
      # it's last row of data frame or
      # it's last row of bounding box or
      # left was empty on bottom
      if (col > 1 && input[row, col - 1] == r$letter && (row == nrow(input) || row == r$bounding_box[3] || input[row + 1, col - 1] != r$letter)) {
        next
      }
      # print(paste("bottom side starts at", row, col))
      sides <- sides + 1
    }
  }
  # print(paste(r$letter, r$area, sides, r$area * sides))
  regions[[i]]$sides <- sides
}

print(sum(map_vec(regions, \(x) x$area * x$sides)))

library(readr)
library(stringr)
library(arrangements)

input <- read_lines("08-input.txt") |>
  str_split_fixed("", n = Inf)

antinodes <- matrix(nrow = nrow(input), ncol = ncol(input))

antennas <-  c(LETTERS, letters, 0:9)[c(LETTERS, letters, 0:9) %in% input]

for (antenna in antennas) {
  idxs <- which(input == antenna)
  pairs <- permutations(idxs, k = 2)

  for (i in seq_len(nrow(pairs))) {
    x <- arrayInd(pairs[i, 1], .dim = dim(input))
    y <- arrayInd(pairs[i, 2], .dim = dim(input))

    a1 <- x + (x - y)
    a2 <- y + (y - x)

    if(a1[1] >= 1 && a1[1] <= nrow(antinodes) && a1[2] >= 1 && a1[2] <= ncol(antinodes)) {
      antinodes[a1[1], a1[2]] <- 1
    }
    if(a2[1] >= 1 && a2[1] <= nrow(antinodes) && a2[2] >= 1 && a2[2] <= ncol(antinodes)) {
      antinodes[a2[1], a2[2]] <- 1
    }
  }
}

print(sum(antinodes, na.rm = TRUE))

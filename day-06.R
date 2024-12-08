library(readr)
library(purrr)

input <- read_lines("06-sample.txt") |>
  str_split_fixed("", n = Inf)

path <- matrix(nrow = nrow(input), ncol = ncol(input))
starting <- which(input == "^", TRUE)
guard <- starting
angle <- 0.5

# part 1
while (TRUE) {
  path[guard] <- 1
  new_point <- guard + c(-sinpi(angle), cospi(angle))

  if (new_point[1] < 1 || new_point[1] > nrow(input) || new_point[2] < 1 || new_point[2] > ncol(input)) {
    break
  } else if (input[new_point] == "#") {
    angle <- angle - 0.5
    next
  }
  guard <- new_point
}

print(sum(path, na.rm = TRUE))

# part 2
loops <- 0
for (i in seq_along(input)) {
  # print(paste(i, "/", length(input)))
  map <- input
  map[i] <- "O"
  path <- vector(mode = "list", length = length(map))
  staring <- which(map == "^", TRUE)
  guard <- starting
  angle <- 0.5
  turns <- 0

  while (TRUE) {
    idx <- (guard[2] - 1) * nrow(map) + guard[1]
    if (is.null(path[[idx]])) {
      path[[idx]] <- angle
    } else if (angle %in% path[[idx]]) {
      print("infinite loop")
      # print(map2_vec(map, path, \(x,y) if (is.na(y)) { x } else { y } ) |> matrix(ncol=ncol(input), nrow=nrow(input)))
      loops <- loops + 1
      break
    } else {
      path[[idx]] <- append(path[[idx]], angle)
    }

    new_point <- guard + c(-sinpi(angle), cospi(angle))

    if (new_point[1] < 1 || new_point[1] > nrow(input) || new_point[2] < 1 || new_point[2] > ncol(input)) {
      break
    } else if (map[new_point] %in% c("#", "O")) {
      angle <- angle - 0.5
      if (angle < 0) {
        angle <- angle + 2
      }
      next
    }

    guard <- new_point
  }
}
print(loops)

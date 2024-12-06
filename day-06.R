library(readr)

input <- read_lines("06-input.txt") |> str_split_fixed("", n = Inf)

path <- matrix(nrow = nrow(input), ncol = ncol(input))
guard <- which(input == "^", TRUE)
angle <- 0.5

while (TRUE) {
  path[guard] <- 1
  new_point <- guard + c(-sinpi(angle), cospi(angle))

  if (new_point[1] < 1 | new_point[1] > nrow(input) | new_point[2] < 1 | new_point[2] > ncol(input)) {
    break
  } else if (input[new_point] == "#") {
    angle <- angle - 0.5
    next
  }
  guard <- new_point
}

print(sum(path, na.rm = TRUE))

library(readr)
library(purrr)
library(stringr)

is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

input <- read_lines("13-input.txt", skip_empty_rows = TRUE) |>
  (\(x) split(x, ceiling(seq_along(x) / 3)))() |>
  map(\(x) paste(x, collapse = " ")) |>
  str_extract_all("(\\d+)") |>
  map(\(x) list(A = matrix(as.numeric(x[1:4]), nrow = 2), b = as.numeric(x[5:6]))) |>
  map(\(x) solve(x$A, x$b)) |>
  keep(\(x) every(x, \(y) is_wholenumber(y))) |>
  unlist() |>
  matrix(nrow = 2)

print(sum(colSums(input * c(3, 1))))

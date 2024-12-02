library(readr)

# part 1
input <- melt_delim("02-input.txt", delim=" ") |> 
  mutate(value = as.integer(value)) |> 
  unstack(value ~ row) |>
  map(diff) |>
  map_lgl(\(x) min(abs(x)) >= 1 & max(abs(x)) <= 3 & (all(sign(x) < 0) | all(sign(x) > 0))) |>
  sum(na.rm = TRUE)


  
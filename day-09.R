library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

input <- read_lines("09-input.txt") |>
  str_split_fixed("", n = Inf) |>
  as.numeric()

# part 1
id <- 0
idx <- 1
file <- TRUE
blocks <- rep(NA, sum(input))
for (digit in input) {
  if (file) {
    blocks[idx:(idx + digit - 1)] <- id
    id <- id + 1
  }
  file <- !file
  idx <- idx + digit
}
blocks2 <- blocks

while (length(which(is.na(blocks[1:max(which(!is.na(blocks)))])))) {
  idx <- max(which(!is.na(blocks)))
  blocks[min(which(is.na(blocks)))] <- blocks[idx]
  blocks[idx] <- NA
}

acc <- reduce2(blocks[1:max(which(!is.na(blocks)))], seq_along(blocks[1:max(which(!is.na(blocks)))]), \(acc, x, y) return(acc + x*(y-1)), .init = 0)

format(acc, scientific = FALSE)

# part 2
blocks <- blocks2
# which(is.na(blocks[1:max(which(!is.na(blocks)))]))
block_map <- tibble(block = blocks, grouping = rep(seq_along(rle(replace_na(blocks, -1))$lengths), rle(replace_na(blocks, -1))$lengths)) |>
  mutate(idx = row_number()) |>
  group_by(grouping) |>
  summarise(idx = first(idx), block = first(block), len = n())

for (b in max(blocks, na.rm = TRUE):min(blocks, na.rm = TRUE)) {
  blck <- block_map |> filter(block == b)
  new_pos <- block_map |> filter(is.na(block) & idx < blck$idx & len >= blck$len) |> arrange(idx) |> first()
  if (is.na(new_pos$idx)) {
    next
  }
  blocks[new_pos$idx:(new_pos$idx + blck$len - 1)] <- b
  blocks[blck$idx:(blck$idx + blck$len - 1)] <- NA
  if (new_pos$len == blck$len) {
    block_map <- block_map |>
      mutate(idx = if_else(block == b, new_pos$idx, idx, missing = idx)) |> # move block to free space
      mutate(idx = if_else(is.na(block) & idx == new_pos$idx, blck$idx, idx)) # move free space to block
  } else {
    block_map <- block_map |>
      mutate(idx = if_else(block == b, new_pos$idx, idx, missing = idx)) |>
      mutate(idx = if_else(is.na(block) & idx == new_pos$idx, blck$idx, idx), len = if_else(is.na(block) & idx == blck$idx, blck$len, len)) |>
      add_row(idx = new_pos$idx + blck$len, block = NA, len = new_pos$len - blck$len)
  }
}
# print(blocks)

acc <- reduce2(blocks[1:max(which(!is.na(blocks)))], seq_along(blocks[1:max(which(!is.na(blocks)))]), \(acc, x, y) { if (is.na(x)) { return(acc) } else { return(acc + x*(y-1)) } }, .init = 0)

format(acc, scientific = FALSE)

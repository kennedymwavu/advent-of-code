input <- readLines(con = "input.txt")

# input looks like this:
# > input
# [1] "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
# [2] "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
# [3] "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
# [4] "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
# [5] "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
# [6] "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"


input |>
  gsub(pattern = "Card [0-9]:\\s+", replacement = "") |>
  strsplit(split = " | ", fixed = TRUE) |>
  lapply(\(char) {
    char_split <- strsplit(char, split = " ", fixed = TRUE) |>
      lapply(FUN = Filter, f = nzchar)
    n <- sum(char_split[[2]] %in% char_split[[1]])
    if (n == 0) 0 else 2^(n - 1)
  }) |>
  unlist() |>
  sum()

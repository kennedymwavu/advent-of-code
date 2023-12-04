# part 2----
input <- readLines(con = "input.txt")

input_mtrx <- strsplit(input, split = "") |> do.call(what = rbind)

# input_mtrx looks like this:
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,] "4"  "6"  "7"  "."  "."  "1"  "1"  "4"  "."  "."
#  [2,] "."  "."  "."  "*"  "."  "."  "."  "."  "."  "."
#  [3,] "."  "."  "3"  "5"  "."  "."  "6"  "3"  "3"  "."
#  [4,] "."  "."  "."  "."  "."  "."  "#"  "."  "."  "."
#  [5,] "6"  "1"  "7"  "*"  "."  "."  "."  "."  "."  "."
#  [6,] "."  "."  "."  "."  "."  "+"  "."  "5"  "8"  "."
#  [7,] "."  "."  "5"  "9"  "2"  "."  "."  "."  "."  "."
#  [8,] "."  "."  "."  "."  "."  "."  "7"  "5"  "5"  "."
#  [9,] "."  "."  "."  "$"  "."  "*"  "."  "."  "."  "."
# [10,] "."  "6"  "6"  "4"  "."  "5"  "9"  "8"  "."  "."

rows <- seq_len(nrow(input_mtrx))
cols <- seq_len(ncol(input_mtrx))

is_digit <- \(x) grepl(pattern = "[0-9]", x = x)
is_single_digit <- \(x) grepl(pattern = "^[0-9]$", x = x)

# ensure that consecutive numbers have the same value instead of being single
# digits eg.
#       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9]  [,10]
#  [1,] "467" "467" "467" "."   "."   "114" "114" "114" "."   "."
#  [2,] "."   "."   "."   "*"   "."   "."   "."   "."   "."   "."
#  [3,] "."   "."   "35"  "35"  "."   "."   "633" "633" "633" "."
#  [4,] "."   "."   "."   "."   "."   "."   "#"   "."   "."   "."
#  [5,] "617" "617" "617" "*"   "."   "."   "."   "."   "."   "."
#  [6,] "."   "."   "."   "."   "."   "+"   "."   "58"  "58"  "."
#  [7,] "."   "."   "592" "592" "592" "."   "."   "."   "."   "."
#  [8,] "."   "."   "."   "."   "."   "."   "755" "755" "755" "."
#  [9,] "."   "."   "."   "$"   "."   "*"   "."   "."   "."   "."
# [10,] "."   "664" "664" "664" "."   "598" "598" "598" "."   "."
for (r in rows) {
  current_row <- input_mtrx[r, ]
  row_rle <- is_single_digit(current_row) |> rle()
  row_rle_values <- row_rle$values
  row_rle_lengths <- row_rle$lengths
  row_rle_lengths_cumsum <- row_rle_lengths |> cumsum()
  from <- 0
  for (i in seq_along(row_rle_lengths)) {
    inds <- seq(from = from + 1, to = row_rle_lengths_cumsum[i])
    if (row_rle_values[i]) {
      current_row[inds] <- paste(current_row[inds], collapse = "")
    }
    from <- row_rle_lengths_cumsum[i]
  }

  input_mtrx[r, ] <- current_row
}

star_inds <- which(input_mtrx == "*", arr.ind = TRUE)
# > star_inds
#      row col
# [1,]   2   4
# [2,]   5   4
# [3,]   9   6

part_numbers <- list()

for (i in seq_len(nrow(star_inds))) {
  r <- star_inds[i, "row"]
  c <- star_inds[i, "col"]
  start_row <- if ((r - 1) %in% rows) r - 1 else r
  end_row <- if ((r + 1) %in% rows) r + 1 else r
  row_inds <- start_row:end_row
  start_col <- if ((c - 1) %in% cols) c - 1 else c
  end_col <- if ((c + 1) %in% cols) c + 1 else c
  col_inds <- start_col:end_col
  around_star <- input_mtrx[row_inds, col_inds]
  digit_inds <- is_digit(around_star) |>
    matrix(nrow = nrow(around_star)) |>
    which(arr.ind = TRUE)
  nums <- around_star[is_digit(around_star)] |>
    unique() |>
    as.integer()
  if (length(nums) == 2) {
    part_numbers[[length(part_numbers) + 1]] <- nums
  }
}

part_numbers |>
  lapply(prod) |>
  unlist() |>
  sum()

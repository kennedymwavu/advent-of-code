input <- readLines(con = "input.txt")

# part 1----
input_mtrx <- strsplit(input, split = "") |> do.call(what = rbind)

is_symbol_and_not_dot <- \(x) {
  x != "." & !x %in% c(letters, LETTERS) & !x %in% 0:9
}

is_single_digit <- \(x) grepl(pattern = "^[0-9]$", x = x)

# Loop through each value in the input matrix and check whether it's
# adjacent to a symbol (left, right or diagonally).
# If so, keep track of it as a 1 in the `adjacent_to_symbol` matrix.
rows <- seq_len(nrow(input_mtrx))
cols <- seq_len(ncol(input_mtrx))
adjacent_to_symbol <- matrix(0, nrow = length(rows), ncol = length(cols))
for (i in rows) {
  for (j in cols) {
    char <- input_mtrx[i, j]
    if (!is_single_digit(char)) next
    start_col <- if ((j - 1) %in% cols) j - 1 else j
    end_col <- if ((j + 1) %in% cols) j + 1 else j
    start_row <- if ((i - 1) %in% rows) i - 1 else i
    end_row <- if ((i + 1) %in% rows) i + 1 else i
    neighbors <- input_mtrx[start_row:end_row, start_col:end_col]
    if (is_symbol_and_not_dot(neighbors) |> any()) {
      adjacent_to_symbol[i, j] <- 1
    }
  }
}

# Check for consecutive numbers in the input matrix. If any of them has a 1 in
# the `adjacent_to_symbol`, then those numbers consecutive to it should also
# have a 1:
while (TRUE) {
  updated <- FALSE
  for (i in rows) {
    for (j in cols) {
      cond <- ((j + 1) %in% cols) &&
        (adjacent_to_symbol[i, j] == 1) &&
        is_single_digit(input_mtrx[i, j + 1]) &&
        adjacent_to_symbol[i, j + 1] != 1
      if (cond) {
        adjacent_to_symbol[i, j + 1] <- 1
        updated <- TRUE
      }
      cond2 <- ((j - 1) %in% cols) &&
        (adjacent_to_symbol[i, j] == 1) &&
        is_single_digit(input_mtrx[i, j - 1]) &&
        adjacent_to_symbol[i, j - 1] != 1
      if (cond2) {
        adjacent_to_symbol[i, j - 1] <- 1
        updated <- TRUE
      }
    }
  }
  if (!updated) break
}

# set all 0's to dots:
input_mtrx[adjacent_to_symbol == 0] <- "."

# now we only have the part numbers and dots:
rows |>
  sapply(\(i) input_mtrx[i, ] |> paste(collapse = "")) |>
  strsplit(split = ".", fixed = TRUE) |>
  unlist() |>
  Filter(f = nzchar) |>
  as.numeric() |>
  sum()

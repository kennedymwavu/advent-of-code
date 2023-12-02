input <- readLines(con = "input.txt")

# part 1----
get_color_counts <- \(input, color) {
  pattern <- sprintf("(\\d+)(?=\\s%s)", color)
  input |>
    stringr::str_extract_all(pattern = pattern) |>
    lapply(as.numeric)
}

n_reds <- get_color_counts(input, "red")
n_greens <- get_color_counts(input, "green")
n_blues <- get_color_counts(input, "blue")

is_possible <- \(n_red, n_green, n_blue) {
  all(n_red <= 12) && all(n_green <= 13) && all(n_blue <= 14)
}

inds <- Map(f = is_possible, n_reds, n_greens, n_blues) |> unlist()

seq_along(input)[inds] |> sum()

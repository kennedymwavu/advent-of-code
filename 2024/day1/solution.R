# part 1:
input <- read.table(
  file = "input.txt",
  col.names = c("group1", "group2"),
  colClasses = "integer"
) |>
  Map(f = sort)

dist <- abs(input$group1 - input$group2) |> sum()
dist

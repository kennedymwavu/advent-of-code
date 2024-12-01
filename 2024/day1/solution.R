# part 1:
input <- read.table(
  file = "input.txt",
  col.names = c("group1", "group2"),
  colClasses = "integer"
) |>
  Map(f = sort)

dist <- abs(input$group1 - input$group2) |> sum()
dist

# part 2:
g1 <- input$group1[input$group1 %in% input$group2] |> unique()
g2 <- input$group2[input$group2 %in% g1]
n <- sapply(g1, \(i) sum(i == g2))
sum(g1 * n)

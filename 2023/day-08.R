input <- readLines("2023/data/example.txt")
input <- readLines("2023/data/input-08.txt")

# Process input
steps <- input[1]
input_df <- input[3:length(input)] |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("node", "value"), delim = " = ") |> 
  tidyr::separate_wider_delim(value, names = c("L", "R"), delim = ", ") |> 
  dplyr::mutate(dplyr::across(c(L, R), ~stringr::str_remove(.x, "\\(|\\)")))
all_steps <- rep(stringr::str_split_1(steps, ""), 10000*ceiling(nrow(input_df) / nchar(steps)))

# Part 1
i <- 1
current_node <- "AAA"
while (current_node != "ZZZ") {
  to_step <- all_steps[i]
  current_node <- input_df |> 
    dplyr::filter(node == current_node) |> 
    dplyr::select(all_of(to_step)) |> 
    dplyr::pull(to_step)
  i = i + 1
}
i - 1

# Part 2
i <- 1
current_nodes <- input_df |> 
  dplyr::mutate(ends = substr(node, nchar(node), nchar(node))) |> 
  dplyr::filter(ends == "A") |> 
  dplyr::pull(node)
last_chars <- function(x) {
  purrr::map_vec(.x = x, .f = ~substr(.x, nchar(.x), nchar(.x)))
}

while (!all(last_chars(current_nodes) == "Z")) {
  to_step <- all_steps[i]
  for (j in 1:length(current_nodes)) {
    current_nodes[j] <- input_df |> 
      dplyr::filter(node == current_nodes[j]) |> 
      dplyr::select(all_of(to_step)) |> 
      dplyr::pull(to_step)
  }
  i = i + 1
}
i - 1



input <- readLines("2023/data/input-08.txt")

# Process input
steps <- input[1]
input_df <- input[3:length(input)] |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("node", "value"), delim = " = ") |> 
  tidyr::separate_wider_delim(value, names = c("L", "R"), delim = ", ") |> 
  dplyr::mutate(dplyr::across(c(L, R), ~stringr::str_remove(.x, "\\(|\\)")))
all_steps <- rep(stringr::str_split_1(steps, ""), 10000*ceiling(nrow(input_df) / nchar(steps)))


# Part  -------------------------------------------------------------------

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


# Part 2 ------------------------------------------------------------------

last_chars <- function(x) {
  purrr::map_vec(.x = x, .f = ~substr(.x, nchar(.x), nchar(.x)))
}

# do each A independently
start_nodes <- input_df |> 
  dplyr::mutate(ends = substr(node, nchar(node), nchar(node))) |> 
  dplyr::filter(ends == "A") |> 
  dplyr::pull(node)

num_steps <- numeric(length = length(start_nodes))
for (s in 1:length(start_nodes)) {
  i <- 1
  current_node <- start_nodes[s]
  while (last_chars(current_node) != "Z") {
    to_step <- all_steps[i]
    current_node <- input_df |> 
      dplyr::filter(node == current_node) |> 
      dplyr::select(all_of(to_step)) |> 
      dplyr::pull(to_step)
    i = i + 1
  }
  num_steps[s] <- i - 1
  print(s)
}

# find lowest common multiple
total_steps <- DescTools::LCM(num_steps)
format(total_steps, scientific = FALSE)

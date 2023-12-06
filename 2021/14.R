input <- readLines("2021/Data/input-14.txt")


# Part 1 ------------------------------------------------------------------

# Process data
input_df <- input |> 
  tibble::as_tibble() |> 
  dplyr::slice_tail(n = -2) |> 
  tidyr::separate_wider_delim(value, names = c("pair", "insert"), delim = " -> ")

# Function to insert pair
insert_pair <- function(start_poly) {
  start_poly <- stringr::str_split_1(start_poly, pattern = "")
  new_poly <- character(length = length(start_poly) - 1)
  for (n in 1:(length(start_poly) - 1)) {
    pair_n <- stringr::str_flatten(start_poly[c(n, n + 1)])
    to_insert <- input_df$insert[which(input_df$pair == pair_n)]
    if (n == 1) {
      inserted <- stringr::str_flatten(c(start_poly[n], to_insert, start_poly[n + 1]))
    } else {
      inserted <- stringr::str_flatten(c(to_insert, start_poly[n + 1]))
    }
    new_poly[n] <- inserted
  }
  return(stringr::str_flatten(new_poly))
}

# Iterate
start_poly <- input[1]
num_iter <- 10
for (i in 1:num_iter) {
  start_poly <- insert_pair(start_poly)
  print(i)
}
poly_count <- sort(table(stringr::str_split_1(start_poly, pattern = "")))
tail(poly_count, 1) - poly_count[1]

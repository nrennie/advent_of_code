input <- readLines("2023/data/input-05.txt")

# Process data
map_starts <- which(stringr::str_detect(input, " map:"))
map1 <- input[(map_starts[1]+1):(map_starts[2]-2)]
map2 <- input[(map_starts[2]+1):(map_starts[3]-2)]
map3 <- input[(map_starts[3]+1):(map_starts[4]-2)]
map4 <- input[(map_starts[4]+1):(map_starts[5]-2)]
map5 <- input[(map_starts[5]+1):(map_starts[6]-2)]
map6 <- input[(map_starts[6]+1):(map_starts[7]-2)]
map7 <- input[(map_starts[7]+1):length(input)]


# Part 1 ------------------------------------------------------------------

seeds <- input[1] |> 
  stringr::str_remove("seeds: ") |> 
  stringr::str_split_1(" ") |> 
  as.numeric()

make_transform <- function(map, s) {
  x1 <- map |> 
    stringr::str_split(" ") |> 
    purrr::map(.f = ~tibble::tibble(
      x_in = as.numeric(.x[2]),
      x_out = as.numeric(.x[1]),
      x_len = as.numeric(.x[3]))) |> 
    dplyr::bind_rows() |> 
    dplyr::filter(x_in <= s, (x_in + x_len) >= s)
  if (nrow(x1)) {
    x <- tibble::tibble(x_out = s + (x1$x_out[1] - x1$x_in[1]),
                        x_in = s)
  } else {
    x <- tibble::tibble(x_out = s, x_in = s)
  }
  return(x)
}

transform_seed <- function(s) {
  # round 1
  x_lookup <- make_transform(map1, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  # round 2
  x_lookup <- make_transform(map2, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  # round 3
  x_lookup <- make_transform(map3, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  # round 4
  x_lookup <- make_transform(map4, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  # round 5
  x_lookup <- make_transform(map5, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  # round 6
  x_lookup <- make_transform(map6, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  # round 7
  x_lookup <- make_transform(map7, s)
  s = x_lookup$x_out[which(x_lookup$x_in == s)]
  return(s)
}

new_seeds <- numeric(length = length(seeds))
for (i in 1:length(seeds)) {
  new_seeds[i] <- transform_seed(seeds[i])
}
min(new_seeds)

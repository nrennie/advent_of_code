input <- readLines("2023/data/input-06.txt")
input <- readLines("2023/data/example.txt")

# process inputs
times <- stringr::str_remove(input[1], "Time: ") |> 
  stringr::str_split_1(" ") |> 
  as.numeric() |> 
  purrr::discard(is.na)
dists <- stringr::str_remove(input[2], "Distance: ") |> 
  stringr::str_split_1(" ") |> 
  as.numeric() |> 
  purrr::discard(is.na)

# Part 1

# Wins if s*(times[i] - s) > dists[i]
# Wins if s*(times[i] - s) - dists[i] > 0
# How many integer values of s satisfy this?
wins <- numeric(length = length(times))
for (i in 1:length(wins)) {
  upper <- 0.5 * (times[i] + sqrt(times[i]^2 - 4*dists[i]))
  lower <- 0.5 * (times[i] - sqrt(times[i]^2 - 4*dists[i]))
  wins[i] <- length(seq(ceiling(lower + 0.00001), floor(upper - 0.00001)))
}
prod(wins)


# Part 2
t <- as.numeric(stringr::str_flatten(times))
d <- as.numeric(stringr::str_flatten(dists))
upper <- 0.5 * (t + sqrt(t^2 - 4*d))
lower <- 0.5 * (t - sqrt(t^2 - 4*d))
length(seq(ceiling(lower + 0.00001), floor(upper - 0.00001)))






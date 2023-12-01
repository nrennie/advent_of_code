input <- readLines("2023/data/input-01.txt")

# Part 1
input |> 
  stringr::str_extract_all(pattern = "[:digit:]") |> 
  purrr::map(.f = ~ paste0(.x[1], .x[length(.x)])) |> 
  unlist() |> 
  as.numeric() |> 
  sum()

# Part 2
digits_df <- data.frame(
  d = as.character(1:9),
  name = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
)
for (i in 1:length(input)) {
  s <- input[i]
  pattern <- "(?=(one|two|three|four|five|six|seven|eight|nine))"
  res <- gregexpr(pattern, s, perl=TRUE)
  starts <- attr(res[[1]],'capture.start') 
  lengths <- attr(res[[1]],'capture.length')
  ends <- starts + lengths - 1
  df_pos <- do.call(rbind, Map(data.frame, start=starts, end=ends))
  if (nrow(df_pos) == 1) {
    if (df_pos[1,1] >= 0) {
      x = substring(input[i], df_pos[1,1], df_pos[1,2])
      substring(input[i], df_pos[1,1], df_pos[1,2]) <- digits_df[digits_df$name == x, ]$d
    }
  } else if (nrow(df_pos) >= 2) {
    df_pos2 <- rbind(head(df_pos, 1), tail(df_pos, 1))
    if (df_pos2[1,1] >= 0) {
      x = substring(input[i], df_pos2[1,1], df_pos2[1,2])
      substring(input[i], df_pos2[1,1], df_pos2[1,2]) <- digits_df[digits_df$name == x, ]$d
    }
    if (df_pos2[2,1] >= 0) {
      x = substring(input[i], df_pos2[2,1], df_pos2[2,2])
      substring(input[i], df_pos2[2,1], df_pos2[2,2]) <- digits_df[digits_df$name == x, ]$d
    }
  }
}
input |> 
  stringr::str_extract_all(pattern = "[:digit:]") |> 
  purrr::map(.f = ~ paste0(.x[1], .x[length(.x)])) |> 
  unlist() |> 
  as.numeric() |> 
  sum()
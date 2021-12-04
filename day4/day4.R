# --- Day 4: Giant Squid ---

# library("tidyverse")

# Load data ---------------------------------------------------------------

test <- read_lines(here::here("day4", "test.txt"))

format_draws <- function(element){
  out <- as.numeric(strsplit(element, ",")[[1]])
  return(out)
}
draws <- format_draws(test[[1]])


board_mat <- function(vec){
  dim <- length(vec)^0.5 # assume square
  matrix(vec, byrow = TRUE, ncol = dim, nrow = dim)
}

format_boards <- function(vec){
  x <- vec != ""      # which are board elements
  n <- sum(vec == "") # number of boards
  
  all <- vec[x] |>
    paste0(collapse = " ") |>
    strsplit("\\s+") |>
    {\(x) x[[1]]}() # base equivalent of %>% `[[(1)` see shorturl.at/cktO7

  out <- split(all, cut(seq_along(all), n, labels = FALSE)) |>
    lapply(board_mat)
  
  return(out)
}

boards <- format_boards(test[-1])

# Part 1 ------------------------------------------------------------------



# Scratch -----------------------------------------------------------------

pp <- gsub("^$", "break", jj)
sum(pp == "break") # number of boards
sum(pp != "break")/sum(pp == "break") # number rows per board (we know it's 5)
split(pp, f = pp == "break")

separate(tibble(pp), pp, "break", into = "a")


# --- Day 4: Giant Squid ---

# library("tidyverse")

# Data functions ---------------------------------------------------------------
#' Format raw data for draws
#'
#' @param string single element string separated by commas
#'
#' @return vector of numbers representing draws
format_draws <- function(string){
  out <- as.numeric(strsplit(string, ",")[[1]])
  return(out)
}

#' Format raw board vectors into matrices
#'
#' @param vec numeric vector of board values
#'
#' @return numeric matrix representing the board
board_mat <- function(vec){
  dim <- length(vec)^0.5 # assumes square
  matrix(vec, byrow = TRUE, ncol = dim, nrow = dim)
}

#' Format raw data for boards
#'
#' @param vec character vector of raw data for boards, separated by blanks
#'
#' @return numeric list of matrices representing boards
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


# Part 1 ------------------------------------------------------------------
# Format input data
test <- readLines(here::here("day4", "test.txt"))
draws <- format_draws(test[[1]])
boards <- format_boards(test[-1])



# Scratch -----------------------------------------------------------------

pp <- gsub("^$", "break", jj)
sum(pp == "break") # number of boards
sum(pp != "break")/sum(pp == "break") # number rows per board (we know it's 5)
split(pp, f = pp == "break")

separate(tibble(pp), pp, "break", into = "a")


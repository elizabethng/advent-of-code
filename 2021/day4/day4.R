# --- Day 4: Giant Squid ---

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
  out <- as.numeric(vec) |>
    matrix(byrow = TRUE, ncol = dim, nrow = dim)
  return(out)
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
    {\(x) x[[1]]}() # base equivalent of %>% `[[`(1) see shorturl.at/cktO7

  out <- split(all, cut(seq_along(all), n, labels = FALSE)) |>
    lapply(board_mat)
  
  return(out)
}


# Part 1 ------------------------------------------------------------------
# Which board wins in the smallest number of steps?

#' Play bingo on one board
#'
#' @param board numeric matrix
#' @param draws numeric vector
#'
#' @return list with number of draws and product of last number and unmarked
playbingo <- function(board, draws){
  n <- nrow(board)
  i <- n - 1 # can't win until 5 draws
  win <- FALSE

  while(win == FALSE){
    i <- i + 1
    marked <- matrix(board %in% draws[1:i], byrow = FALSE, nrow = n)
    win <- any(c(colSums(marked) == n,rowSums(marked) == n))
  }
  
  return(list(
    n_draws = i,
    product = draws[i]*sum(board[!marked])
  ))
}


#' Which has least number of draws?
#'
#' @param l list of output from playbingo()
#'
#' @return numeric element corresponding to product output from lowest number of draws
get_winner <- function(l){
  sapply(l, `[[`, "n_draws") |>
    {\(x) which(x == min(x))}() |>
    {\(x) l[[x]]$product}()
}


test_part_one <- function(){
  test <- readLines(here::here("day4", "test.txt"))
  
  draws <- format_draws(test[[1]])
  boards <- format_boards(test[-1])
  # playbingo(boards[[3]], draws)
  
  gameresults <- lapply(boards, playbingo, draws = draws)
  get_winner(gameresults)
}
test_part_one() == 4512

do_part_one <- function(){
  input <- readLines(here::here("day4", "input.txt"))
  
  draws <- format_draws(input[[1]])
  boards <- format_boards(input[-1])

  gameresults <- lapply(boards, playbingo, draws = draws)
  get_winner(gameresults)
}
do_part_one() # 8442


# Part 2 ------------------------------------------------------------------
#' Which has most number of draws?
#'
#' @param l list of output from playbingo()
#'
#' @return numeric element corresponding to product output from highest number of draws
get_loser <- function(l){
  sapply(l, `[[`, "n_draws") |>
    {\(x) which(x == max(x))}() |>
    {\(x) l[[x]]$product}()
}

test_part_two <- function(){
  test <- readLines(here::here("day4", "test.txt"))
  
  draws <- format_draws(test[[1]])
  boards <- format_boards(test[-1])
  
  gameresults <- lapply(boards, playbingo, draws = draws)
  get_loser(gameresults)
}
test_part_two() == 1924

do_part_two <- function(){
  input <- readLines(here::here("day4", "input.txt"))
  
  draws <- format_draws(input[[1]])
  boards <- format_boards(input[-1])
  
  gameresults <- lapply(boards, playbingo, draws = draws)
  get_loser(gameresults)
}
do_part_two() # 4590

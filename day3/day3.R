# --- Day 3: Binary Diagnostic ---
# https://adventofcode.com/2021/day/3

library("tidyverse")

# Load data ---------------------------------------------------------------
testdat <- readr::read_lines(here::here("day3", "test.txt"))
inputdat <- readr::read_lines(here::here("day3", "input.txt"))

# gamma rate = most common bit in position
# epsilon rate = opposite, least common bit
t1_gamma_binary <- c(1, 0, 1, 1, 0)
t1_gamma_decimal <- 22
t1_epsilon_binary <- c(0, 1, 0, 0, 1)
t1_epsilon_decimal <- 9
t1_result_final <- 198


# Part 1 ------------------------------------------------------------------
# Mode if more than half observations
# All are 1 or zero
binmode <- function(vec){
  l <- length(vec)/2
  s <- sum(vec)
  
  if(s == l){
    stop("Equal number of 1 and 0")
  } else if(s > l){
    return(1)
  } else {
    return(0)
  }
}

# Binary-decimal converter
bindec <- function(vec){
  cev <- rev(vec) # reverse to start with power 0
  dec <- 0        # counter
  
  for(i in 1:length(cev)){
    dec <- dec + cev[i]*2^(i - 1)
  }
  
  return(dec)
}
bindec(t1_gamma_binary) == t1_gamma_decimal
bindec(t1_epsilon_binary) == t1_epsilon_decimal

# Cheater cheater tidyverse
tidyway <- function(vec){
  len <- nchar(vec[1])
  gamma <- tibble(raw = vec) %>%
    separate(raw, sep = 1:len, into = letters[1:len]) %>%
    mutate_all(as.numeric) %>%
    summarize_all(binmode) %>%
    as.matrix()
  epsilon <- -(gamma - 1)
  
  gamma_dec <- bindec(gamma)
  epsilon_dec <- bindec(epsilon)
  
  res <- gamma_dec*epsilon_dec
  
  return(res)
}
tidyway(testdat) == t1_result_final
tidyway(inputdat)



# Other stuff -------------------------------------------------------------
# Rearrange data column-wise?
as.matrix(paste0(testdat, collapse = ""), 
            byrow = TRUE, 
            nrow = length(testdat),
            ncol = nchar(testdat[1]))
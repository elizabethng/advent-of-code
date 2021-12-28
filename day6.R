# Day 6

library("tidyverse")

# each lantern fish creates a new lantern fish every 7 days
# not synchronized, track each fish as # days till spawn
# new fish need 2 extra days (9 to first spawn)
# timer runs from 6 to 0 (COUNTS DOWN)

# How many fish after 80 days?

# Matrix population model
# Possible "ages" are 8:0

# Given vector of initial ages,
# daily growth is defined as
# n_8_{t+1} = n_0_t
# n_7_{t+1} = n_8_t
# n_6_{t+1} = n_7_t + n_0_t
# n_5_{t+1} = n_6_t
# ...
# n_0_{t+1} = n_1_t

A <- matrix(c(
# 8  7  6  5  4  3  2  1  0
  0, 1, 0, 0, 0, 0, 0, 0, 0, # 8 
  0, 0, 1, 0, 0, 0, 0, 0, 0, # 7
  0, 0, 0, 1, 0, 0, 0, 0, 0, # 6
  0, 0, 0, 0, 1, 0, 0, 0, 0, # 5
  0, 0, 0, 0, 0, 1, 0, 0, 0, # 4
  0, 0, 0, 0, 0, 0, 1, 0, 0, # 3
  0, 0, 0, 0, 0, 0, 0, 1, 0, # 2
  0, 0, 0, 0, 0, 0, 0, 0, 1, # 1
  1, 0, 1, 0, 0, 0, 0, 0, 0  # 0
  ), byrow = TRUE, nrow = 9, ncol = 9
)

# Convert list of ages into vector
freq_fun <- function(vec){
  all_ages <- tibble(age = 0:8)
  counts <- tibble(age = vec) %>%
    group_by(age) %>%
    tally() %>%
    full_join(all_ages, by = "age") %>%
    replace_na(list(n = 0)) %>%
    arrange(desc(age))
  out <- pull(counts, n)
  names(out) <- pull(counts, age)
  
  return(out)
}

# Project population a given number of steps
grow <- function(nvec, A, steps){
  n <- nvec
  for(i in 1:steps){
    n <- n%*%A 
  }
  return(n)
}

# Test data
if(FALSE){
  vec <- c(3,4,3,1,2)
  n0 <- freq_fun(vec)
  n0%*%A 
  grow(n0, A, 1)
  (n0%*%A)%*%A
  grow(n0, A, 2)
  
  # After 18 days, 26 fish
  grow(n0, A, 18) %>% sum()
  # After 80 days, 5934 fish
  grow(n0, A, 80) %>% sum()
}



# Part 1 answer -----------------------------------------------------------
dat <- scan("day6_dat.txt", sep = ",")
n0 <- freq_fun(dat)
grow(n0, A, 80) %>% sum() # 352,195 fish

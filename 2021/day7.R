# Day 7
library("tidyverse")

# crabs move horizontally
# 1 step costs 1 fuel
# determine cheapest position to line up at

# Check for whole range of values
move_crabs <- function(vec){
  res <- tibble(position = min(vec):max(vec)) %>%
    mutate(fuel = map_dbl(position, ~ sum(abs(vec - .x))))
  out <- slice_min(res, order_by = fuel)
  return(out)
}

if(FALSE){
  test <- c(16,1,2,0,4,2,7,1,2,14)
  abs(test - 1) %>% sum() # cost of fuel to move to 1
  abs(test - 2) %>% sum() # cost of fuel to move to 2
  move_crabs(test)
}

dat <- scan("day7_dat.txt", sep = ",")
res <- move_crabs(dat) # 345197 fuel


# Part 2
move_crabs2 <- function(vec){
  tmp <- expand_grid(
    dat = list(vec),
    position = min(vec):max(vec)
  ) %>%
    rowid_to_column("id") %>%
    unnest(dat) %>%
    mutate(moves = abs(dat - position)) %>%
    mutate(cost = map_dbl(moves, ~ sum(1:.x))) %>%
    mutate(cost = ifelse(moves == 0, cost - 1, cost))
  # correction for 0:1
  
  res <- tmp %>%
    group_by(id) %>%
    summarize(
      cost = sum(cost),
      .groups = "drop")
  
  out <- slice_min(res, order_by = cost)
  
  return(out)
}

if(FALSE){
  test <- c(16,1,2,0,4,2,7,1,2,14)
  move_crabs2(test) # 168
}

res2 <- move_crabs2(dat) # 96361608 # too high!
                         # 96361606 # correct!
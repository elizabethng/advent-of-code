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
res <- move_crabs(dat) # 345,197 fuel

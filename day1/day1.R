# https://adventofcode.com/2021/day/1
# Count the number of times the depth measurement increases

library("here")
library("tidyverse")

raw <- read_lines(here("day1", "input.txt"))
dat <- sapply(raw, as.numeric)

# For an increase, the next number should be larger
# No comparison for the last value

# Vectorized
sum(dat[-length(dat)] < dat[-1])

# Using a counter
n_inc <- 0 # initialize counter
for(i in 1:(length(dat)-1)){  # check if next value is larger
  if(dat[i] < dat[i+1]){
    n_inc <- n_inc + 1
  }
  print(n_inc)
}



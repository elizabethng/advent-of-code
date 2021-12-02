# https://adventofcode.com/2021/day/1
# Count the number of times the depth measurement increases

library("here")
library("tidyverse")


# Part 1 ------------------------------------------------------------------
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
}
n_inc

# Wrap in a function
count_increases <- function(d){
  n <- sum(d[-length(d)] < d[-1])
  return(n)
}
count_increases(dat)

# Part 2 ------------------------------------------------------------------
# Consider sums of a three-measurement sliding window
# Count the number of times the sum of measurements in the
# sliding window increases
# Stop when there aren't enough to create a three-measurement window

# put values into an array
mdat <- matrix(
  c(dat[1:(length(dat) - 2)], 
    dat[2:(length(dat) - 1)],
    dat[3:length(dat)]),
  byrow = FALSE,
  ncol = 3
)
msum <- apply(mdat, 1, sum)
count_increases(msum)

# For loop
s_inc <- 0 # initialize counter
for(i in 1:(length(dat)-3)){  # check if next value is larger
  s1 <- dat[i] + dat[i+1] + dat[i+2]
  s2 <- dat[i+1] + dat[i+2] + dat[i+3]
  
  if(s1 < s2){ s_inc <- s_inc + 1 }
}
s_inc

# Tidyverse
tdat <- tibble(x1 = dat) %>%
  mutate(
    x2 = lead(x1),
    x3 = lead(x2)
  ) %>%
  mutate(sum = x1 + x2 + x3) %>%
  mutate(next_sum = lead(sum)) %>%
  mutate(check = next_sum > sum)

sum(tdat$check, na.rm = TRUE)
group_by(tdat, check) %>% tally()

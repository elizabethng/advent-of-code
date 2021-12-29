# Day 8 Part 1

library("tidyverse")

# Same wire connections are used within each entry, but
# not necessarily between them

# the unique signal patterns correspond to the 
# ten different ways the submarine tries to render a 
# digit using the current wire/segment connections

# "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
# 10 unique signals, | delimiter, 4 digit output value

# only a 7 uses three letters, so dab corresponds to 7
# only a 4 uses four letters, so eafb corresponds to 4
# find which signal wires correspond to each of 10 digits
# then, decode the output value

# 1, 4, 7, 8 use unique number of segments

# In the output values, how many times do 1, 4, 7, or 8 appear?
key <- tibble(
  number = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  segments = c(6, 2, 5, 5, 4, 5, 6, 3, 7, 6)
)

dup_segments <- key$segments[duplicated(key$segments)]
unique_key <- filter(key, !(segments %in% dup_segments))


procdat <- function(raw){
  dat <- tibble(raw) %>%
    separate(col = 1, 
             into = c(paste0("s", 1:10), 
                      paste0("o", 1:4)))
  return(dat)
}
get1478 <- function(dat){
  res <- select(dat, starts_with("o")) %>%
    pivot_longer(everything()) %>% 
    mutate(length = nchar(value)) %>%
    filter(length %in% c(2, 4, 3, 7))
  out <- nrow(res)
  return(out)
}

# Test
if(FALSE){
  raw <- read_lines("day8_test.txt")
  dat <- tibble(raw) %>%
    separate(col = 1, 
             into = c(paste0("s", 1:10), paste0("o", 1:4)))
  
  out <- select(dat, starts_with("o")) %>%
    pivot_longer(everything()) %>% 
    mutate(length = nchar(value)) %>%
    filter(length %in% c(2, 4, 3, 7))
  nrow(out)
}
if(FALSE){
  read_lines("day8_test.txt") %>%
    procdat() %>%
    get1478()
}

# Part 1
read_lines("day8_dat.txt") %>%
  procdat() %>%
  get1478() # 362


# Day 8 Part 2 -- Brute force approach

library("tidyverse")


# 00. Preliminaries -------------------------------------------------------
# slots
#  1111
# 2    3
# 2    3
#  4444
# 5    6
# 5    6
#  7777

# Create a key for all numbers
# If a slot/segment is empty, put 0
# row is number, column is segment
key <- matrix(c(
  1, 1, 1, 0, 1, 1, 1, # 0
  0, 0, 1, 0, 0, 1, 0, # 1
  1, 0, 1, 1, 1, 0, 1, # 2
  1, 0, 1, 1, 0, 1, 1, # 3
  0, 1, 1, 1, 0, 1, 0, # 4
  1, 1, 0, 1, 0, 1, 1, # 5
  1, 1, 0, 1, 1, 1, 1, # 6
  1, 0, 1, 0, 0, 1, 0, # 7
  1, 1, 1, 1, 1, 1, 1, # 8
  1, 1, 1, 1, 0, 1, 1),# 9
  byrow = TRUE, 
  nrow = 10, ncol = 7) %>%
  `==`(1)

# Use key and a permutation list to get codes for each number
# pvec is one permutation vector (character)
# key is the T/F matrix representation of each number
# Returns a vector of codes
get_codes <- function(pvec, key){
  L <- nrow(key)
  res <- rep(NA, L)
  
  for(i in 1:L){
    res[i] <- pvec[key[i,]] %>% paste0(collapse = "")
  }
  
  out <- res
  # out <- matrix(res, ncol = L, nrow = 1)
  # colnames(out) <- 0:9
  
  return(out)
}

# Alphabetize components of a string element
alphabetize <- function(e){
  out <- strsplit(e, "") %>%
    unlist() %>%
    sort() %>%
    paste0(collapse = "")
  
  return(out)
}
# Vectorized version of alphabetize
alphabetize_vec <- function(vec){
  out <- vec %>%
    matrix(ncol = 1) %>%
    apply(1, alphabetize)
  
  return(out)
}

# Find whether a list of codes is a match for a message
# codes is the list of all codes corresponding to one configuration
# message is the list of values to identify
# returns TRUE if all codes match, otherwise FALSE
find_match <- function(codes, message){
  all(message %in% codes)
}

# Function to decode a message, given all permutations
# dat is a list of two elements, first is vector of encoded values
# and second is vector of elements to decode
# Returns a number encoded in the message
decode <- function(dat, allcodes_abc){
  # 1. alphabetize
  dat_abc <- alphabetize_vec(dat[[1]])
  code_abc <- alphabetize_vec(dat[[2]])
  
  # 2. look up all codes
  ck <- apply(allcodes_abc, 1, find_match, message = dat_abc)
  
  # 3. Get the decoded numbers
  nm <- bind_cols(number = 0:9, code = allcodes_abc[ck, ]) 
  
  # 4. Decode the message
  out <- tibble(code = code_abc) %>%
    left_join(nm, by = "code") %>%
    pull(number) %>%
    paste0(collapse = "") %>%
    as.numeric()
  
  return(out)
}


# 01.  Read in data -------------------------------------------------------
raw <- read_lines("day8_dat.txt")

# Split into key and code
dat <- str_split(raw, " \\| ") %>%
  lapply(str_split, pattern = " ")

# 02.  Get results --------------------------------------------------------
# Get all possible configurations
perms <- gtools::permutations(7, 7, letters[1:7])
allcodes <- t(apply(perms, 1, get_codes, key = key)) # dim swap
allcodes_abc <- apply(allcodes, c(1,2), alphabetize)

# Do for all
allres <- lapply(dat, decode, allcodes_abc = allcodes_abc)

# Sum to retrieve answer
sapply(allres, `*`, 1) %>% sum() # 1020159



# Test data ---------------------------------------------------------------
if(FALSE){
  raw <- read_lines("day8_test.txt")
  dat <- str_split(raw, " \\| ") %>%
    lapply(str_split, pattern = " ")
  
  # Do for all
  allres <- lapply(dat, decode, allcodes_abc = allcodes_abc)
  
  # Sum to retrieve answer
  sapply(allres, `*`, 1) %>% sum() # 61229 
}


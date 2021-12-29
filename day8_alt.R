# Brute force approach

library("tidyverse")

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

# Get all possible configurations
perms <- gtools::permutations(7, 7, letters[1:7])

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
# Example
if(FALSE){
  get_codes(perms[1,], key)
  
  # do for a bunch of perms
  permsub <- perms[1:15,] 
}

# Alphabetize components of a string element
alphabetize <- function(e){
  out <- strsplit(e, "") %>%
    unlist() %>%
    sort() %>%
    paste0(collapse = "")
  
  return(out)
}
alphabetize_vec <- function(vec){
  out <- vec %>%
    matrix(ncol = 1) %>%
    apply(1, alphabetize)
  
  return(out)
}

# don't know why dimensions have changed after apply...
allcodes <- t(apply(perms, 1, get_codes, key = key))
allcodes_abc <- apply(allcodes, c(1,2), alphabetize)

# Now I have a list of all possible combinations and 
# their corresponding number (column). 
# Need to look up in each list

if(FALSE){
  # Example 1 from first row of data
  ex1 <- matrix(c("acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", 
                  "cefabd", "cdfgeb", "eafb", "cagedb", "ab"),
                ncol = 1)
  
  # Put in alphabetical order
  ex1_abc <- apply(ex1, 1, alphabetize)
  
  # Now need to search through allcodes and find matches
  # Need one where they all match
  ex1_abc %in% allcodes[1,] # do for first row
  ex1_abc %in% allcodes[2,] # do for second row
  # deafgbc is true, should match row 2537
  ex1_abc %in% allcodes[2537,]
  ex1_abc %in% allcodes_abc[2537,] # now works!
  
  perms %>%
    data.frame() %>%
    tibble() %>%
    rowid_to_column() %>%
    filter(X1 == "d", X2 == "e", X3 == "a",
           X4 == "f", X5 == "g", X6 == "b", X7 == "c")
  
  
  # NOTE: better test might be for example shown, i.e., for 
  #       first test case where they are all in order
  myex <- matrix(c("abcefg", "cf", "acdeg", "acdfg", "bcdf",
                   "abdfg", "abdefg", "abcdefg", "abcdfg"),
                 ncol = 1)
  myex_abc <- apply(myex, 1, alphabetize)
  # should match #1
  all(myex %in% allcodes[1,]) # do for first row
  
  # What if missing some of the values?
  myex2 <- myex[1:5]
  # should match #1
  all(myex2 %in% allcodes[1,]) # works!
  
}

# Find whether a list of codes is a match for a message
# codes is the list of all codes corresponding to one configuration
# message is the list of values to identify
# returns TRUE if all codes match, otherwise FALSE
find_match <- function(codes, message){
  all(message %in% codes)
}

find_match(codes = allcodes[1,], message = ex1_abc)
find_match(codes = allcodes[1,], message = myex)

# Do for all codes with a single message
ck <- apply(allcodes_abc, 1, find_match, message = ex1_abc)

# Now that I've identified the correct sequence, need to 
# use the numbers to decode!!

# apply back to key to get numbers
# wrap in function decode()
jj <- bind_cols(number = 0:9, code = allcodes_abc[ck, ]) 

tibble(code = ex1_abc) %>%
  left_join(jj, by = "code")
# Split LHS (numbers) and RHS (nums to sum)
# probably just want to use alphabetized everything

# Do for one
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


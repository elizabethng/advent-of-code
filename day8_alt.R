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

# pvec is one permutation vector
# key is the T/F matrix representation
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

get_codes(perms[1,], key)

# do for a bunch of perms
permsub <- perms[1:15,]

# don't know why dimensions have changed
allcodes <- t(apply(perms, 1, get_codes, key = key))
allcodes_abc <- apply(allcodes, c(1,2), alphabetize)

# Now I have a list of all possible combinations and 
# their corresponding number (column). 
# Need to look up in each list
# Tempted to make everything all tibbles now?

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

# codes is the list of all codes corresponding to one configuration
# message is the list of values to identify
check_fun <- function(codes, message){
  all(message %in% codes)
}

check_fun(codes = allcodes[1,], message = ex1_abc)
check_fun(codes = allcodes[1,], message = myex)

# Do for all
ck <- apply(allcodes_abc, 1, check_fun, message = ex1_abc)
index <- which(ck == TRUE)

# Now that I've identified the correct sequence, need to 
# use the numbers to decode!!

# apply back to key to get numbers
# wrap in function decode()
jj <- bind_cols(number = 0:9, 
          code = allcodes[index, ]) %>%
  rowwise() %>%
  mutate(code = alphabetize(code))

tibble(code = ex1_abc) %>%
  left_join(jj, by = "code")
# Split LHS (numbers) and RHS (nums to sum)
# probably just want to use alphabetized everything

# Scratch -----------------------------------------------------------------


# tibble seems like an expensive approach,
# but maybe will work?
library("tidyverse")
colnames(perms) <- paste0("s", 1:7)
pp <- perms %>%
  data.frame() %>%
  tibble()

colnames(key) <- paste0("s", 1:7)
rownames(key) <- c(0:9)
kk <- key %>%
  data.frame() %>%
  rownames_to_column("n") %>%
  tibble() 

# Now try for one
# Think about writing a function to generate the list
# of letters for each number representation
# arranged in alphabetical order
pp[1,]
kk

pp[1, kk[1,2:8] == 1] 


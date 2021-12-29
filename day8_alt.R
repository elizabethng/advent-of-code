# Brute force approach

# slots
#  1111
# 2    3
# 2    3
#  4444
# 5    6
# 5    6
#  7777

# If slot is empty, put 0
# 1  2  3  4  5  6  7
zero <- c(1, 1, 1, 0, 1, 1, 1) # 0
one <- c(0, 0, 1, 0, 0, 1, 0) # 1
c(1, 0, 1, 1, 1, 0, 1) # 2
c(1, 0, 1, 1, 0, 1, 1) # 3
c(0, 1, 1, 1, 0, 1, 0) # 4
c(1, 1, 0, 1, 0, 1, 1) # 5
c(1, 1, 0, 1, 1, 1, 1) # 6
c(1, 0, 1, 0, 0, 1, 0) # 7
c(1, 1, 1, 1, 1, 1, 1) # 8
c(1, 1, 1, 1, 0, 1, 1) # 9


# All permutations of letters[1:7]
jj <- gtools::permutations(7, 7, letters[1:7])

tfz <- zero == 1
tfo <- one == 1
jj[1,][tfz]
jj[1,][tfo]

# Key
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
  1, 1, 1, 1, 0, 1, 1  # 9
), byrow = TRUE, nrow = 10, ncol = 7)

perms <- gtools::permutations(7, 7, letters[1:7])

# Get one combo for all
tfkey <- key == 1

# Expand to same dimensions as key
jj <- matrix(rep(perms[1,], 10),
             byrow = TRUE, nrow = 10, ncol = 7)
# result will be a ragged array...
# can't use a matrix anymore
jj[tfkey]

# Do for one
jj[1,]
tfkey[1,]

# If I collapse then the dimensions will be same
jj[1, tfkey[1,]] %>% paste0(collapse = "")
jj[1, tfkey[2,]] %>% paste0(collapse = "")
jj[1, tfkey[3,]] %>% paste0(collapse = "")

# pvec is one permutation vector
# key is the T/F matrix representation
function(pvec, key){
  L <- nrow(key)
  out <- rep(NA, L)
  for(i in 1:L){
    out[i] <- pvec[key[i,]] %>% paste0(collapse = "")
  }
}



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


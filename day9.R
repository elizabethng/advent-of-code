# Day 9

library("tidyverse")


# Preliminaries ------------------------------------------------------
proc_data <- function(raw){
  tmp <- strsplit(raw, "") 
  
  r <- length(raw)
  c <- length(tmp[[1]])
    
  out <- unlist(tmp) %>%
    as.numeric() %>%
    matrix(byrow = TRUE, nrow = r, ncol = c)
  
  return(out)
}

# Helper functions to handle out of bounds errors
index <- function(A, i, j){ A[i, j] }
index_s <- possibly(index, NULL)

# Returns a logical matrix of same size as A
# where TRUE if the element is a local minima
get_minima <- function(A){
  r <- nrow(A)
  c <- ncol(A)
  O <- matrix(NA, nrow = r, ncol = c)
  
  for(i in 1:r){
    for(j in 1:c){
      val <- A[i,j]
      
      tp <- index_s(A = A, i = (i - 1), j = j)
      bt <- index_s(A = A, i = (i + 1), j = j)
      lh <- index_s(A = A, i = i, j = (j - 1))
      rh <- index_s(A = A, i = i, j = (j + 1))
      
      out <- all(c(tp, bt, lh, rh) > val)
      O[i,j] <- out
    }
  }
  return(O)
}



# Part 1 ------------------------------------------------------------------
# Test case
if(FALSE){
  raw <- read_lines("day9_test.txt")
  A <- proc_data(raw)  
  res <- get_minima(A)
  sum(A[res] + 1) # 15
}

dat <- read_lines("day9_input.txt")
A <- proc_data(dat)  
res <- get_minima(A)
sum(A[res] + 1) # 486


# Part 2 ------------------------------------------------------------------
A <- read_lines("day9_test.txt") %>% proc_data() 
minima <- get_minima(A)

(A == 9) %>%
  apply(c(1,2), as.numeric)
# apply(res, c(1,2), as.numeric)

# Get the locations of the minima and start searching
# around each one. Search ends when 9s are encountered (or NULL)

# Do for 1
M <- which(minima == TRUE, arr.ind = TRUE)
ij <- M[1,]


# Turn TRUE to 0 to initialize output array
zero_fun <- function(e){
  ifelse(e, NA, e)
}

R <- (A != 9) %>%
  # apply(c(1,2), as.numeric) %>%
  apply(c(1,2), zero_fun)


# Start at the min
i = ij[1]
j = ij[2]

# Local minima is included
R[i, j] <- TRUE

# Look around
tp <- index_s(A = R, i = (i - 1), j = j)
bt <- index_s(A = R, i = (i + 1), j = j)
lh <- index_s(A = R, i = i, j = (j - 1))
rh <- index_s(A = R, i = i, j = (j + 1))

# If it's FALSE, stop, that's a 9
# If it's NA, make it TRUE and then pick up there
# It length == 0, skip because those are edges

# Could write this as a checking function
# Iterate through each direction for now?
# function(vl, i, j, M)

update_map <- function(R, val, i, j){
  if(length(val) == 0){
    print("encountered edge") 
  } else if(is.na(val)) {
    R[i, j] <- TRUE
  } else if(val == FALSE){
    print("encountered 9")
  } else {
    "error"
  }
  return(R)
}

update_map(R, tp, i = (i - 1), j = j) %>%
update_map(bt, i = (i + 1), j = j) %>%
update_map(lh, i = i, j = (j - 1)) %>%
update_map(rh, i = i, j = (j + 1))

# But then need indices of any that were good
# so search can continue there
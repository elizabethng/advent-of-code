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

# Output array
# 9s are represented by FALSE, everything else is NA
R <- apply((A != 9), c(1,2), zero_fun)

# Start at the min
i = ij[1]
j = ij[2]

# Local minima is included
R[i, j] <- TRUE

# Get coordinates to check around
c(i-1, i+1, i, i)
c(j, j, j-1, j+1)
c(tp, bt, lh, rh)

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
# I know from lh above that it's going to turn into
# a TRUE, so those are the coordinates I'll want to use
# at next step. So need to store those as well.
# Perhaps add the NA coordinates to a list to check next?
# Let's just add them to a running list of values to check
# But remember that it needs to be done around each mimima

# Outline
# 0. Initialize the output array & get minima
# 1. Get the first minima
# 2. Check values at all four points
# 3. Make a list of the coordinates that are NA
# 4. Update the matrix
# 5. Take the first value in the list of coordinates
#    and use that as the value in step 1, repeat
# 6. When the list of coordinates is empty, get the
#    indices that are TRUE, this is output for first
#    minima. 
# 7. Make all the TRUE values FALSE
# 8. Select the next minima value and go to step 2

# 0. Initialize output array
M <- which(minima == TRUE, arr.ind = TRUE)
R <- apply((A != 9), c(1,2), zero_fun)
minima <- get_minima(A)

# 1. Get first minima
ij <- M[1,]
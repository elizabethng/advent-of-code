
library("tidyverse")

# High level -- may need to recode the output matrix
# 2 = included in basin
# 0 = not evaluated yet
# 1 = ridge (i.e., 9)

# Functions ---------------------------------------------------------------
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
index <- function(A, i, j){ 
  out <- A[i, j] 
  if(length(out) == 0) out <- NA
  return(out)
  }
index_s <- possibly(index, NA)

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
      
      out <- all(c(tp, bt, lh, rh) > val, na.rm = TRUE)
      O[i,j] <- out
    }
  }
  return(O)
}

# Turn TRUE to 0 to initialize output array
zero_fun <- function(e){
  ifelse(e, NA, e)
}

# Check if a cell value is an edge, part of a basin,
# or part of a ridge (i.e., a 9) and return an updated
# matrix (filling TRUE for basin cell) and the coordinates
# of a cell if it is determined to be part of a basin
update_map <- function(R, val, i, j){
  coords <- NA
  if(is.na(val)){
    print("encountered edge") 
  } else if(val == 0) {
    R[i, j] <- 2   # update array
    coords <- c("row" = unname(i), "col" = unname(j))  # return coordinates
  } else if(val == 1){
    print("encountered ridge")
  } else {
    "error"
  }
  return(list(R = R, coords = coords))
}


# Outline -----------------------------------------------------------------
# 0. Initialize the output array & get minima
# 1. Get the first minima & set to 2
# 2. Check values at all four points
# 3. Make a list of the coordinates that are 0 (basin)
# 4. Update the matrix to mark basin cells with 2
# 5. Take the first value in the list of coordinates
#    and use that as the value in step 1, repeat
# 6. When the list of coordinates is empty, get the
#    indices that are TRUE, this is output for first
#    minima. 
# 7. Make all the TRUE values FALSE
# 8. Select the next minima value and go to step 2


# 0. Initialize output array & get minima ---------------------------------
A <- read_lines("day9_test.txt") %>% proc_data() 
minima <- get_minima(A)
M <- which(minima == TRUE, arr.ind = TRUE) # minima locations
# R <- apply((A != 9), c(1,2), zero_fun)     # output array
R <- apply((A == 9), c(1, 2), as.numeric) # ridges := 1

# Initialize vector for storing points to evaluate
# Cleaner alternative (potentially) store in list
new_points <- list()
# matrix(NA, ncol = 2, dimnames = list(c(1),c("row", "col")))

## For the first basin - may need to initialize separately
## There will be one for each minima
# 1. Get first minima location & set TRUE
ij <- M[2,]
i <- ij[1]
j <- ij[2]
R[i, j] <- 2 # set that value as TRUE in output array

# 2. Check values at all four points
# clockwise order is top, right, bottom, left
P <- matrix(c(
  (i - 1), j,
  i, (j + 1),
  (i + 1), j,
  i, (j - 1)),
  byrow = TRUE, ncol = 2,
  dimnames = list(
    c("top", "rhs", "bot", "lhs"),
    c("row", "col"))) 

# Maybe use for loop or apply here instead
# Updated to include NA for edge cells
val_vec <- c(
  "top" = index_s(A = R, i = P[1,1], j = P[1,2]),
  "rhs" = index_s(A = R, i = P[2,1], j = P[2,2]),
  "bot" = index_s(A = R, i = P[3,1], j = P[3,2]),
  "lhs" = index_s(A = R, i = P[4,1], j = P[4,2])
)

# 3. Add 0 coordinates to list of coords
# 4. Update matrix, only need to do for 0s
# Actually, could just use this as next starting point?
# Then how to remove once done? Just advance index using while loop
new_points <- rbind(new_points, 
                    P[(val_vec) == 0 & (!is.na(val_vec)), ])


# Need to run through one whole loop to get nonzero new_points
# length before I can run the while loop
ij <- unlist(new_points[3,]) # harmless to inc, fixes list issue
i <- ij[1]
j <- ij[2]
R[i, j] <- 2 # set that value as TRUE in output array

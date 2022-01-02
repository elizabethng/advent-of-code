
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
# zero_fun <- function(e){
#   ifelse(e, NA, e)
# }

#' Function to update results matrix
#'
#' @param coords 1x2 numeric matrix with row index and column index
#' @param resmat results matrix to be updated
#' @param pointlist running list of new points to check (i.e, basin points)
#'
#' @return updated results matrix and point list
update_map <- function(coords, resmat, pointlist){
  # 1. Get cell and mark as basin
  ij <- unlist(coords)
  i <- ij[1]
  j <- ij[2]
  resmat[i, j] <- 2 
  
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
  val_vec <- c(
    "top" = index_s(A = resmat, i = P[1,1], j = P[1,2]),
    "rhs" = index_s(A = resmat, i = P[2,1], j = P[2,2]),
    "bot" = index_s(A = resmat, i = P[3,1], j = P[3,2]),
    "lhs" = index_s(A = resmat, i = P[4,1], j = P[4,2])
  )
  
  # 3. Add 0 coordinates to list of coords
  pointlist <- rbind(pointlist, 
                     P[(val_vec) == 0 & (!is.na(val_vec)), ])
  
  return(
    list(resmat, pointlist)
  )
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
R <- apply((A == 9), c(1, 2), as.numeric) # ridges := 1

# Now wrap in function and apply to each row of M to get a list
# of results for each basin
# ACTUALLY only need the size of each basin so add that as final
# step

get_basin <- function(coords, resmat){
  # Initialize storage for points to evaluate
  new_points <- list()
  
  first <- update_map(coords = coords, resmat = R, pointlist = new_points)
  
  counter <- 1
  R <- first[[1]]
  new_points <- first[[2]]
  
  while(counter <= nrow(new_points)){
    tmp <- update_map(
      coords = new_points[counter, ], 
      resmat = R, 
      pointlist = new_points)
    
    R <- tmp[[1]]
    new_points <- tmp[[2]]
    counter <- counter + 1
  }
  
  result <- sum(R == 2)
  
  return(result)
}

apply(M, 1, get_basin, resmat = R)


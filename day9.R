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
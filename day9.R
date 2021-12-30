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
top <- function(A, i, j){ A[i - 1, j] }
bot <- function(A, i, j){ A[i + 1, j] }
lhs <- function(A, i, j){ A[i, j - 1] }
rhs <- function(A, i, j){ A[i, j + 1] }

top_s <- possibly(top, NULL)
bot_s <- possibly(bot, NULL)
lhs_s <- possibly(lhs, NULL)
rhs_s <- possibly(rhs, NULL)


# Returns a logical matrix of same size as A
# where TRUE if the element is a local minima
get_minima <- function(A){
  r <- nrow(A)
  c <- ncol(A)
  O <- matrix(NA, nrow = r, ncol = c)
  
  for(i in 1:r){
    for(j in 1:c){
      val <- A[i,j]
      
      tp <- top_s(A = A, i = i, j = j)
      bt <- bot_s(A = A, i = i, j = j)
      lh <- lhs_s(A = A, i = i, j = j)
      rh <- rhs_s(A = A, i = i, j = j)
      
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

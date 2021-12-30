# Day 9

library("tidyverse")

raw <- read_lines("day9_test.txt")

proc_data <- function(raw){
  tmp <- strsplit(raw, "") 
  
  r <- length(raw)
  c <- length(tmp[[1]])
    
  out <- unlist(tmp) %>%
    as.numeric() %>%
    matrix(byrow = TRUE, nrow = r, ncol = c)
  
  return(out)
}

proc_data(raw)  



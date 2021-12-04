# --- Day 3: Binary Diagnostic ---
# https://adventofcode.com/2021/day/3

library("tidyverse")
run_test <- TRUE


# Part 1 ------------------------------------------------------------------
# Mode if more than half observations
# All are 1 or zero
binmode <- function(vec, type = "power"){
  if(!(type %in% c("power", "life_support"))){
    stop("Type must be one of 'power' or 'life_support'")
  }
  
  l <- length(vec)/2
  s <- sum(vec)
  
  if(s == l){
    if(type == "power") {
      stop("Equal number of 1 and 0")
    } else if(type == "life_support") {
      return(1)
    }
  } else if(s > l) {
    return(1)
  } else {
    return(0)
  }
}

# Binary-decimal converter
bindec <- function(vec){
  cev <- rev(vec) # reverse to start with power 0
  dec <- 0        # counter
  
  for(i in 1:length(cev)){
    dec <- dec + cev[i]*2^(i - 1)
  }
  
  return(dec)
}

# Cheater cheater tidyverse
tidyway <- function(vec){
  len <- nchar(vec[1])
  gamma <- tibble(raw = vec) %>%
    separate(raw, sep = 1:len, into = letters[1:len]) %>%
    mutate_all(as.numeric) %>%
    summarize_all(binmode) %>%
    as.matrix()
  epsilon <- -(gamma - 1)
  
  gamma_dec <- bindec(gamma)
  epsilon_dec <- bindec(epsilon)
  
  res <- gamma_dec*epsilon_dec
  
  return(res)
}



# Part 2 ------------------------------------------------------------------
# Life support rating
# oxygen generator rating : corresponds to gamma
# times CO2 scrubber rating : corresponds to epsilon
# while loop for length

# Oxygen generator
get_oxygen <- function(vec){
  vecset <- vec
  l <- length(vecset)
  i <- 1
  while(l > 1){
    bit <- substring(vecset, i, i)
    mode <- binmode(as.numeric(bit), type = "life_support")

    vecset <- vecset[bit == mode]
    l <- length(vecset)
    i <- i + 1
  }
  result <- as.numeric(strsplit(vecset, "")[[1]])
  return(result)
}

# Carbon dioxide scrubber
get_carbondioxide <- function(vec){
  vecset <- vec
  l <- length(vecset)
  i <- 1
  while(l > 1){
    bit <- substring(vecset, i, i)
    mode <- -(binmode(as.numeric(bit), type = "life_support") - 1)
    vecset <- vecset[bit == mode]
    l <- length(vecset)
    i <- i + 1
  }
  result <- as.numeric(strsplit(vecset, "")[[1]])
  return(result)
}



# Test --------------------------------------------------------------------
if(run_test == TRUE){
  testdat <- readr::read_lines(here::here("day3", "test.txt"))
  
  # gamma rate = most common bit in position
  # epsilon rate = opposite, least common bit
  t1_gamma_binary <- c(1, 0, 1, 1, 0)
  t1_gamma_decimal <- 22
  t1_epsilon_binary <- c(0, 1, 0, 0, 1)
  t1_epsilon_decimal <- 9
  t1_result_final <- 198
  
  # Mode function
  print(binmode(c(0,1,1,1,0)) == 1)
  print(binmode(c(0,1), type = "life_support") == 1)
  print(-(binmode(c(0,1), type = "life_support") - 1) == 0)
  # binmode(c(0,1), type = "x") 
  
  # Converter function
  print(bindec(t1_gamma_binary) == t1_gamma_decimal)
  print(bindec(t1_epsilon_binary) == t1_epsilon_decimal)
  
  # Power consumption function
  print(tidyway(testdat) == t1_result_final)
  
  # Oxygen generator
  print(all(get_oxygen(testdat) == c(1,0,1,1,1)))
  
  # Carbon dioxide scrubber
  print(all(get_carbondioxide(testdat) == c(0,1,0,1,0)))
  
  # Life support combined
  print(bindec(get_oxygen(testdat))*bindec(get_carbondioxide(testdat)) == 230)
  
}




# Results -----------------------------------------------------------------
inputdat <- readr::read_lines(here::here("day3", "input.txt"))

# Part 1
tidyway(inputdat) # 1540244

# Part 2
o2 <- bindec(get_oxygen(inputdat))
co2 <- bindec(get_carbondioxide(inputdat))
o2*co2 # 4203981


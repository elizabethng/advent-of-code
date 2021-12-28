# --- Day 5: Hydrothermal Venture ---

# Each line is a line segment, x1,y1 -> x2,y2
# Top left corner is 0,0; bottom right corner is 9,9

library("tidyverse")


#  Part 1 --------------------------------------------------------------
# Consider only the horizontal and vertical lines
# At how many points do at least two lines overlap?
# i.e., how many intersections

proc_dat <- function(raw){
  out <- tibble(raw) %>%
    rownames_to_column("step") %>%
    separate(raw, into = c("from", "to"), sep = " -> ") %>%
    pivot_longer(c(from, to)) %>%
    separate(value, into = c("x", "y")) %>%
    pivot_wider(names_from = name, values_from = c(x, y)) %>%
    mutate_all(as.numeric) %>%
    mutate(
      dy = y_to - y_from,
      dx = (x_to - x_from)
    ) %>%
    mutate(
      slope = case_when(
        dy == 0 & abs(dx) > 0 ~ "horizontal",
        dx == 0 & abs(dy) > 0 ~ "vertical",
        TRUE                  ~ "other"
      )
    ) %>%
    filter(slope != "other") %>%
    mutate(d = max(x_from, x_to, y_from, y_to))
  return(out)
}
draw_path <- function(x_from, x_to, y_from, y_to, d){
  # reindex
  x_from <- x_from + 1
  x_to <- x_to + 1
  y_from <- y_from + 1
  y_to <- y_to + 1
  d <- d +1
  
  m <- matrix(0, nrow = d, ncol = d)
  
  if(x_from == x_to){
    for(y in y_from:y_to){
      m[x_to, y] <- 1
    }
  } else if(y_from == y_to){
    for(x in x_from:x_to){
      m[x, y_to] <- 1
    }
  }
  
  return(m)
}
count_cells <- function(dat){
  tmp <- mutate(dat, 
                m = pmap(list(x_from, x_to, y_from, y_to, d), 
                         draw_path))
  
  res <- tmp$m[[1]]
  for(i in 2:nrow(tmp)){
    res <- res + tmp$m[[i]]
  }
  
  # Get more than 2 overlap cells
  out <- sum(res > 1)
  
  return(out)
}


if(FALSE){
  test <- read_lines(here::here("day5", "test.txt")) 
  testdat <- proc_dat(test)
  ggplot(testdat, aes(group = step, color = as.factor(step))) +
    geom_segment(aes(x = x_from, xend = x_to,
                     y = y_from, yend = y_to))
  count_cells(testdat) # 5
}

read_lines(here::here("day5", "input.txt")) %>%
  proc_dat() %>%
  count_cells() # 5169


# Part 2 ------------------------------------------------------------------
# Now need to include diagonals, which are all 45 degrees

proc_dat2 <- function(raw){
  out <- tibble(raw) %>%
    rownames_to_column("step") %>%
    separate(raw, into = c("from", "to"), sep = " -> ") %>%
    pivot_longer(c(from, to)) %>%
    separate(value, into = c("x", "y")) %>%
    pivot_wider(names_from = name, values_from = c(x, y)) %>%
    mutate_all(as.numeric) %>%
    mutate(
      dy = y_to - y_from,
      dx = (x_to - x_from)
    ) %>%
    mutate(
      slope = case_when(
        dy == 0 & abs(dx) > 0 ~ "horizontal",
        dx == 0 & abs(dy) > 0 ~ "vertical",
        TRUE                  ~ "other"
      )
    ) %>%
    mutate(d = max(x_from, x_to, y_from, y_to))
  return(out)
}
draw_path2 <- function(x_from, x_to, y_from, y_to, d){
  # reindex
  x_from <- x_from + 1
  x_to <- x_to + 1
  y_from <- y_from + 1
  y_to <- y_to + 1
  d <- d +1
  
  m <- matrix(0, nrow = d, ncol = d)
  
  if(x_from == x_to){
    for(y in y_from:y_to){
      m[x_to, y] <- 1
    }
  } else if(y_from == y_to){
    for(x in x_from:x_to){
      m[x, y_to] <- 1
    }
  } else {
    x <- x_from:x_to
    y <- y_from:y_to
    
    for(i in 1:length(x)){
      m[x[i], y[i]] <- 1
    }
  }
  
  return(m)
}
count_cells2 <- function(dat){
  tmp <- mutate(dat, 
                m = pmap(list(x_from, x_to, y_from, y_to, d), 
                         draw_path2))
  
  res <- tmp$m[[1]]
  for(i in 2:nrow(tmp)){
    res <- res + tmp$m[[i]]
  }
  
  # Get more than 2 overlap cells
  out <- sum(res > 1)
  
  return(out)
}

if(FALSE){
  test <- read_lines(here::here("day5", "test.txt")) 
  testdat <- proc_dat2(test)
  testres <- count_cells2(testdat) # 12
  
}

read_lines(here::here("day5", "input.txt")) %>%
  proc_dat2() %>%
  count_cells2() # 22083

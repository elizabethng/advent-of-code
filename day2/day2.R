# --- Day 2: Dive! ---
library("here")
library("tidyverse")

# 0. Instructions ------------------------------------------------------------
# Now, you need to figure out how to pilot this thing.
# 
# It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:
#   
# forward X increases the horizontal position by X units.
#*backward X decreases the horizontal position by X units.
# down X increases the depth by X units.
# up X decreases the depth by X units.
# 
# Note that since you're on a submarine, down and up affect your depth, and so they have the opposite result of what you might expect.
# 
# The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's going. For example:
#   
# forward 5
# down 5
# forward 8
# up 3
# down 8
# forward 2
# 
# Your horizontal position and depth both start at 0. The steps above would then modify them as follows:
#   
# forward 5 adds 5 to your horizontal position, a total of 5.
# down 5 adds 5 to your depth, resulting in a value of 5.
# forward 8 adds 8 to your horizontal position, a total of 13.
# up 3 decreases your depth by 3, resulting in a value of 2.
# down 8 adds 8 to your depth, resulting in a value of 10.
# forward 2 adds 2 to your horizontal position, a total of 15.
# 
# After following these instructions, you would have a horizontal position of 15 and a depth of 10. (Multiplying these together produces 150.)
# 
# Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?


# 1.  Read input ----------------------------------------------------------
# raw <- RCurl::getURL("https://adventofcode.com/2021/day/2/input")
# 400 error was because not logged in

raw <- read_lines(here("day2", "input.txt"))

# 02. Tidyverse approach --------------------------------------------------
# First idea, split into direction and count
tidycalcs <- function(vec, plot = FALSE){
  calcs <- tibble(raw = vec) %>%
    separate(raw, into = c("direction", "distance")) %>%
    mutate(
      distance = as.numeric(distance),
      axis = case_when(
        direction %in% c("forward", "backward") ~ "horizontal",
        direction %in% c("up", "down") ~ "depth"
      ),
      sign = case_when(
        direction %in% c("forward", "down") ~ 1,
        direction %in% c("backward", "up") ~ -1
      ),
      movement = distance*sign
    )

  if(plot == TRUE){
    # Make a plot
    pdat <- select(calcs, axis, movement) %>%
      rownames_to_column("step") %>%
      mutate(step = as.numeric(step)) %>%
      pivot_wider(names_from = axis, 
                  values_from = movement,
                  values_fill = 0) %>%
      bind_rows(tibble(step = 0, horizontal = 0, depth = 0), .) %>%
      mutate(x = cumsum(horizontal), y = cumsum(depth))
    
    check <- with(pdat[nrow(pdat),], prod(x, y))
    
    p <- ggplot(pdat, aes(x = x, y = y, color = step)) +
      geom_point() +
      scale_y_reverse() +
      xlab("Horizontal") +
      ylab("Depth") +
      scale_color_continuous("Step") +
      theme_bw()
  }
  
  result <- group_by(calcs, axis) %>% 
    summarise(total = sum(movement), .groups = "drop")
  
  out <- prod(result$total)
  
  if(plot == TRUE){
    if(check != out) stop("Results do not match")
    return(list(value = out, plot = p))
  } else {
    return(out)
  }
}

tidycalcs(raw)
tidycalcs(raw, plot = TRUE)$plot

# 03. Looping approach -------------------------------------------------

loopcalcs <- function(vec){
  tmp <- str_split(vec, " ", simplify = TRUE)
  
  x <- 0
  y <- 0
  
  for(i in 1:nrow(tmp)){
    if(tmp[i, 1] == "forward") x <- x + as.numeric(tmp[i, 2])
    if(tmp[i, 1] == "backward") x <- x - as.numeric(tmp[i, 2])
    if(tmp[i, 1] == "up") y <- y - as.numeric(tmp[i, 2])
    if(tmp[i, 1] == "down") y <- y + as.numeric(tmp[i, 2])
  }
  
  return(x*y)
}

loopcalcs(raw)


# 04. Part 1 Answer -------------------------------------------------------
loopcalcs(raw) == tidycalcs(raw)


# 05. Instructions Part 2 -------------------------------------------------
# Based on your calculations, the planned course doesn't seem to make any sense. 
# You find the submarine manual and discover that the process is actually slightly
# more complicated.
# 
# In addition to horizontal position and depth, you'll also need to track a 
# third value, aim, which also starts at 0. 
# The commands also mean something entirely different than you first thought:
#   
# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
#   It increases your depth by your aim multiplied by X.
# 
# Again note that since you're on a submarine, down and up do the opposite 
# of what you might expect: "down" means aiming in the positive direction.
# 
# Now, the above example does something different:
# 
#     forward 5 adds 5 to your horizontal position, a total of 5. 
#     Because your aim is 0, your depth does not change.
#
#     down 5 adds 5 to your aim, resulting in a value of 5.
# 
#     forward 8 adds 8 to your horizontal position, a total of 13. 
#     Because your aim is 5, your depth increases by 8*5=40.
#
#     up 3 decreases your aim by 3, resulting in a value of 2.
# 
#     down 8 adds 8 to your aim, resulting in a value of 10.
# 
#     forward 2 adds 2 to your horizontal position, a total of 15. 
#     Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
# 
# After following these new instructions, you would have a horizontal position of 15 and a depth of 60. (Multiplying these produces 900.)
# 
# Using this new interpretation of the commands, calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?


looptwo <- function(vec){
    tmp <- str_split(vec, " ", simplify = TRUE)
    cmd <- tmp[,1]              # command
    x <- as.numeric(tmp[,2])  # value
    
    h <- 0 # horizontal
    d <- 0 # depth
    a <- 0 # aim
    
    for(i in 1:nrow(tmp)){
      if(cmd[i] == "forward"){
        h <- h + x[i]
        d <- d + a*x[i]
      }
      if(cmd[i] == "backward"){
        h <- h - x[i]
      }
      if(cmd[i] == "up"){
        a <- a - x[i]
      }
      if(cmd[i] == "down"){
        a <- a + x[i]
      }
    }
    print(c(h, d, a))
    return(h*d)
  }

testvec <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
test2res <- 900

looptwo(testvec) == test2res

looptwo(raw)

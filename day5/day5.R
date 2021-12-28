# --- Day 5: Hydrothermal Venture ---

# Each line is a line segment, x1,y1 -> x2,y2
# Top left corner is 0,0; bottom right corner is 9,9

library("tidyverse")
library("sf")


#  Part 1 --------------------------------------------------------------
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
    ) 
  return(out)
}
make_spatial <- function(dat){
  out <- dat %>%
    select(-dx, -dy) %>%
    pivot_longer(
      c(x_from, x_to, y_from, y_to),
      names_to = c("coord", "type"),
      names_sep = "_",
    ) %>%
    pivot_wider(names_from = coord, values_from = value) %>%
    sf::st_as_sf(coords = c("x","y")) %>%
    select(-type) %>%
    group_by(step) %>% 
    summarize(
      slope = unique(slope),
      .groups = "drop"
    ) %>% 
    st_cast("LINESTRING")
  return(out)
}
get_overlaps <- function(spat){
  dat <- spat %>%
    st_intersection() %>%
    mutate(
      cl = map(geometry, class),
      cl = map_chr(cl, 2)
    ) %>%
    filter(cl == "POINT") %>%
    select(geometry) %>%
    distinct()
  
  out <- nrow(dat)
  
  return(out)
}

# Test
if(FALSE){
  test <- read_lines(here::here("day5", "test.txt")) 
  testdat <- proc_dat(test)
  ggplot(testdat, aes(group = step, color = slope)) +
    geom_segment(aes(x = x_from, xend = x_to,
                     y = y_from, yend = y_to))
  testspat <- make_spatial(testdat)
  plot(testspat)
  get_overlaps(testspat) # 9
}

read_lines(here::here("day5", "input.txt")) %>%
  proc_dat() %>%
  # ggplot(aes(group = step, color = slope)) +
  # geom_segment(aes(x = x_from, xend = x_to,
  #                  y = y_from, yend = y_to))
  make_spatial() %>%
  get_overlaps() # 15,416 

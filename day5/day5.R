# --- Day 5: Hydrothermal Venture ---

# Each line is a line segment, x1,y1 -> x2,y2
# Top left corner is 0,0; bottom right corner is 9,9

library("tidyverse")
library("sf")


#  Load data --------------------------------------------------------------

testdat <- read_lines(here::here("day5", "test.txt")) %>%
  tibble("raw" = .) %>%
  rownames_to_column("step") %>%
  separate(raw, into = c("from", "to"), sep = " -> ") %>%
  pivot_longer(c(from, to)) %>%
  separate(value, into = c("x", "y")) %>%
  pivot_wider(names_from = name, values_from = c(x, y)) %>%
  mutate_all(as.numeric) %>%
  mutate(dy = y_to - y_from,
         dx = (x_to - x_from)) %>%
  mutate(
    slope = case_when(
      dy == 0 & abs(dx) > 0 ~ "horizontal",
      dx == 0 & abs(dy) > 0 ~ "vertical",
      TRUE                  ~ "other"
    )
  ) 

ggplot(testdat, aes(group = step, color = slope)) +
  geom_segment(aes(x = x_from, xend = x_to,
                   y = y_from, yend = y_to))


pp <- testdat %>%
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
  summarize(slope = unique(slope)) %>% 
  st_cast("LINESTRING")

plot(st_geometry(pp))

res <- st_intersects(pp, sparse = FALSE)
res[upper.tri(res)] <- 0
diag(res) <- 0
sum(res)

res2 <- st_intersection(pp)
plot(select(res2, step)) # looks like 9

group_by(res2, step) %>%
  summarize(sum(n.overlaps))

filter(res2, n.overlaps >= 2) # 14, which matches sum above
# Clearly things are getting double-counted

# Can I extract points and then look at unique ones?
# Don't know how to search geometry, but points have two
# length in origins (except line 10 for some reason)
# looked at dput of geometry, looks like I can go by
# class perhaps

jj <- res2 %>%
  mutate(
    ck = map(geometry, class),
    ck = map_chr(ck, 2)
  ) %>%
  filter(ck == "POINT") %>%
  select(geometry) %>%
  distinct()
  
nrow(jj) # number of unique intersection points!

# Scratch -----------------------------------------------------------------
ls <- st_linestring(rbind(c(0,0),c(1,1),c(2,1)))
mls <- st_multilinestring(list(rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))
(sfc <- st_sfc(ls,mls))
st_cast(sfc, "MULTILINESTRING")
st_cast(ls, "LINESTRING")
sf <- st_sf(a = 5:4, geom = sfc)
st_cast(sf, "MULTILINESTRING")


pp <- testdat %>%
  rowwise() %>%
  mutate(
    from = list(c(x_from, y_from)),
    to = list(c(x_to, y_to))
    ) %>%
  mutate(move = list(rbind(from, to))) %>%
  select(step, move, slope)


x <- lapply(pp$move, st_linestring) %>%
  rbind() %>%
  st_sfc()

cbind(pp, x)
st_sfc(id = 1:10, geometry = x)

ck <- st_multilinestring(pp$move) %>%
  st_cast("MULTILINESTRING")
plot(ck)
st_geometry(ck) %>% plot()

cbind(ck, pp)
s = st_sf(a = select(pp, -move), geometry = ck)

# Part 1 ------------------------------------------------------------------

# At how many points do at least two lines overlap?
# i.e., how many intersections

chart_path <- function()

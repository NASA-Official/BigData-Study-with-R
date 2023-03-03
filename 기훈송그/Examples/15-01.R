library(dplyr)
library(tibble)

sportLeague <- tibble(
  sport = c("Hotkey", "Baseball", "Football"),
  league = c("NHL", "MLB", "NFL")
)

trophy <-
  tibble(trophy = c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy"))

trophies1 <- bind_cols(sportLeague, trophy)

trophies1

trophies2 <- tribble(
  ~sport, ~league, ~trophy,
  "Basketball", "NBA", "Larry O'Brien Championship Trophy",
  "Golf", "PGA", "Wanamaker Trophy"
)

trophies <- bind_rows(trophies1, trophies2)

trophies
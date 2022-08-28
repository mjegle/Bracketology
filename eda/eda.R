# METHOD TO THE MADNESS: AN ANALYTICAL APPROACH TO BRACKETOLOGY #

install.packages("wehoop")
install.packages("hoopR")

library(wehoop)
library(tidyverse)
library(hoopR)

tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

wbb_pbp %>%
  head() %>%
  view()

wbb_pbp %>%
  group_by(t)

wbb_schedule <- load_wbb_schedule(2022)

wbb_pbp %>%
  pull(id) %>%
  unique() %>%
  length()

load_wbb_pbp(seasons = 2015)

wbb_schedule %>%
  select(team = home_location, tournament_id) %>%
  bind_rows(wbb_schedule %>% select(team = away_location, tournament_id)) %>%
  distinct() %>%
  filter(tournament_id == 22)

pbp <- load_mbb_pbp(2011)

pbp %>%
  group_by(type_id, type_text) %>%
  count()



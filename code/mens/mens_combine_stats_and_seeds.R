##### COMBINE STATS AND SEEDS ########

library(tidyverse)

stats <- read_csv("../../data/mens/mens_stats.csv")
seeds <- read_csv("../../data/mens/mens_tournament_seeds.csv")

seeds <- seeds %>%
  mutate(School = str_trim(School),
         School = case_when(School == "Arkansas–Little Rock" ~ "Little Rock",
                            School == "Connecticut" ~ "UConn",
                            School == "Loyola" ~ "Loyola (MD)",
                            School == "LIU-Brooklyn" ~ "Long Island University",
                            School == "LIU Brooklyn" ~ "Long Island University",
                            School == "Long Island" ~ "Long Island University",
                            School == "Saint Mary's (CA)" ~ "Saint Mary's",
                            School == "Louisiana-Lafayette" ~ "Louisiana",
                            School == "Massachusetts" ~ "UMass",
                            School == "Mississippi" ~ "Ole Miss",
                            School == "North Carolina State" ~ "NC State",
                            School == "Hawaii" ~ "Hawai'i",
                            School == "Cal State Bakersfield" ~ "CSU Bakersfield",
                            School == "Miami (FL)" ~ "Miami",
                            School == "Cal State Fullerton" ~ "CSU Fullerton",
                            School == "College of Charleston" ~ "Charleston",
                            School == "Penn" ~ "Pennsylvania",
                            School == "Gardner–Webb" ~ "Gardner-Webb", # Weird different kind of dash
                            School == "Texas A&M–Corpus Christi" ~ "Texas A&M-CC",
                            School == "American" ~ "American University",
                            T ~ School))

stats_seeds <- stats %>%
  left_join(seeds, by = c("team" = "School", "year" = "year"))

write_csv(stats_seeds, "../../data//mens/mens_stats_seeds.csv")




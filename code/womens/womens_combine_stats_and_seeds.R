##### COMBINE STATS AND SEEDS ########

library(tidyverse)

stats <- read_csv("../../data/womens/womens_stats.csv")
seeds <- read_csv("../../data/womens/womens_tournament_seeds.csv")

seeds <- seeds %>%
  mutate(School = str_trim(School),
         School = case_when(School == "American" ~ "American University",
                            School == "Arizona St." ~ "Arizona State",
                            School == "Arkansas-Little Rock" ~ "Little Rock",
                            School == "Cal State Northridge" ~ "CSU Northridge",
                            School == "Bethune–Cookman" ~ "Bethune-Cookman",
                            School == "Central Ark." ~ "Central Arkansas",
                            School == "Florida St." ~ "Florida State",
                            School == "Grambling State" ~ "Grambling",
                            School == "Hawaiʻi" ~ "Hawai'i",
                            School == "Hawaii" ~ "Hawai'i",
                            School == "Kansas St." ~ "Kansas State",
                            School == "Long Beach St." ~ "Long Beach State",
                            School == "Miami (FL)" ~ "Miami",
                            School == "Miami (Florida)" ~ "Miami",
                            School == "Michigan St." ~ "Michigan State",
                            School == "Middle Tennessee State" ~ "Middle Tennessee",
                            School == "Mississippi St." ~ "Mississippi State",
                            School == "Montana St." ~ "Montana State",
                            School == "New Mexico St." ~ "New Mexico State",
                            School == "Nicholls State" ~ "Nicholls",
                            School == "North Carolina St." ~ "NC State",
                            School == "Ohio St." ~ "Ohio State",
                            School == "Oregon St." ~ "Oregon State",
                            School == "Penn" ~ "Pennsylvania",
                            School == "Saint Francis (PA)" ~ "St. Francis (PA)",
                            School == "Seattle" ~ "Seattle U",
                            School == "St. Francis Brooklyn" ~ "St. Francis (BKN)",
                            School == "St. Joseph's" ~ "Saint Joseph's",
                            School == "Tennessee-Martin" ~ "UT Martin",
                            School == "UNI" ~ "Northern Iowa",
                            School == "Western Ill." ~ "Western Illinois",
                            School == "Western Ky." ~ "Western Kentucky",
                            School == "Connecticut" ~ "UConn",
                            School == "South Fla." ~ "South Florida",
                            T ~ School))

stats_seeds <- stats %>%
  left_join(seeds, by = c("team" = "School", "year" = "year"))

write_csv(stats_seeds, "../../data//womens/womens_stats_seeds.csv")

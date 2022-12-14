##### Men's results check ######

# Test how many teams the model correctly placed in the field of 68 as of march 1st of the given year
seeds <- read_csv("../../data/mens/mens_tournament_seeds.csv")

for (i in 2011:2022)
{
  if (i != 2020)
  {
    projection <- read_csv(paste0("../../app/mens_final_projection_", i, ".csv"))
    year_seeds <- seeds %>% filter(year == i)
    
    projection <- projection %>% inner_join(year_seeds, by = c("team" = "School", "year" = "year"))
    
    print(nrow(projection))
  }
}
###### SCRAPE WIKIPEDIA NCAA TOURNAMENT SEED LISTS

library(xml2)
library(rvest)
library(tidyverse)

#'
#' Helper method to make the tournament seed scraping more automated
#' @author Michael Egle
#' @param yr the tournament year to scrape
#' @return seed_list the list of the teams that made the tournament that year and their corresponding seed
#' 
scrape_wikipedia_seed_list_table <- function(yr)
{
  # These all follow the same URL structure
  url <- paste0("https://en.wikipedia.org/wiki/", yr, "_NCAA_Division_I_Women%27s_Basketball_Tournament")
  
  html <- read_html(url)
  if (yr %in% c(2013, 2014))
  {
    tables <- html_table(html)[5:8] %>%
      lapply(as.data.frame)
    
    for (i in 1:4)
    {
      t <- tables[[i]]
      colnames(tables[[i]]) <- c("Seed", "School", "Conference", "Record", "Berth type")
      tables[[i]] <- tables[[i]] %>%
        filter(Seed != "Seed")
    }
  }
  else if (yr %in% c(2015:2021))
  {
    tables <- html_table(html)[5:8] %>%
      lapply(as.data.frame)
    
    for (i in 1:4)
    {
      t <- tables[[i]]
      colnames(tables[[i]]) <- c("Seed", "School", "Conference", "Record", "RPI", "Berth type")
      tables[[i]] <- tables[[i]] %>%
        filter(Seed != "Seed")
    }
  } else if (yr %in% c(2011))
  {
    tables <- html_table(html)[6:9] %>%
      lapply(as.data.frame)
    
    for (i in 1:4)
    {
      t <- tables[[i]]
      colnames(tables[[i]]) <- c("Seed", "School", "Conference", "Record", "Berth type")
      tables[[i]] <- tables[[i]] %>%
        filter(Seed != "Seed")
    }
  }
  else if (yr %in% c(2012))
  {
    tables <- html_table(html)[5:8] %>%
      lapply(as.data.frame)
    
    for (i in 1:4)
    {
      t <- tables[[i]]
      #colnames(tables[[i]]) <- t[1,]
      tables[[i]] <- tables[[i]] %>%
        filter(Seed != "Seed")
    }
  }
  else 
  {
    tables <- html_table(html)[7:10] %>%
      lapply(as.data.frame)
  }
  
  # need to remove the asterisks from seed numbers and make it numeric
  for (i in 1:4)
  {
    tables[[i]] <- tables[[i]] %>%
      mutate(Seed = str_remove(Seed, "\\*"),
             Seed = str_remove(Seed, "#"),
             Seed = as.numeric(Seed))
  }
  
  # For sure a more elegant way to do this, I just couldn't find it
  
  seed_list <- tables[[1]] %>%
    bind_rows(tables[[2]]) %>%
    bind_rows(tables[[3]]) %>%
    bind_rows(tables[[4]])
  
  return(seed_list)
  
}

seeds <- data.frame()
# Scrape all the seasons NCAA tournament seeds
for (i in 2014:2022)
{
  if (i != 2020)
  {
    seeds <- seeds %>%
      bind_rows(scrape_wikipedia_seed_list_table(i) %>% select(Seed, School, Berth = `Berth type`) %>% mutate(year = i))
    print(i)
  }
}

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

write_csv(seeds, "../../data/womens/womens_tournament_seeds.csv")


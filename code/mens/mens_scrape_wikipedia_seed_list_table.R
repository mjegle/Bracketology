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
  url <- paste0("https://en.wikipedia.org/wiki/", yr, "_NCAA_Division_I_Men%27s_Basketball_Tournament")
  
  # When using the html_table function, each wikipedia page has a slightly different layout
  # So we have to break it up a little
  html <- read_html(url)
  if (yr %in% c(2014, 2019:2021))
  {
    tables <- html_table(html)[5:8] %>% # take tables 5-8 and make them data frames
      lapply(as.data.frame)
  } else if (yr %in% c(2015, 2016, 2017, 2018))
  {
    tables <- html_table(html)[4:7] %>% # take tables 4-7 and make the m data frames
      lapply(as.data.frame)
  } else if (yr %in% c(2013))
  {
    tables <- html_table(html)[6:9] %>%
      lapply(as.data.frame)
    
    for (i in 1:4)
    {
      t <- tables[[i]]
      colnames(tables[[i]]) <- t[1,]
      tables[[i]] <- tables[[i]] %>%
        filter(Seed != "Seed") # Get rid of games that have a 
    }
  }
  else if (yr %in% c(2011, 2012))
  {
    tables <- html_table(html)[5:8] %>%
      lapply(as.data.frame)
    
    for (i in 1:4)
    {
      t <- tables[[i]]
      colnames(tables[[i]]) <- t[1,]
      tables[[i]] <- tables[[i]] %>%
        filter(Seed != "Seed")
    }
  }
  else 
  {
    tables <- html_table(html)[6:9] %>%
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
for (i in 2011:2022)
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
         School = case_when(School == "Arkansas???Little Rock" ~ "Little Rock",
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
                            School == "Gardner???Webb" ~ "Gardner-Webb", # Weird different kind of dash
                            School == "Texas A&M???Corpus Christi" ~ "Texas A&M-CC",
                            School == "American" ~ "American University",
                            T ~ School))


write_csv(seeds, "../../data/mens/mens_tournament_seeds.csv")


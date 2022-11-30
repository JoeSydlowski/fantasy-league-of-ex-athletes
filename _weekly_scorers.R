# Libraries ---------------------------------------------------------------
library(ffscrapr)
library(tidyverse)

# Functions ---------------------------------------------------------------
get_schedule_history_espn <- 
  function(season) {
    conn <- espn_connect(
      season = season,
      league_id = 805175,
      espn_s2 = Sys.getenv("FLEA_ESPN_S2"),
      swid = Sys.getenv("FLEA_SWID"),
      rate_limit_number = 1000,
      rate_limit_seconds = 60
    )
    
    df <- 
      ff_schedule(conn) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

get_schedule_history_sleeper <- 
  function(season, id) {
    conn <- sleeper_connect(
      season = season,
      league_id = id,
      rate_limit_number = 1000,
      rate_limit_seconds = 60
    )
    
    df <- 
      ff_schedule(conn) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

espn_schedule_history <- 
  tibble(season = c(2015:2020)) %>% 
  mutate(nested_sched = map(season, get_schedule_history_espn)) %>% 
  unnest(cols = nested_sched)

sleeper_schedule_history <- 
  tibble(season = c(2021,2022),
         id = c("739986523688230912","863895883606056960")) %>% 
  mutate(nested_sched = map2(season, id, get_schedule_history_sleeper)) %>% 
  unnest(cols = nested_sched)

schedule_history <- 
  espn_schedule_history %>% 
  bind_rows(sleeper_schedule_history) %>% 
  filter(!is.na(result)) %>% 
  group_by(season, week) %>% 
  mutate(weekly_rank = row_number(-franchise_score),
         user_name = case_when(
           user_name == "AnthonyClay" ~ "Anthony Ossola",
           user_name == "EricBu" ~ "Eric Bulak",
           user_name == "Galletti" ~ "Neal Galletti",
           user_name == "jsydlow99ski" ~ "Jonathan Sydlowski",
           user_name == "JTNoel" ~ "Josh Noel",
           user_name == "LizandroS23" ~ "Lizandro Sanchez",
           user_name == "nickd1212" ~ "Nick Duncan",
           user_name == "rgalletti" ~ "Ryan Galletti",
           user_name == "scottydontknovv" ~ "Scott Wilkens",
           user_name == "sox05syd" ~ "Joe Sydlowski",
           user_name == "terryclark1991" ~ "terry clark",
           user_name == "TyNoel" ~ "Ty Noel",
           TRUE ~ user_name)) %>% 
  ungroup()

weeks <- 
  schedule_history %>% 
  group_by(user_name) %>% 
  summarise(weeks = n(),
            median = median(weekly_rank)) %>% 
  ungroup()

finishes <- 
  schedule_history %>% 
  left_join(weeks, by = "user_name") %>% 
  group_by(weekly_rank, user_name, weeks) %>% 
  summarise(finishes = n()) %>% 
  ungroup() %>% 
  mutate(rate = round(finishes/weeks,3))


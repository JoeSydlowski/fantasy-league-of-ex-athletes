# Libraries ---------------------------------------------------------------
# pak::pak("ffverse/ffscrapr@dev")
library(ffscrapr)
library(tidyverse)
library(gt)
library(RColorBrewer)
library(emoji)

# Functions ---------------------------------------------------------------
get_draft_history_espn <- 
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
      ff_draft(conn) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

get_standings_history_espn <- 
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
      ff_standings(conn) %>% 
      left_join(ff_franchises(conn) %>% select(franchise_id, user_name), by = "franchise_id")
    
    return(df)
  }

.sleeper_history <- function(league_endpoint) {
  history <- tibble::tibble(season = character(),
                            league_id = character())
  history <- history %>% 
    add_row(season = league_endpoint$season,
            league_id = league_endpoint$league_id)
  
  prev <- league_endpoint$previous_league_id
  
  while (!is.null(prev) && prev != "0") {
    prev_endpoint <- glue::glue("league/{prev}") %>%
      sleeper_getendpoint() %>%
      purrr::pluck("content")
    
    history <- history %>% 
      add_row(season = prev_endpoint$season,
              league_id = prev)
    
    prev <- prev_endpoint$previous_league_id
  }
  
  return(history)
}

create_league_id_history <- function(platform,
                                     league_id,
                                     conn){
  
  if (platform == "sleeper") {
    league_endpoint <- glue::glue("league/{conn$league_id}") %>%
      sleeper_getendpoint() %>%
      purrr::pluck("content")
    
    league_id_history <- .sleeper_history(league_endpoint = league_endpoint)
  } else {
    year_range <- ff_league(conn) %>%
      separate(col = years_active,
               into = c("min_year", "max_year"),
               sep = "-")
    
    league_id_history <- crossing(season = as.numeric(year_range$min_year):as.numeric(year_range$max_year),
                                  league_id)
  }
  
  league_id_history <- 
    league_id_history %>% 
    mutate(platform = platform,
           season = as.numeric(season)) %>% 
    arrange(season)
  
  return(league_id_history)
}

get_draft_history_sleeper <- function(league_id,
                              season){
  local_connection <- ffscrapr::ff_connect(
    platform = "sleeper",
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )
  
  df <- ff_draft(local_connection) %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(df)
}

get_standings_history_sleeper <- function(season,
                                          league_id,
                                          platform) {
  
  local_connection <- ffscrapr::ff_connect(
    platform = platform,
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )
  
  df <- ff_standings(local_connection) %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(df)
}


get_sleeper_playoffs <- function(league_id, season){
  
  local_connection <- ffscrapr::ff_connect(
    platform = "sleeper",
    league_id = league_id,
    season = season,
    rate_limit_number = 1000,
    rate_limit_seconds = 60
  )
  
  winners_query <- glue::glue('league/{league_id}/winners_bracket')
  
  sleeper_playoffs <- sleeper_getendpoint(winners_query) %>% 
    purrr::pluck("content") %>% 
    dplyr::bind_rows()
  
  playoffs_long <- 
    sleeper_playoffs %>% 
    pivot_longer(cols = c(w, l),
                 values_to = "franchise_id") %>% 
    mutate(playoff_outcome = case_when(p == 1 & name == "w" ~ 1,
                                       p == 1 & name == "l" ~ 2,
                                       p == 3 & name == "w" ~ 3,
                                       p == 3 & name == "l" ~ 4,
                                       p == 5 & name == "w" ~ 5,
                                       p == 5 & name == "l" ~ 6,
                                       TRUE ~ 99
    ),
    bye_wins = if_else(r == 2 & t1 == franchise_id, 1, 0),
    wins = if_else(name == "w", 1, 0),
    losses = if_else(name == "l", 1, 0))
  
  losers_query <- glue::glue('league/{league_id}/losers_bracket')
  
  sleeper_consolation <- sleeper_getendpoint(losers_query) %>% 
    purrr::pluck("content") %>% 
    dplyr::bind_rows()
  
  consolation_long <- 
    sleeper_consolation %>% 
    pivot_longer(cols = c(w, l),
                 values_to = "franchise_id") %>% 
    mutate(playoff_outcome = case_when(p == 1 & name == "w" ~ 12,
                                       p == 1 & name == "l" ~ 11,
                                       p == 3 & name == "w" ~ 10,
                                       p == 3 & name == "l" ~ 9,
                                       p == 5 & name == "w" ~ 8,
                                       p == 5 & name == "l" ~ 7,
                                       TRUE ~ 99),
           bye_wins = 0,
           wins = 0,
           losses = 0)
  
  playoffs_summary <- playoffs_long %>%
    bind_rows(consolation_long) %>% 
    group_by(franchise_id) %>% 
    summarise(playoff_h2h_wins = sum(wins) + sum(bye_wins),
              playoff_h2h_losses = sum(losses),
              final_place = min(playoff_outcome)) %>% 
    ungroup() %>% 
    left_join(ff_franchises(local_connection) %>%
                select(franchise_id, user_name), by = "franchise_id")
  
  return(playoffs_summary)
  
}

get_headshot <- 
  function(player_id, season){
    
    if(season <= 2020) {
      nflreadr::load_rosters(season) %>% 
        filter(espn_id == player_id) %>% 
        pull(headshot_url)
    } else {
      nflreadr::load_rosters(season) %>% 
        filter(sleeper_id == player_id) %>% 
        pull(headshot_url)
    }
    
  }


# Data Pulls --------------------------------------------------------------
espn_conn <- espn_connect(
  season = 2021,
  league_id = 805175,
  espn_s2 = Sys.getenv("FLEA_ESPN_S2"),
  swid = Sys.getenv("FLEA_SWID"),
  rate_limit_number = 1000,
  rate_limit_seconds = 60
)

sleeper_conn <- 
  sleeper_connect(
    season = 2022,
    league_id = 863895883606056960,
    rate_limit_number = 1000,
    rate_limit_seconds = 60   
  )

league_id_history <- create_league_id_history(platform = "sleeper",
                                              league_id = 863895883606056960,
                                              conn = sleeper_conn)

espn_scoring_history <- 
  ff_scoringhistory(espn_conn, season = 2015:2020) %>%
  filter(week <= 16,
         pos %in% c("QB","RB","WR","TE")) %>% 
  rename(platform_id = espn_id) %>% 
  mutate(platform_id == case_when(player_name == "Rob Gronkowski" ~ "13229",
                                  TRUE ~ platform_id))

sleeper_scoring_history <- 
  ff_scoringhistory(sleeper_conn, season = 2021:2022) %>%
  filter(week <= 17, pos %in% c("QB","RB","WR","TE")) %>% 
  rename(platform_id = sleeper_id)

scoring_history <- 
  espn_scoring_history %>% 
  bind_rows(sleeper_scoring_history) %>% 

  group_by(season, platform_id, pos, gsis_id) %>% 
  summarise(ppg = mean(points),
            games = n()) %>%
  ungroup() %>% 
  group_by(season, pos) %>% 
  mutate(ppg = if_else(games < 4, NA_real_, ppg),
         ppg_rank = row_number(-ppg),
         ppg_rank_value = 1000 * exp(-0.1 * ppg_rank),
         # Week 17 title means 16 games per player
         games_played_percent = if_else(season >= 2021, games/16, games/15),
         games_played_percent = if_else(games_played_percent >= 1, 1, games_played_percent)) %>% 
  ungroup() %>% 
  filter(!is.na(platform_id)) %>% 
  select(season, platform_id, ppg_rank, ppg_rank_value, games, games_played_percent)

# Draft Skill
espn_draft_history <- 
  tibble(season = c(2015:2020)) %>% 
  mutate(nested_draft = map(season, get_draft_history_espn)) %>% 
  unnest(cols = nested_draft)

sleeper_draft_history <- league_id_history %>% 
  mutate(draft_history = map2(.x = league_id,
                              .y = season,
                              .f = get_draft_history_sleeper)) %>% 
  select(-season) %>% 
  unnest(draft_history) %>% 
  group_by(season) %>% 
  mutate(season = as.numeric(season),
         player_id = as.numeric(player_id),
         overall = row_number()) %>% 
  ungroup()

draft_history <- 
  espn_draft_history %>% 
  bind_rows(sleeper_draft_history) %>% 
  group_by(season, pos) %>% 
  mutate(pos_rank = row_number(overall),
         # By position because we switched from 1QB to SF
         pos_rank_value = 1000 * exp(-0.1 * pos_rank)) %>% 
  mutate(player_id = as.character(player_id),
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
           user_name == "nickg1212" ~ "Nick Galletti",
           user_name == "reinhart64" ~ "Michael Reinhart",
           TRUE ~ user_name)) %>% 
  ungroup()

draft_history_join <- 
  draft_history %>% 
  left_join(scoring_history, by = c("season", "player_id" = "platform_id")) %>%
  mutate(value_diff = ppg_rank_value - pos_rank_value,
         games_played_percent = replace_na(games_played_percent, 0),
         injury_value_lost = pos_rank_value * (1-games_played_percent)) %>% 
  filter(pos %in% c("QB","RB","WR","TE"))

draft_values <- 
  draft_history_join %>% 
  group_by(user_name) %>% 
  summarise(injury_value_oe = sum(injury_value_lost, na.rm = TRUE) / sum(pos_rank_value, na.rm = TRUE),
            draft_value_oe= sum(ppg_rank_value, na.rm = TRUE) / sum(pos_rank_value, na.rm = TRUE) - 1) %>% 
  ungroup()

#Determine best and worst picks
combine_word <- function(name, finish_string){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{finish_string}</span></div>"
  )
}

library(glue)
best_draft_picks <-
  draft_history_join %>%
  group_by(user_name) %>%
  slice_min(order_by = -value_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(best_string = glue("Drafted {pos}{pos_rank} - Finshed {pos}{ppg_rank}"),
         best_headshot = map2(player_id, season, get_headshot),
         player_name = glue("{player_name} ({season})"),
         best_combo = combine_word(player_name, best_string),
         best_combo = map(best_combo, gt::html)) %>% 
  select(user_name, best_combo, best_headshot)

worst_draft_picks <-
  draft_history_join %>%
  group_by(user_name) %>%
  slice_min(order_by = value_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(worst_string = glue("Drafted {pos}{pos_rank} - Finshed {pos}{ppg_rank}"),
         worst_headshot = map2(player_id, season, get_headshot),
         player_name = glue("{player_name} ({season})"),
         worst_combo = combine_word(player_name, worst_string),
         worst_combo = map(worst_combo, gt::html)) %>%
  select(user_name, worst_combo, worst_headshot)

top_draft_teams <-
  draft_history_join %>%
  group_by(user_name, team) %>%
  summarise(avg_value = sum(pos_rank_value)) %>%
  ungroup() %>%
  group_by(user_name) %>%
  arrange(-avg_value) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(team = case_when(team == "NEP" ~ "NE",
                          team == "GBP" ~ "GB",
                          team == "KCC" ~ "KC",
                          team == "NOS" ~ "NO",
                          TRUE ~ team)) %>%
  left_join(select(nflfastR::teams_colors_logos, team_abbr, team_logo_espn = team_logo_wikipedia), by = c("team" = "team_abbr")) %>%
  select(user_name, team_logo_espn)

#Draft History Plot
draft_pick <- 
  draft_history %>% 
  group_by(season, user_name) %>% 
  summarise(yearly_pick = min(overall)) %>% 
  ungroup() %>%
  mutate(pick_suffix = case_when(yearly_pick %in% c(11,12,13) ~ "th",
                                 yearly_pick %% 10 == 1 ~ 'st',
                                 yearly_pick %% 10 == 2 ~ 'nd',
                                 yearly_pick %% 10 == 3 ~'rd',
                                 TRUE ~ "th")) %>%
  group_by(user_name) %>% 
  summarise(pick_year = paste(yearly_pick, pick_suffix, sep = "", collapse = ", "),
            years = n(),
            last_season = max(season)) %>% 
  ungroup()

# Records
# Draft Skill
espn_standings_history <- 
  tibble(season = c(2015:2020)) %>%
  mutate(nested_records = map(season, get_standings_history_espn)) %>% 
  unnest(cols = nested_records)

standings_nested <- league_id_history %>% 
  mutate(yearly_standings = pmap(.l = list(season = season,
                                           league_id = league_id,
                                           platform = platform),
                                 .f = get_standings_history_sleeper))

standings_unnested <- standings_nested %>% 
  unnest(yearly_standings)


playoffs_nested <- league_id_history %>% 
  mutate(playoff_standings = map2(.x = league_id,
                                  .y = season,
                                  .f = get_sleeper_playoffs))

playoffs_unnested <- playoffs_nested %>% 
  unnest(playoff_standings)

sleeper_standings_history <- standings_unnested %>% 
  left_join(playoffs_unnested, by = c("franchise_id", "user_name", "season", "league_id", "platform")) %>% 
  rename(league_rank = final_place) %>% 
  select(colnames(espn_standings_history))


standings_history_combined <-
  espn_standings_history %>% 
  bind_rows(sleeper_standings_history) %>% 
  mutate(user_name = case_when(
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
    user_name == "nickg1212" ~ "Nick Galletti",
    user_name == "reinhart64" ~ "Michael Reinhart",
    
    TRUE ~ user_name))

blank_emojis <- 
  crossing(user_name = unique(standings_history_combined$user_name),
           season = c(2015:2022))

standings_history <- 
  blank_emojis %>% 
  full_join(standings_history_combined, by = c("user_name", "season")) %>%
  mutate(finish_emoji = case_when(is.na(league_rank) ~ emoji("black_square_button"),
                                  league_rank == 1 ~ emoji("trophy"),
                                  league_rank == 2 ~ medal("silver"),
                                  league_rank == 3 ~ medal("bronze"),
                                  league_rank %in% c(4,5,6) ~ emoji("flexed_biceps"),
                                  league_rank == 12 ~ emoji("taco"),
                                  TRUE ~ emoji("wastebasket"))) %>% 
  group_by(user_name) %>% 
  summarise(across(.cols = c(h2h_wins, h2h_losses, h2h_ties, allplay_wins, allplay_losses, points_for),
                   .fns = ~sum(.x, na.rm = TRUE)),
            emoji_collase = paste(finish_emoji, sep = "", collapse = ""),
            h2h_winpct = h2h_wins / (h2h_wins + h2h_losses + h2h_ties),
            allplay_winpct = allplay_wins / (allplay_wins + allplay_losses),
            first_place = sum(league_rank == 1, na.rm = TRUE),
            luck_pct = h2h_winpct - allplay_winpct) %>% 
  ungroup() %>% 
  select(user_name, h2h_winpct, allplay_winpct, allplay_wins, emoji_collase, luck_pct, first_place)

# PLOTS
flea_df <- 
  draft_pick %>% 
  # filter(last_season == 2020) %>%
  inner_join(standings_history, by = "user_name") %>% 
  inner_join(best_draft_picks, by = "user_name") %>%
  inner_join(worst_draft_picks, by = "user_name") %>%
  inner_join(top_draft_teams, by = "user_name") %>%
  inner_join(draft_values, by = "user_name") %>% 
  mutate(user_name = str_to_title(user_name)) %>% 
  
  arrange(-first_place, -allplay_wins) %>% 
  select(-c(first_place, last_season, pick_year, allplay_wins))

saveRDS(flea_df, "data/flea_df_2022.RDS")

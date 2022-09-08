
# Libraries ---------------------------------------------------------------

library(ffscrapr)
library(tidyverse)
library(ffpros)


# Load Data 2021 ---------------------------------------------------------------

flea_conn <- ff_connect(platform = "sleeper", league_id = "739986523688230912", season = 2021)

flea_draft <- ff_draft(flea_conn) %>%
  mutate(ovr_pick = row_number(),
         pick_value = 1000*exp(-0.025 * ovr_pick))

ffpros_ranks <- fp_rankings(page = "superflex-cheatsheets", sport = "nfl") %>%
  left_join(select(dp_playerids(), fantasypros_id, sleeper_id), by = "fantasypros_id") %>% 
  transmute(sleeper_id, rank, pos_rank, rank_value = 1000*exp(-0.025 * rank))

flea_join <- 
  flea_draft %>% 
  left_join(ffpros_ranks, by = c("player_id"="sleeper_id")) %>% 
  mutate(pick_diff = rank_value - pick_value) 

worst_picks <- flea_join %>% 
  group_by(franchise_name) %>%
  slice_min(order_by = pick_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

best_picks <- flea_join %>% 
  group_by(franchise_name) %>%
  slice_min(order_by = -pick_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

# Load Data 2022 ---------------------------------------------------------------

flea_conn <- ff_connect(platform = "sleeper", league_id = "863895883606056960", season = 2022)

flea_draft <- ff_draft(flea_conn) %>%
  mutate(ovr_pick = row_number(),
         pick_value = 1000*exp(-0.025 * ovr_pick)) %>% 
  group_by(pos) %>% 
  mutate(pos_pick = row_number(ovr_pick)) %>% 
  ungroup()

ffpros_ranks <- fp_rankings(page = "superflex-cheatsheets", sport = "nfl") %>%
  left_join(select(dp_playerids(), fantasypros_id, sleeper_id), by = "fantasypros_id") %>% 
  transmute(sleeper_id, rank, pos_rank, rank_value = 1000*exp(-0.025 * rank))

flea_join <- 
  flea_draft %>% 
  left_join(ffpros_ranks, by = c("player_id"="sleeper_id")) %>% 
  mutate(pick_diff = rank_value - pick_value) 

worst_picks <- flea_join %>% 
  group_by(franchise_name) %>%
  slice_min(order_by = pick_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

best_picks <- flea_join %>% 
  group_by(franchise_name) %>%
  slice_min(order_by = -pick_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

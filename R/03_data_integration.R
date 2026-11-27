# Data Integration ----

library(tidyverse)
library(lubridate)
library(here)

premier_league_matches <- readRDS(
  here("data", "processed",
       "premier_league_matches.rds")
)

xg_matches <- readRDS(
  here("data", "processed",
       "xg_matches.rds")
)

# Team Name Standardisation ----

standardise_team <- function(team) {
  
  team %>%
    str_squish() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "") %>%
    recode(
      "manchestercity" = "mancity",
      "manchesterunited" = "manunited",
      "tottenhamhotspur" = "tottenham",
      "wolverhamptonwanderers" = "wolves",
      "wolverhampton" = "wolves",
      "nottinghamforest" = "nottmforest",
      "newcastleunited" = "newcastle",
      "brightonhovealbion" = "brighton",
      "brightonandhovealbion" = "brighton",
      "westhamunited" = "westham",
      "westbromwichalbion" = "westbrom",
      "leedsunited" = "leeds",
      "leicestercity" = "leicester",
      "afcbournemouth" = "bournemouth",
      "huddersfieldtown" = "huddersfield",
      "cardiffcity" = "cardiff",
      "swanseacity" = "swansea",
      "norwichcity" = "norwich",
      "ipswichtown" = "ipswich",
      "hullcity" = "hull",
      "st okecity" = "stoke",
      "stokecity" = "stoke"
    )
}
# Apply Team Name Standardisation ----
premier_league_matches <- premier_league_matches %>%
  mutate(
    home_key = standardise_team(home_team),
    away_key = standardise_team(away_team)
  )

xg_matches <- xg_matches %>%
  mutate(
    home_key = standardise_team(home_team),
    away_key = standardise_team(away_team)
  )

premier_league_matches <- premier_league_matches %>%
  mutate(
    home_key = standardise_team(home_team),
    away_key = standardise_team(away_team)
  )

xg_matches <- xg_matches %>%
  mutate(
    home_key = standardise_team(home_team),
    away_key = standardise_team(away_team)
  )

# Check for 
# Check for Inconsistent Team Names ----
football_teams <- unique(c(
  premier_league_matches$home_key,
  premier_league_matches$away_key
))

xg_teams <- unique(c(
  xg_matches$home_key,
  xg_matches$away_key
))

setdiff(xg_teams, football_teams)
# Check all Fixtures Match----
join_keys <- c(
  "season",
  "date",
  "home_key",
  "away_key"
)

unmatched_xg <- xg_matches %>%
  anti_join(
    premier_league_matches,
    by = join_keys
  )

print(unmatched_xg)

# Join Football Data and Expected Goals ----

premier_league_master <- premier_league_matches %>%
  
  rename(
    home_xg_football_data = home_xg,
    away_xg_football_data = away_xg
  ) %>%
  
  left_join(
    
    xg_matches %>%
      select(
        all_of(join_keys),
        understat_id,
        xg_scoreline,
        home_xg_understat,
        away_xg_understat,
        xg_provider,
        totals_unreliable
      ),
    
    by = join_keys,
    
    relationship = "one-to-one"
    
  ) %>%
  arrange(date)
# Check for Loss or Duplication----
stopifnot(
  nrow(premier_league_master) ==
    nrow(premier_league_matches)
)
# Check for Conflicting Scorelines----
scoreline_conflicts <- premier_league_master %>%
  filter(
    !is.na(xg_scoreline),
    scoreline != xg_scoreline
  ) %>%
  select(
    date,
    home_team,
    away_team,
    scoreline,
    xg_scoreline
  )

print(scoreline_conflicts)
# Check Final xG Coverage----
master_coverage <- premier_league_master %>%
  group_by(season) %>%
  summarise(
    
    matches = n(),
    
    understat_coverage =
      mean(
        !is.na(home_xg_understat) &
          !is.na(away_xg_understat)
      ) * 100,
    
    football_data_xg_coverage =
      mean(
        !is.na(home_xg_football_data) &
          !is.na(away_xg_football_data)
      ) * 100,
    
    .groups = "drop"
  )

print(master_coverage, n = Inf)
# Save Master Dataset----
stopifnot(
  nrow(unmatched_xg) == 0,
  nrow(scoreline_conflicts) == 0
)

big_six_master <- premier_league_master %>%
  filter(big_six_fixture)

write_csv(
  premier_league_master,
  here("data", "processed",
       "premier_league_master.csv")
)

saveRDS(
  premier_league_master,
  here("data", "processed",
       "premier_league_master.rds")
)

saveRDS(
  big_six_master,
  here("data", "processed",
       "big_six_master.rds")
)
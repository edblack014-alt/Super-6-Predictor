# Data Acquisition

library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# Data Structure----

# Define Seasons and Leagues
start_years <- 2016:2026

seasons <- sprintf(
  "%02d%02d",
  start_years %% 100,
  (start_years + 1) %% 100
)

big_six <- c(
  "Arsenal",
  "Chelsea",
  "Liverpool",
  "Man City",
  "Man United",
  "Tottenham"
)


# Data Retrieval Function


# Identify Date ----
today <- Sys.Date()

current_year <- year(today)

season_start <- if (
  month(today) >= 7
) {
  current_year
} else {
  current_year - 1
}

current_season <- sprintf(
  "%02d%02d",
  season_start %% 100,
  (season_start + 1) %% 100
)

start_years <- 2016:season_start

seasons <- sprintf(
  "%02d%02d",
  start_years %% 100,
  (start_years + 1) %% 100
)


# Data Retrieval Function ----
get_premier_league <- function(season) {
  
  file_path <- here(
    "data",
    "raw",
    paste0("EPL_", season, ".csv")
  )
  
  # Load historical data if already downloaded
  if (file.exists(file_path) &&
      season != current_season) {
    
    message("Loading cached season: ", season)
    
    data <- read_csv(
      file_path,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
    
  } else {
    
    # Download missing or current season data
    message("Downloading season: ", season)
    
    url <- paste0(
      "https://www.football-data.co.uk/mmz4281/",
      season,
      "/E0.csv"
    )
    
    data <- read_csv(
      url,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
    
    # Validate before saving
    required_columns <- c(
      "Date", "HomeTeam", "AwayTeam",
      "FTHG", "FTAG"
    )
    
    if (!all(required_columns %in% names(data))) {
      stop("Missing essential columns: ", season)
    }
    
    if (nrow(data) == 0) {
      stop("Downloaded dataset is empty: ", season)
    }
    
    write_csv(data, file_path)
  }
  
  data %>%
    mutate(season = season)
}

# Retrieve and Combine Historical Data ----

premier_league_raw <- map_dfr(
  seasons,
  get_premier_league
)

# Select and Clean Variables----
optional_columns <- c(
  "HTHG", "HTAG",
  "HS", "AS", "HST", "AST",
  "HxG", "AxG",
  "HC", "AC",
  "HY", "AY", "HR", "AR",
  "AvgH", "AvgD", "AvgA",
  "Avg>2.5", "Avg<2.5",
  "BbAvH", "BbAvD", "BbAvA",
  "BbAv>2.5", "BbAv<2.5"
)

for (column in optional_columns) {
  if (!column %in% names(premier_league_raw)) {
    premier_league_raw[[column]] <- NA_character_
  }
}

premier_league_matches <- premier_league_raw %>%
  transmute(
    season,
    date = dmy(Date),
    
    home_team = HomeTeam,
    away_team = AwayTeam,
    
    home_goals = as.integer(FTHG),
    away_goals = as.integer(FTAG),
    
    home_ht_goals = as.integer(HTHG),
    away_ht_goals = as.integer(HTAG),
    
    home_shots = as.numeric(HS),
    away_shots = as.numeric(AS),
    
    home_shots_target = as.numeric(HST),
    away_shots_target = as.numeric(AST),
    
    home_xg = as.numeric(HxG),
    away_xg = as.numeric(AxG),
    
    home_corners = as.numeric(HC),
    away_corners = as.numeric(AC),
    
    home_yellow_cards = as.numeric(HY),
    away_yellow_cards = as.numeric(AY),
    
    home_red_cards = as.numeric(HR),
    away_red_cards = as.numeric(AR),
    
    odds_home = coalesce(
      as.numeric(AvgH), as.numeric(BbAvH)
    ),
    
    odds_draw = coalesce(
      as.numeric(AvgD), as.numeric(BbAvD)
    ),
    
    odds_away = coalesce(
      as.numeric(AvgA), as.numeric(BbAvA)
    ),
    
    odds_over_25 = coalesce(
      as.numeric(`Avg>2.5`), as.numeric(`BbAv>2.5`)
    ),
    
    odds_under_25 = coalesce(
      as.numeric(`Avg<2.5`), as.numeric(`BbAv<2.5`)
    ),
      
    odds_source = case_when(
      !is.na(AvgH) ~ "OddsPortal",
      !is.na(BbAvH) ~ "Betbrain",
      TRUE ~ NA_character_
      )
    ) %>%
    
  filter(
    !is.na(date),
    !is.na(home_team),
    !is.na(away_team),
    !is.na(home_goals),
    !is.na(away_goals),
    date <= Sys.Date()
  ) %>%
  mutate(
    total_goals = home_goals + away_goals,
    
    scoreline = paste0(home_goals, "-", away_goals),
    
    result = case_when(
      home_goals > away_goals ~ "H",
      home_goals == away_goals ~ "D",
      TRUE ~ "A"
    ),
    
    big_six_fixture = home_team %in% big_six |
      away_team %in% big_six,
    
    fixture_type = case_when(
      home_team %in% big_six &
        away_team %in% big_six ~ "Big Six vs Big Six",
      
      home_team %in% big_six ~ "Big Six at Home",
      
      away_team %in% big_six ~ "Big Six Away",
      
      TRUE ~ "Other"
    )
  ) %>%
  arrange(date)

# Big Six Dataset----
big_six_matches <- premier_league_matches %>%
  filter(big_six_fixture)


# Validate Data----
premier_league_matches %>%
  group_by(season) %>%
  summarise(
    total_matches = n(),
    big_six_matches = sum(big_six_fixture),
    .groups = "drop"
  )

# Check for Missing Data / Duplicates----
missing_summary <- premier_league_matches %>%
  summarise(
    across(
      everything(),
      ~ round(mean(is.na(.x)) * 100, 2)
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_pct"
  ) %>%
  arrange(desc(missing_pct))

print(missing_summary, n = Inf)

premier_league_matches %>%
  count(season, home_team, away_team) %>%
  filter(n > 1)


# Write Processed Data ----
write_csv(
  premier_league_matches,
  here("data", "processed", "premier_league_matches.csv")
)

write_csv(
  big_six_matches,
  here("data", "processed", "big_six_matches.csv")
)

saveRDS(
  premier_league_matches,
  here("data", "processed", "premier_league_matches.rds")
)

saveRDS(
  big_six_matches,
  here("data", "processed", "big_six_matches.rds")
)
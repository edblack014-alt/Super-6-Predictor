# Load Packages and Datasets ----
library(tidyverse)
library(lubridate)
library(here)

premier_league_master <- readRDS(
  here(
    "data", "processed",
    "premier_league_master.rds"
  )
)

feature_data <- readRDS(
  here(
    "data", "processed",
    "team_features.rds"
  )
)

development_data <- premier_league_master %>%
  filter(date < as.Date("2024-07-01"))
# Restructure Expected Goals Data ----

valid_xg_matches <- development_data %>%
  filter(
    !is.na(home_xg_understat),
    !is.na(away_xg_understat),
    totals_unreliable == FALSE
  )

home_xg_data <- valid_xg_matches %>%
  transmute(
    date,
    season,
    team = home_key,
    opponent = away_key,
    home = 1L,
    xg = home_xg_understat,
    xga = away_xg_understat
  )

away_xg_data <- valid_xg_matches %>%
  transmute(
    date,
    season,
    team = away_key,
    opponent = home_key,
    home = 0L,
    xg = away_xg_understat,
    xga = home_xg_understat
  )

team_xg_history <- bind_rows(
  home_xg_data,
  away_xg_data
) %>%
  arrange(date)

glimpse(team_xg_history)
# Calculate Weekly xG Features ----

build_weekly_xg <- function(
    cutoff,
    window = 10,
    prior_strength = 5
) {
  
  history <- team_xg_history %>%
    filter(
      date < cutoff,
      date >= cutoff - 365
    )
  
  stopifnot(nrow(history) > 0)
  
  league_xg <- mean(history$xg)
  
  team_form <- history %>%
    group_by(team) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_tail(n = window) %>%
    summarise(
      
      xg_matches = n(),
      
      recent_xgf = (
        sum(xg) + prior_strength * league_xg
      ) / (n() + prior_strength),
      
      recent_xga = (
        sum(xga) + prior_strength * league_xg
      ) / (n() + prior_strength),
      
      .groups = "drop"
    )
  
  fixtures <- feature_data %>%
    filter(week == cutoff) %>%
    distinct(team, week)
  
  fixtures %>%
    left_join(
      team_form,
      by = "team",
      relationship = "many-to-one"
    ) %>%
    mutate(
      
      recent_xgf = coalesce(
        recent_xgf, league_xg
      ),
      
      recent_xga = coalesce(
        recent_xga, league_xg
      ),
      
      xg_matches = coalesce(
        xg_matches, 0L
      )
    )
}
# Generate Historical Weekly Features ----

xg_weeks <- feature_data %>%
  filter(week >= as.Date("2018-07-01")) %>%
  distinct(week) %>%
  arrange(week) %>%
  pull(week)

weekly_xg_features <- map_dfr(
  xg_weeks,
  build_weekly_xg
)
# Integrate Goals and Expected Goals Features ----

xg_feature_data <- feature_data %>%
  filter(week >= as.Date("2018-07-01")) %>%
  
  left_join(
    weekly_xg_features,
    by = c("team", "week"),
    relationship = "many-to-one"
  ) %>%
  
  left_join(
    
    weekly_xg_features %>%
      rename(
        opponent = team,
        opponent_xgf = recent_xgf,
        opponent_xga = recent_xga,
        opponent_xg_matches = xg_matches
      ),
    
    by = c("opponent", "week"),
    
    relationship = "many-to-one"
    
  )

View(xg_feature_data)
xg_feature_validation <- xg_feature_data %>%
  summarise(
    
    observations = n(),
    
    missing_xgf = sum(is.na(recent_xgf)),
    
    missing_xga = sum(is.na(recent_xga)),
    
    missing_opponent_xga =
      sum(is.na(opponent_xga)),
    
    min_history = min(xg_matches),
    
    max_history = max(xg_matches),
    
    fallback_observations =
      sum(xg_matches == 0)
    
  )

print(xg_feature_validation, width = Inf)
# Validate and Save ----
xg_feature_validation <- xg_feature_data %>%
  summarise(
    
    observations = n(),
    
    missing_xgf = sum(is.na(recent_xgf)),
    
    missing_xga = sum(is.na(recent_xga)),
    
    missing_opponent_xga =
      sum(is.na(opponent_xga)),
    
    min_history = min(xg_matches),
    
    max_history = max(xg_matches),
    
    fallback_observations =
      sum(xg_matches == 0)
    
  )

print(xg_feature_validation, width = Inf)
stopifnot(
  
  nrow(xg_feature_data) ==
    sum(feature_data$week >= as.Date("2018-07-01")),
  
  !anyNA(xg_feature_data$recent_xgf),
  
  !anyNA(xg_feature_data$recent_xga),
  
  !anyNA(xg_feature_data$opponent_xga),
  
  all(xg_feature_data$recent_xgf > 0),
  
  all(xg_feature_data$opponent_xga > 0),
  
  all(xg_feature_data$xg_matches <= 10)
  
)

saveRDS(
  xg_feature_data,
  here(
    "data", "processed",
    "xg_feature_data.rds"
  )
)
# Predictive Power of XG ----
xg_correlations <- xg_feature_data %>%
  filter(date < as.Date("2021-07-01")) %>%
  summarise(
    
    goals_attack = cor(
      recent_gf, goals
    ),
    
    xg_attack = cor(
      recent_xgf, goals
    ),
    
    goals_defence = cor(
      opponent_ga, goals
    ),
    
    xg_defence = cor(
      opponent_xga, goals
    )
  )

print(xg_correlations, width = Inf)
xg_feature_data %>%
  filter(date < as.Date("2021-07-01")) %>%
  select(
    goals,
    recent_gf,
    recent_xgf
  ) %>%
  pivot_longer(
    cols = c(recent_gf, recent_xgf),
    names_to = "indicator",
    values_to = "recent_performance"
  ) %>%
  group_by(indicator) %>%
  mutate(
    performance_decile =
      ntile(recent_performance, 10)
  ) %>%
  group_by(indicator, performance_decile) %>%
  summarise(
    average_performance = mean(recent_performance),
    subsequent_goals = mean(goals),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = average_performance,
      y = subsequent_goals,
      colour = indicator
    )
  ) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Goals vs Expected Goals as Predictors",
    subtitle = "Observations grouped into performance deciles",
    x = "Recent attacking performance",
    y = "Subsequent goals scored",
    colour = "Indicator"
  ) +
  theme_minimal()
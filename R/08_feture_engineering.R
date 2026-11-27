library(tidyverse)
library(lubridate)
library(here)

premier_league_master <- readRDS(
  here(
    "data", "processed",
    "premier_league_master.rds"
  )
)

development_data <- premier_league_master %>%
  filter(date < as.Date("2024-07-01"))

# Create Team-Level Historical Data ----

home_data <- development_data %>%
  transmute(
    date,
    season,
    home_key,
    away_key,
    big_six_fixture,
    scoreline,
    
    team = home_key,
    opponent = away_key,
    home = 1L,
    
    goals = home_goals,
    conceded = away_goals
  )

away_data <- development_data %>%
  transmute(
    date,
    season,
    home_key,
    away_key,
    big_six_fixture,
    scoreline,
    
    team = away_key,
    opponent = home_key,
    home = 0L,
    
    goals = away_goals,
    conceded = home_goals
  )

team_history <- bind_rows(
  home_data,
  away_data
) %>%
  mutate(
    week = floor_date(
      date,
      unit = "week",
      week_start = 1
    )
  ) %>%
  arrange(date)

glimpse(team_history)
# Generate Pre-Match Features ----

build_weekly_features <- function(cutoff,
                                  window = 10,
                                  prior_strength = 5) {
  
  # Historical information available at cutoff
  history <- team_history %>%
    filter(
      date < cutoff,
      date >= cutoff - 365
    )
  
  # No historical information for the first week
  if (nrow(history) == 0) {
    return(tibble())
  }
  
  league_average <- mean(history$goals)
  
  # Estimate recent team performance
  team_form <- history %>%
    group_by(team) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_tail(n = window) %>%
    summarise(
      
      form_matches = n(),
      
      recent_gf = (
        sum(goals) +
          prior_strength * league_average
      ) / (n() + prior_strength),
      
      recent_ga = (
        sum(conceded) +
          prior_strength * league_average
      ) / (n() + prior_strength),
      
      .groups = "drop"
    )
  
  # Identify matches taking place this week
  fixtures <- team_history %>%
    filter(week == cutoff)
  
  # Attach own team and opposition form
  fixtures %>%
    
    left_join(
      team_form,
      by = "team",
      relationship = "many-to-one"
    ) %>%
    
    left_join(
      team_form %>%
        rename(
          opponent = team,
          opponent_form_matches = form_matches,
          opponent_gf = recent_gf,
          opponent_ga = recent_ga
        ),
      by = "opponent",
      relationship = "many-to-one"
    ) %>%
    
    mutate(
      
      # Fallback for teams without recent history
      recent_gf = coalesce(
        recent_gf, league_average
      ),
      
      recent_ga = coalesce(
        recent_ga, league_average
      ),
      
      opponent_gf = coalesce(
        opponent_gf, league_average
      ),
      
      opponent_ga = coalesce(
        opponent_ga, league_average
      ),
      
      form_matches = coalesce(
        form_matches, 0L
      ),
      
      opponent_form_matches = coalesce(
        opponent_form_matches, 0L
      )
    )
}
# Generate Historical Features ----
all_weeks <- team_history %>%
  distinct(week) %>%
  arrange(week) %>%
  pull(week)

feature_data <- map_dfr(
  all_weeks,
  build_weekly_features
)

View(feature_data)

glimpse(feature_data)
# Validate Features ----
feature_data %>%
  summarise(
    observations = n(),
    
    missing_gf = sum(is.na(recent_gf)),
    
    missing_ga = sum(is.na(recent_ga)),
    
    missing_opponent_ga =
      sum(is.na(opponent_ga)),
    
    min_form_matches = min(form_matches),
    
    max_form_matches = max(form_matches),
    
    average_recent_gf = mean(recent_gf),
    
    average_recent_ga = mean(recent_ga)
  )
stopifnot(
  !anyNA(feature_data$recent_gf),
  !anyNA(feature_data$recent_ga),
  !anyNA(feature_data$opponent_ga),
  
  all(feature_data$form_matches <= 10),
  
  all(feature_data$recent_gf > 0),
  
  all(feature_data$opponent_ga > 0)
)
# Save Dataset ----
saveRDS(
  feature_data,
  here(
    "data", "processed",
    "team_features.rds"
  )
)
# Predictive Power of Recent Form ----
feature_correlations <- feature_data %>%
  filter(date < as.Date("2021-07-01")) %>%
  summarise(
    
    recent_attack = cor(
      recent_gf, goals
    ),
    
    opponent_defensive_weakness = cor(
      opponent_ga, goals
    ),
    
    own_defensive_weakness = cor(
      recent_ga, conceded
    ),
    
    opponent_attack = cor(
      opponent_gf, conceded
    )
  )

print(feature_correlations, width = Inf)
feature_data %>%
  filter(date < as.Date("2021-07-01")) %>%
  mutate(
    attacking_group = ntile(recent_gf, 10)
  ) %>%
  group_by(attacking_group) %>%
  summarise(
    predicted_form = mean(recent_gf),
    actual_goals = mean(goals),
    observations = n(),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = predicted_form,
      y = actual_goals
    )
  ) +
  geom_point(size = 3, colour = "#398469") +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Does Recent Attacking Form Predict Goals?",
    subtitle = "Historical matches grouped into deciles",
    x = "Recent average goals scored",
    y = "Subsequent average goals scored"
  ) +
  theme_minimal()


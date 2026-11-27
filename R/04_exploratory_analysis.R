# Exploratory Data Analysis ----

library(tidyverse)
library(lubridate)
library(here)

premier_league_master <- readRDS(
  here(
    "data",
    "processed",
    "premier_league_master.rds"
  )
)

theme_set(theme_minimal())

development_data <- premier_league_master %>%
  filter(date < as.Date("2024-07-01"))

test_data <- premier_league_master %>%
  filter(
    season %in% c("2425", "2526")
  )

current_season_data <- premier_league_master %>%
  filter(season == "2627")

big_six_development <- development_data %>%
  filter(big_six_fixture)

# Scoreline Frequency ----

scoreline_frequency <- big_six_development %>%
  count(scoreline, sort = TRUE) %>%
  mutate(
    probability = n / sum(n),
    percentage = probability * 100
  )

print(scoreline_frequency, n = 20)
scoreline_frequency %>%
  slice_head(n = 15) %>%
  ggplot(
    aes(
      x = reorder(scoreline, percentage),
      y = percentage
    )
  ) +
  geom_col(fill = "#398469") +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    hjust = -0.2
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Most Common Big Six Scorelines",
    subtitle = "Premier League, 2016/17–2023/24",
    x = "Scoreline",
    y = "Percentage of fixtures"
  )
scoreline_matrix <- big_six_development %>%
  count(home_goals, away_goals) %>%
  mutate(probability = n / sum(n)) %>%
  complete(
    home_goals = 0:max(home_goals),
    away_goals = 0:max(away_goals),
    fill = list(n = 0, probability = 0)
  )

scoreline_matrix %>%
  filter(home_goals <= 5, away_goals <= 5) %>%
  ggplot(
    aes(
      x = away_goals,
      y = home_goals,
      fill = probability
    )
  ) +
  geom_tile(colour = "white") +
  geom_text(
    aes(label = scales::percent(
      probability,
      accuracy = 0.1
    ))
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#398469",
    labels = scales::percent
  ) +
  scale_y_reverse(breaks = 0:5) +
  scale_x_continuous(breaks = 0:5) +
  coord_equal() +
  labs(
    title = "Historical Scoreline Probability Matrix",
    x = "Away goals",
    y = "Home goals",
    fill = "Probability"
  )
# Goal Distribution Statistics ----

goal_distribution <- development_data %>%
  summarise(
    mean_home = mean(home_goals),
    variance_home = var(home_goals),
    
    mean_away = mean(away_goals),
    variance_away = var(away_goals),
    
    dispersion_home = variance_home / mean_home,
    dispersion_away = variance_away / mean_away,
    
    correlation = cor(home_goals, away_goals)
  )

print(goal_distribution)
# Compare Observed and Theoretical Distributions ----
home_goal_comparison <- development_data %>%
  count(home_goals) %>%
  complete(
    home_goals = 0:max(home_goals),
    fill = list(n = 0)
  ) %>%
  mutate(
    observed = n / sum(n),
    
    poisson = dpois(
      home_goals,
      lambda = mean(development_data$home_goals)
    )
  ) %>%
  pivot_longer(
    cols = c(observed, poisson),
    names_to = "distribution",
    values_to = "probability"
  )

ggplot(
  home_goal_comparison,
  aes(
    x = factor(home_goals),
    y = probability,
    fill = distribution
  )
) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Observed vs Poisson Goal Distribution",
    subtitle = "Home goals",
    x = "Goals scored",
    y = "Probability",
    fill = "Distribution"
  )
# League-wide Home Advantage ----

home_advantage <- development_data %>%
  summarise(
    average_home_goals = mean(home_goals),
    average_away_goals = mean(away_goals),
    
    goal_advantage =
      average_home_goals - average_away_goals,
    
    home_win_pct = mean(result == "H") * 100,
    draw_pct = mean(result == "D") * 100,
    away_win_pct = mean(result == "A") * 100
  )

print(home_advantage)
home_advantage_season <- development_data %>%
  group_by(season) %>%
  summarise(
    home_goals = mean(home_goals),
    away_goals = mean(away_goals),
    goal_advantage = home_goals - away_goals,
    .groups = "drop"
  )

home_advantage_season %>%
  pivot_longer(
    cols = c(home_goals, away_goals),
    names_to = "venue",
    values_to = "average_goals"
  ) %>%
  ggplot(
    aes(
      x = season,
      y = average_goals,
      colour = venue,
      group = venue
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Evolution of Home Advantage",
    x = "Season",
    y = "Average goals per match",
    colour = "Venue"
  )
# Fixture Type Analysis ----

fixture_summary <- big_six_development %>%
  group_by(fixture_type) %>%
  summarise(
    matches = n(),
    
    mean_home_goals = mean(home_goals),
    mean_away_goals = mean(away_goals),
    mean_total_goals = mean(total_goals),
    
    home_win_pct = mean(result == "H") * 100,
    draw_pct = mean(result == "D") * 100,
    away_win_pct = mean(result == "A") * 100,
    
    over_25_pct = mean(total_goals > 2.5) * 100,
    
    both_teams_score_pct =
      mean(home_goals > 0 & away_goals > 0) * 100,
    
    .groups = "drop"
  )

print(fixture_summary, width = Inf)
fixture_scorelines <- big_six_development %>%
  count(fixture_type, scoreline) %>%
  group_by(fixture_type) %>%
  mutate(
    probability = n / sum(n)
  ) %>%
  slice_max(
    order_by = probability,
    n = 5,
    with_ties = FALSE
  ) %>%
  arrange(fixture_type, desc(probability)) %>%
  ungroup()

print(fixture_scorelines, n = Inf)
# Changes in Premier League Scoring ----
# Evolution of Goal Scoring ----

seasonal_scoring <- development_data %>%
  group_by(season) %>%
  summarise(
    average_goals = mean(total_goals),
    average_home_goals = mean(home_goals),
    average_away_goals = mean(away_goals),
    over_25_pct = mean(total_goals > 2.5) * 100,
    .groups = "drop"
  )

ggplot(
  seasonal_scoring,
  aes(
    x = season,
    y = average_goals,
    group = 1
  )
) +
  geom_line(colour = "#398469", linewidth = 1.2) +
  geom_point(size = 3, colour = "#398469") +
  geom_text(
    aes(label = round(average_goals, 2)),
    vjust = -1
  ) +
  labs(
    title = "Evolution of Premier League Goal Scoring",
    x = "Season",
    y = "Average goals per match"
  )

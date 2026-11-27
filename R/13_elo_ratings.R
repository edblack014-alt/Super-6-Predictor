# Load Packages and Data ----
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
  filter(date < as.Date("2024-07-01")) %>%
  mutate(
    week = floor_date(
      date,
      unit = "week",
      week_start = 1
    )
  ) %>%
  arrange(date, home_key, away_key)
# ELO Engine ----
calculate_elo <- function(
    matches,
    k = 20,
    home_advantage = 65,
    initial_rating = 1500
) {
  
  matches <- matches %>%
    arrange(date, home_key, away_key)
  
  teams <- sort(
    unique(
      c(matches$home_key, matches$away_key)
    )
  )
  
  ratings <- setNames(
    rep(initial_rating, length(teams)),
    teams
  )
  
  matches$elo_home <- NA_real_
  matches$elo_away <- NA_real_
  
  weekly_indices <- split(
    seq_len(nrow(matches)),
    matches$week
  )
  
  for (indices in weekly_indices) {
    
    # Freeze ratings at the weekly cutoff
    
    matches$elo_home[indices] <-
      ratings[matches$home_key[indices]]
    
    matches$elo_away[indices] <-
      ratings[matches$away_key[indices]]
    
    # Update ratings after recording forecasts
    
    for (i in indices) {
      
      h <- matches$home_key[i]
      a <- matches$away_key[i]
      
      expected_home <- 1 / (
        1 + 10^(
          (ratings[a] -
             ratings[h] -
             home_advantage) / 400
        )
      )
      
      actual_home <- case_when(
        matches$home_goals[i] >
          matches$away_goals[i] ~ 1,
        
        matches$home_goals[i] ==
          matches$away_goals[i] ~ 0.5,
        
        TRUE ~ 0
      )
      
      adjustment <- k * (
        actual_home - expected_home
      )
      
      ratings[h] <- ratings[h] + adjustment
      ratings[a] <- ratings[a] - adjustment
    }
  }
  
  list(
    matches = matches,
    
    final_ratings = tibble(
      team = names(ratings),
      elo = as.numeric(ratings)
    ) %>%
      arrange(desc(elo))
  )
}
# Generate Historical Ratings ----
elo_results <- calculate_elo(
  matches = development_data,
  k = 20,
  home_advantage = 65
)

elo_matches <- elo_results$matches

elo_rankings <- elo_results$final_ratings

print(elo_rankings, n = Inf)
stopifnot(
  nrow(elo_matches) == 3040,
  
  !anyNA(elo_matches$elo_home),
  
  !anyNA(elo_matches$elo_away),
  
  all(is.finite(elo_matches$elo_home)),
  
  all(is.finite(elo_matches$elo_away)),
  
  abs(mean(elo_rankings$elo) - 1500) < 1e-8
)
# Visualise History ----
elo_history <- bind_rows(
  
  elo_matches %>%
    transmute(
      week,
      team = home_key,
      elo = elo_home
    ),
  
  elo_matches %>%
    transmute(
      week,
      team = away_key,
      elo = elo_away
    )
  
) %>%
  distinct(week, team, elo)
big_six_keys <- c(
  "arsenal",
  "chelsea",
  "liverpool",
  "mancity",
  "manunited",
  "tottenham"
)

elo_history %>%
  filter(team %in% big_six_keys) %>%
  ggplot(
    aes(
      x = week,
      y = elo,
      colour = team
    )
  ) +
  geom_line(linewidth = 0.8) +
  geom_hline(
    yintercept = 1500,
    linetype = "dashed"
  ) +
  labs(
    title = "The Changing Strength of the Big Six",
    subtitle = "Dynamic Elo ratings, 2016–2024",
    x = NULL,
    y = "Elo rating",
    colour = "Club"
  ) +
  theme_minimal()
# Restructure Data ----
team_elo <- bind_rows(
  
  elo_matches %>%
    transmute(
      date,
      week,
      home_team,
      away_team,
      team = home_key,
      opponent = away_key,
      home = 1L,
      goals = home_goals,
      elo_for = elo_home,
      elo_against = elo_away
    ),
  
  elo_matches %>%
    transmute(
      date,
      week,
      home_team,
      away_team,
      team = away_key,
      opponent = home_key,
      home = 0L,
      goals = away_goals,
      elo_for = elo_away,
      elo_against = elo_home
    )
  
) %>%
  mutate(
    elo_for = (elo_for - 1500) / 400,
    
    elo_against = (elo_against - 1500) / 400
  ) %>%
  filter(week >= as.Date("2018-07-01"))
# Define Model Spec ----
elo_formulas <- list(
  
  M3W = goals ~ home + team + opponent,
  
  M9A = goals ~ home +
    elo_for +
    elo_against,
  
  M9B = goals ~ home + team + opponent +
    elo_for +
    elo_against
)
# Load Validation Schedule ----
baseline_predictions <- read_csv(
  here(
    "outputs", "validation",
    "baseline_predictions.csv"
  ),
  show_col_types = FALSE
)

validation_weeks <- baseline_predictions %>%
  distinct(prediction_date) %>%
  arrange(prediction_date) %>%
  pull(prediction_date)
# Walk Forward ----
predict_elo_models <- function(cutoff) {
  
  training <- team_elo %>%
    filter(date < cutoff) %>%
    mutate(
      match_weight = 2^(
        -as.numeric(cutoff - date) / 180
      )
    )
  
  testing <- team_elo %>%
    filter(week == cutoff) %>%
    inner_join(
      
      baseline_predictions %>%
        filter(prediction_date == cutoff) %>%
        select(
          date,
          home_team,
          away_team,
          scoreline,
          lambda_home,
          lambda_away
        ),
      
      by = c(
        "date",
        "home_team",
        "away_team"
      ),
      
      relationship = "many-to-one"
    )
  
  models <- map(
    elo_formulas,
    
    function(specification) {
      
      model <- glm(
        formula = specification,
        family = poisson(link = "log"),
        data = training,
        weights = match_weight
      )
      
      stopifnot(model$converged)
      
      model
    }
  )
  
  known_teams <- unique(training$team)
  
  known_fixture <-
    testing$team %in% known_teams &
    testing$opponent %in% known_teams
  
  fallback_rates <- if_else(
    testing$home == 1L,
    testing$lambda_home,
    testing$lambda_away
  )
  
  map_dfr(
    
    names(models),
    
    function(model_name) {
      
      model <- models[[model_name]]
      
      if (model_name == "M9A") {
        
        rates <- predict(
          model,
          newdata = testing,
          type = "response"
        )
        
      } else {
        
        rates <- fallback_rates
        
        if (any(known_fixture)) {
          
          rates[known_fixture] <- predict(
            model,
            newdata = testing[known_fixture, ],
            type = "response"
          )
        }
      }
      
      testing %>%
        transmute(
          date,
          week,
          home_team,
          away_team,
          scoreline,
          home,
          goals,
          model = model_name,
          rate = rates
        )
    }
  )
}
# Run Validation ----
elo_predictions <- map_dfr(
  validation_weeks,
  predict_elo_models
) %>%
  pivot_wider(
    names_from = home,
    values_from = c(goals, rate),
    names_glue = "{.value}_{home}"
  ) %>%
  mutate(
    
    prediction = paste0(
      floor(rate_1),
      "-",
      floor(rate_0)
    ),
    
    correct = prediction == scoreline,
    
    log_loss = -(
      dpois(goals_1, rate_1, log = TRUE) +
        dpois(goals_0, rate_0, log = TRUE)
    )
  )
stopifnot(
  nrow(elo_predictions) == 594 * 3,
  
  all(count(elo_predictions, model)$n == 594),
  
  !anyNA(elo_predictions$prediction),
  
  all(is.finite(elo_predictions$log_loss)),
  
  all(elo_predictions$rate_1 > 0),
  
  all(elo_predictions$rate_0 > 0)
)
# Evaluate Performance ----
elo_performance <- elo_predictions %>%
  group_by(model) %>%
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(correct),
    
    exact_accuracy = mean(correct) * 100,
    
    log_loss = mean(log_loss),
    
    expected_home_mae = mean(
      abs(goals_1 - rate_1)
    ),
    
    expected_away_mae = mean(
      abs(goals_0 - rate_0)
    ),
    
    .groups = "drop"
  ) %>%
  arrange(log_loss)

elo_performance %>%
  mutate(
    log_loss = sprintf("%.6f", log_loss)
  ) %>%
  print(width = Inf)
# Save ----
write_csv(
  elo_predictions,
  here(
    "outputs", "validation",
    "elo_poisson_predictions.csv"
  )
)

write_csv(
  elo_performance,
  here(
    "outputs", "validation",
    "elo_poisson_performance.csv"
  )
)

write_csv(
  elo_history,
  here(
    "data", "processed",
    "elo_rating_history.csv"
  )
)
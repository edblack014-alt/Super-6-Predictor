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

# Restructure Data ----
create_team_data <- function(matches) {
  
  home_data <- matches %>%
    transmute(
      date,
      season,
      team = home_key,
      opponent = away_key,
      home = 1,
      goals = home_goals
    )
  
  away_data <- matches %>%
    transmute(
      date,
      season,
      team = away_key,
      opponent = home_key,
      home = 0,
      goals = away_goals
    )
  
  bind_rows(
    home_data,
    away_data
  ) %>%
    arrange(date)
}

team_data <- create_team_data(development_data)

glimpse(team_data)

dim(team_data)
# Fit Using Initial Training Data ----
initial_training <- team_data %>%
  filter(date < as.Date("2021-07-01"))

poisson_model <- glm(
  goals ~ home + team + opponent,
  family = poisson(link = "log"),
  data = initial_training
)

summary(poisson_model)
home_coefficient <- coef(poisson_model)["home"]

home_multiplier <- exp(home_coefficient)

home_effect_pct <- (home_multiplier - 1) * 100

print(home_coefficient)

print(home_multiplier)

print(home_effect_pct)
# Inspect Model Dispersion ----
pearson_dispersion <- sum(
  residuals(
    poisson_model,
    type = "pearson"
  )^2
) / df.residual(poisson_model)

print(pearson_dispersion)

# Implement Weekly Walk-Forward Validation ----
predict_poisson <- function(cutoff, half_life = Inf) {
  
  # Training data: all previous Premier League matches
  training <- team_data %>%
    filter(date < cutoff) %>%
    mutate(
      days_ago = as.numeric(cutoff - date),
      
      match_weight = 2^(-days_ago / half_life)
    )
  
  # Testing data: identical fixtures to baseline models
  testing <- baseline_predictions %>%
    filter(prediction_date == cutoff) %>%
    left_join(
      development_data %>%
        select(
          date,
          home_team,
          away_team,
          home_key,
          away_key,
          home_goals,
          away_goals
        ),
      by = c("date", "home_team", "away_team"),
      relationship = "one-to-one"
    )
  
  # Fit Weighted Poisson Model ----
  
  model <- glm(
    goals ~ home + team + opponent,
    family = poisson(link = "log"),
    data = training,
    weights = match_weight
  )
  
  known_teams <- unique(training$team)
  
  # Identify fixtures with sufficient team history
  known_fixture <-
    testing$home_key %in% known_teams &
    testing$away_key %in% known_teams
  
  # Start with M2 fallback rates
  testing$lambda_home_m3 <- testing$lambda_home
  testing$lambda_away_m3 <- testing$lambda_away
  
  # Replace fallback rates with M3 estimates
  if (any(known_fixture)) {
    
    home_newdata <- testing[known_fixture, ] %>%
      transmute(
        team = home_key,
        opponent = away_key,
        home = 1
      )
    
    away_newdata <- testing[known_fixture, ] %>%
      transmute(
        team = away_key,
        opponent = home_key,
        home = 0
      )
    
    testing$lambda_home_m3[known_fixture] <- predict(
      model,
      newdata = home_newdata,
      type = "response"
    )
    
    testing$lambda_away_m3[known_fixture] <- predict(
      model,
      newdata = away_newdata,
      type = "response"
    )
  }
  
  testing %>%
    mutate(
      
      fallback_used = !known_fixture,
      
      prediction_m3 = paste0(
        floor(lambda_home_m3),
        "-",
        floor(lambda_away_m3)
      ),
      
      probability_m3 =
        dpois(floor(lambda_home_m3), lambda_home_m3) *
        dpois(floor(lambda_away_m3), lambda_away_m3),
      
      log_loss_m3 = -(
        dpois(home_goals, lambda_home_m3, log = TRUE) +
          dpois(away_goals, lambda_away_m3, log = TRUE)
      ),
      
      log_loss_m2 = -(
        dpois(home_goals, lambda_home, log = TRUE) +
          dpois(away_goals, lambda_away, log = TRUE)
      )
    )
}
poisson_predictions <- map_dfr(
  validation_weeks,
  predict_poisson
)

stopifnot(
  nrow(poisson_predictions) ==
    nrow(baseline_predictions),
  
  !anyNA(poisson_predictions$prediction_m3),
  
  all(is.finite(poisson_predictions$log_loss_m3))
)

View(poisson_predictions)
# Compare all Four Models ----
model_performance <- poisson_predictions %>%
  pivot_longer(
    cols = c(
      prediction_m0,
      prediction_m1,
      prediction_m2,
      prediction_m3
    ),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  group_by(model) %>%
  summarise(
    matches = n(),
    correct_scorelines = sum(prediction == scoreline),
    accuracy = mean(prediction == scoreline) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(accuracy))

print(model_performance)

# Introduce Log Loss ----
probability_performance <- poisson_predictions %>%
  summarise(
    matches = n(),
    m2_log_loss = mean(log_loss_m2),
    m3_log_loss = mean(log_loss_m3),
    fallback_matches = sum(fallback_used)
  )

print(probability_performance)
# Time-Weighted Poisson Experiment ----

half_lives <- c(
  90,
  180,
  365,
  730,
  1095,
  Inf
)

weighted_predictions <- map_dfr(
  half_lives,
  
  function(h) {
    
    message(
      "Evaluating half-life: ",
      h,
      " days"
    )
    
    map_dfr(
      validation_weeks,
      
      function(cutoff) {
        
        predict_poisson(
          cutoff = cutoff,
          half_life = h
        ) %>%
          mutate(
            half_life = h
          )
      }
    )
  }
)

weighted_predictions %>%
  count(half_life)

stopifnot(
  nrow(weighted_predictions) ==
    length(half_lives) *
    nrow(baseline_predictions),
  
  !anyNA(weighted_predictions$prediction_m3),
  
  all(is.finite(weighted_predictions$log_loss_m3)),
  
  all(is.finite(weighted_predictions$lambda_home_m3)),
  
  all(is.finite(weighted_predictions$lambda_away_m3))
)
weighted_performance <- weighted_predictions %>%
  group_by(half_life) %>%
  summarise(
    
    matches = n(),
    
    correct_scorelines =
      sum(prediction_m3 == scoreline),
    
    exact_accuracy =
      mean(prediction_m3 == scoreline) * 100,
    
    log_loss =
      mean(log_loss_m3),
    
    expected_home_mae =
      mean(abs(home_goals - lambda_home_m3)),
    
    expected_away_mae =
      mean(abs(away_goals - lambda_away_m3)),
    
    fallback_matches =
      sum(fallback_used),
    
    .groups = "drop"
    
  ) %>%
  arrange(log_loss)

print(weighted_performance, width = Inf)

# Visualise Hyperparameter Performance ----
weighted_performance %>%
  mutate(
    half_life_label = factor(
      as.character(half_life),
      levels = as.character(half_lives)
    )
  ) %>%
  ggplot(
    aes(
      x = half_life_label,
      y = log_loss,
      group = 1
    )
  ) +
  geom_line(
    colour = "#398469",
    linewidth = 1
  ) +
  geom_point(
    colour = "#398469",
    size = 3
  ) +
  geom_hline(
    yintercept = mean(
      poisson_predictions$log_loss_m2
    ),
    linetype = "dashed"
  ) +
  labs(
    title = "Optimising the Poisson Model Half-Life",
    subtitle = "Dashed line represents M2 performance",
    x = "Half-life (days)",
    y = "Out-of-sample log loss"
  ) +
  theme_minimal()

# Assess Performance Across Seasons----
weighted_seasonal_performance <- weighted_predictions %>%
  group_by(half_life, season) %>%
  summarise(
    
    matches = n(),
    
    correct_scorelines =
      sum(prediction_m3 == scoreline),
    
    exact_accuracy =
      mean(prediction_m3 == scoreline) * 100,
    
    log_loss =
      mean(log_loss_m3),
    
    .groups = "drop"
  )

print(weighted_seasonal_performance, n = Inf)
# Save the Results ----
write_csv(
  poisson_predictions,
  here("outputs", "validation", "poisson_predictions.csv")
)

write_csv(
  model_performance,
  here("outputs", "validation", "model_performance.csv")
)

write_csv(
  probability_performance,
  here("outputs", "validation", "probability_performance.csv")
)
write_csv(
  weighted_predictions,
  here(
    "outputs", "validation",
    "weighted_poisson_predictions.csv"
  )
)

write_csv(
  weighted_performance,
  here(
    "outputs", "validation",
    "weighted_poisson_performance.csv"
  )
)
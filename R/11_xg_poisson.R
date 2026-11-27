# Load Packages and Datasets ----
library(tidyverse)
library(lubridate)
library(here)

xg_feature_data <- readRDS(
  here(
    "data", "processed",
    "xg_feature_data.rds"
  )
)

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
# Define all Five Model Specs ----
model_formulas <- list(
  
  M3W = goals ~ home + team + opponent,
  
  M6A = goals ~ home +
    log(recent_gf) +
    log(opponent_ga),
  
  M6B = goals ~ home + team + opponent +
    log(recent_gf) +
    log(opponent_ga),
  
  M7A = goals ~ home +
    log(recent_xgf) +
    log(opponent_xga),
  
  M7B = goals ~ home + team + opponent +
    log(recent_xgf) +
    log(opponent_xga)
  
)
# Fit Initial Models ----
initial_cutoff <- as.Date("2021-07-01")

initial_training <- xg_feature_data %>%
  filter(date < initial_cutoff) %>%
  mutate(
    days_ago = as.numeric(initial_cutoff - date),
    match_weight = 2^(-days_ago / 180)
  )

initial_models <- map(
  model_formulas,
  
  function(specification) {
    
    glm(
      formula = specification,
      family = poisson(link = "log"),
      data = initial_training,
      weights = match_weight
    )
    
  }
)
summary(initial_models$M7A)

summary(initial_models$M7B)

xg_coefficients <- tibble(
  
  variable = c(
    "home",
    "log(recent_xgf)",
    "log(opponent_xga)"
  ),
  
  m7a = unname(
    coef(initial_models$M7A)[variable]
  ),
  
  m7b = unname(
    coef(initial_models$M7B)[variable]
  )
  
)

print(xg_coefficients)

# Walk-forward ----
predict_xg_models <- function(cutoff) {
  
  training <- xg_feature_data %>%
    filter(date < cutoff) %>%
    mutate(
      days_ago = as.numeric(cutoff - date),
      match_weight = 2^(-days_ago / 180)
    )
  
  testing <- xg_feature_data %>%
    filter(
      week == cutoff,
      big_six_fixture
    )
  
  # Fit all model specifications
  
  models <- map(
    
    model_formulas,
    
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
  
  # Identify previously observed teams
  
  known_teams <- unique(training$team)
  
  known_fixture <-
    testing$team %in% known_teams &
    testing$opponent %in% known_teams
  
  # Common fallback for models with team effects
  
  fallback_rates <- predict(
    models$M6A,
    newdata = testing,
    type = "response"
  )
  
  # Generate predictions for each model
  
  map_dfr(
    
    names(models),
    
    function(model_name) {
      
      model <- models[[model_name]]
      
      uses_team_effects <- model_name %in%
        c("M3W", "M6B", "M7B")
      
      if (uses_team_effects) {
        
        rates <- fallback_rates
        
        if (any(known_fixture)) {
          
          rates[known_fixture] <- predict(
            model,
            newdata = testing[known_fixture, ],
            type = "response"
          )
        }
        
      } else {
        
        rates <- predict(
          model,
          newdata = testing,
          type = "response"
        )
      }
      
      testing %>%
        transmute(
          date,
          season,
          week,
          home_key,
          away_key,
          scoreline,
          home,
          goals,
          
          model = model_name,
          
          predicted_rate = rates
        )
    }
  )
}

xg_predictions_long <- map_dfr(
  validation_weeks,
  predict_xg_models
)
# Convert Back to Match Levels Predictions ----
xg_predictions <- xg_predictions_long %>%
  
  pivot_wider(
    
    names_from = home,
    
    values_from = c(
      goals,
      predicted_rate
    ),
    
    names_glue = "{.value}_{home}"
    
  ) %>%
  
  mutate(
    
    prediction = paste0(
      floor(predicted_rate_1),
      "-",
      floor(predicted_rate_0)
    ),
    
    log_loss = -(
      dpois(
        goals_1,
        predicted_rate_1,
        log = TRUE
      ) +
        dpois(
          goals_0,
          predicted_rate_0,
          log = TRUE
        )
    ),
    
    correct = prediction == scoreline
    
  )
# Validate Results ----
stopifnot(
  
  nrow(xg_predictions) == 594 * 5,
  
  !anyNA(xg_predictions$prediction),
  
  all(is.finite(xg_predictions$log_loss)),
  
  all(is.finite(xg_predictions$predicted_rate_1)),
  
  all(is.finite(xg_predictions$predicted_rate_0)),
  
  all(xg_predictions$predicted_rate_1 > 0),
  
  all(xg_predictions$predicted_rate_0 > 0)
  
)

xg_predictions %>%
  count(model)
# Evaluate xG models----
xg_model_performance <- xg_predictions %>%
  
  group_by(model) %>%
  
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(correct),
    
    exact_accuracy = mean(correct) * 100,
    
    log_loss = mean(log_loss),
    
    expected_home_mae = mean(
      abs(goals_1 - predicted_rate_1)
    ),
    
    expected_away_mae = mean(
      abs(goals_0 - predicted_rate_0)
    ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(log_loss)

print(xg_model_performance, width = Inf)
xg_model_performance %>%
  mutate(
    log_loss = sprintf("%.6f", log_loss)
  ) %>%
  print(width = Inf)
# Save Results ----
write_csv(
  xg_predictions,
  here(
    "outputs", "validation",
    "xg_poisson_predictions.csv"
  )
)

write_csv(
  xg_model_performance,
  here(
    "outputs", "validation",
    "xg_poisson_performance.csv"
  )
)

print(xg_feature_validation, width = Inf)

print(xg_correlations, width = Inf)

print(xg_coefficients)

print(xg_model_performance, width = Inf)

xg_model_performance %>%
  mutate(log_loss = sprintf("%.6f", log_loss)) %>%
  print(width = Inf)


# Paired Block Bootstrap ----

paired_xg_results <- xg_predictions %>%
  filter(model %in% c("M3W", "M7B")) %>%
  select(
    date,
    week,
    home_key,
    away_key,
    model,
    log_loss,
    correct
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = c(log_loss, correct)
  ) %>%
  mutate(
    loss_improvement =
      log_loss_M3W - log_loss_M7B,
    
    accuracy_improvement =
      as.integer(correct_M7B) -
      as.integer(correct_M3W)
  )

stopifnot(
  nrow(paired_xg_results) == 594,
  !anyNA(paired_xg_results)
)
# Aggregate by Prediction Week ----
weekly_differences <- paired_xg_results %>%
  group_by(week) %>%
  summarise(
    matches = n(),
    
    total_loss_improvement =
      sum(loss_improvement),
    
    total_accuracy_improvement =
      sum(accuracy_improvement),
    
    .groups = "drop"
  ) %>%
  arrange(week)
# Run Simulations ----
set.seed(42)

n_weeks <- nrow(weekly_differences)
block_length <- 4
n_bootstrap <- 5000

bootstrap_results <- map_dfr(
  seq_len(n_bootstrap),
  
  function(i) {
    
    starts <- sample(
      seq_len(n_weeks - block_length + 1),
      size = ceiling(n_weeks / block_length),
      replace = TRUE
    )
    
    indices <- unlist(
      lapply(
        starts,
        function(s) {
          seq(s, length.out = block_length)
        }
      )
    )
    
    sampled <- weekly_differences[
      indices[seq_len(n_weeks)], 
    ]
    
    tibble(
      
      loss_improvement =
        sum(sampled$total_loss_improvement) /
        sum(sampled$matches),
      
      accuracy_improvement =
        100 *
        sum(sampled$total_accuracy_improvement) /
        sum(sampled$matches)
      
    )
  }
)
# Calculate Bootstrap Confidence Intervals ----
bootstrap_summary <- bootstrap_results %>%
  summarise(
    
    loss_lower = quantile(
      loss_improvement, 0.025
    ),
    
    loss_upper = quantile(
      loss_improvement, 0.975
    ),
    
    accuracy_lower = quantile(
      accuracy_improvement, 0.025
    ),
    
    accuracy_upper = quantile(
      accuracy_improvement, 0.975
    )
    
  )

print(bootstrap_summary, width = Inf)
ggplot(
  bootstrap_results,
  aes(x = loss_improvement)
) +
  geom_histogram(
    bins = 50,
    fill = "#398469",
    colour = "white"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mean(
      paired_xg_results$loss_improvement
    ),
    colour = "red"
  ) +
  labs(
    title = "Does Expected Goals Improve Forecasts?",
    subtitle = "5,000 paired block bootstrap simulations",
    x = "Reduction in log loss",
    y = "Frequency"
  ) +
  theme_minimal()
print(bootstrap_summary, width = Inf)


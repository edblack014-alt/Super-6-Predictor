library(tidyverse)
library(lubridate)
library(here)

feature_data <- readRDS(
  here(
    "data", "processed",
    "team_features.rds"
  )
)

initial_cutoff <- as.Date("2021-07-01")

initial_training <- feature_data %>%
  filter(date < initial_cutoff) %>%
  mutate(
    days_ago = as.numeric(initial_cutoff - date),
    match_weight = 2^(-days_ago / 180)
  )

# Form Only Model ----
model_m6a <- glm(
  goals ~ home +
    log(recent_gf) +
    log(opponent_ga),
  
  family = poisson(link = "log"),
  data = initial_training,
  weights = match_weight
)

summary(model_m6a)
# Hybrid Only Model ----
model_m6b <- glm(
  goals ~ home +
    team +
    opponent +
    log(recent_gf) +
    log(opponent_ga),
  
  family = poisson(link = "log"),
  data = initial_training,
  weights = match_weight
)

summary(model_m6b)
# Coeffecients of Interest ----
form_coefficients <- tibble(
  
  variable = c(
    "home",
    "log(recent_gf)",
    "log(opponent_ga)"
  ),
  
  m6a = unname(
    coef(model_m6a)[variable]
  ),
  
  m6b = unname(
    coef(model_m6b)[variable]
  )
)

print(form_coefficients)
# Walk Forward Validation ----
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
# Weekly Model Function ----
predict_dynamic <- function(cutoff) {
  
  training <- feature_data %>%
    filter(date < cutoff) %>%
    mutate(
      days_ago = as.numeric(cutoff - date),
      match_weight = 2^(-days_ago / 180)
    )
  
  testing <- feature_data %>%
    filter(
      week == cutoff,
      big_six_fixture
    )
  
  # M6-A: Recent form model
  
  model_a <- glm(
    goals ~ home +
      log(recent_gf) +
      log(opponent_ga),
    
    family = poisson(link = "log"),
    data = training,
    weights = match_weight
  )
  
  # M6-B: Hybrid model
  
  model_b <- glm(
    goals ~ home +
      team +
      opponent +
      log(recent_gf) +
      log(opponent_ga),
    
    family = poisson(link = "log"),
    data = training,
    weights = match_weight
  )
  
  stopifnot(
    model_a$converged,
    model_b$converged
  )
  
  # Generate predictions
  
  testing$rate_m6a <- predict(
    model_a,
    newdata = testing,
    type = "response"
  )
  
  # Use M6-A as fallback for unknown teams
  
  testing$rate_m6b <- testing$rate_m6a
  
  known_teams <- unique(training$team)
  
  known_fixture <-
    testing$team %in% known_teams &
    testing$opponent %in% known_teams
  
  if (any(known_fixture)) {
    
    testing$rate_m6b[known_fixture] <- predict(
      model_b,
      newdata = testing[known_fixture, ],
      type = "response"
    )
  }
  
  # Convert team predictions back to match level
  
  testing %>%
    select(
      date,
      season,
      week,
      home_key,
      away_key,
      scoreline,
      home,
      goals,
      rate_m6a,
      rate_m6b
    ) %>%
    pivot_wider(
      names_from = home,
      values_from = c(
        goals,
        rate_m6a,
        rate_m6b
      ),
      names_glue = "{.value}_{home}"
    ) %>%
    mutate(
      
      prediction_m6a = paste0(
        floor(rate_m6a_1), "-",
        floor(rate_m6a_0)
      ),
      
      prediction_m6b = paste0(
        floor(rate_m6b_1), "-",
        floor(rate_m6b_0)
      ),
      
      log_loss_m6a = -(
        dpois(goals_1, rate_m6a_1, log = TRUE) +
          dpois(goals_0, rate_m6a_0, log = TRUE)
      ),
      
      log_loss_m6b = -(
        dpois(goals_1, rate_m6b_1, log = TRUE) +
          dpois(goals_0, rate_m6b_0, log = TRUE)
      )
    )
}
# Execute ----
dynamic_predictions <- map_dfr(
  validation_weeks,
  predict_dynamic
)

stopifnot(
  nrow(dynamic_predictions) == 594,
  
  !anyNA(dynamic_predictions$prediction_m6a),
  
  !anyNA(dynamic_predictions$prediction_m6b),
  
  all(is.finite(dynamic_predictions$log_loss_m6a)),
  
  all(is.finite(dynamic_predictions$log_loss_m6b))
)

# Evaluate Two Models ----
dynamic_performance <- dynamic_predictions %>%
  summarise(
    
    matches = n(),
    
    m6a_correct = sum(
      prediction_m6a == scoreline
    ),
    
    m6a_accuracy = mean(
      prediction_m6a == scoreline
    ) * 100,
    
    m6a_log_loss = mean(log_loss_m6a),
    
    m6b_correct = sum(
      prediction_m6b == scoreline
    ),
    
    m6b_accuracy = mean(
      prediction_m6b == scoreline
    ) * 100,
    
    m6b_log_loss = mean(log_loss_m6b)
  )

print(dynamic_performance, width = Inf)

# Save Results ----
write_csv(
  dynamic_predictions,
  here(
    "outputs", "validation",
    "dynamic_poisson_predictions.csv"
  )
)

write_csv(
  dynamic_performance,
  here(
    "outputs", "validation",
    "dynamic_poisson_performance.csv"
  )
)
# Load Data and Packages ----

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

weighted_predictions <- read_csv(
  here(
    "outputs", "validation",
    "weighted_poisson_predictions.csv"
  ),
  show_col_types = FALSE
)

validation_weeks <- weighted_predictions %>%
  distinct(prediction_date) %>%
  arrange(prediction_date) %>%
  pull(prediction_date)
# Create Dixon-Coles Adjustment Function ----
dc_tau <- function(x, y, lambda_home,
                   lambda_away, rho) {
  
  case_when(
    
    x == 0 & y == 0 ~
      1 - lambda_home * lambda_away * rho,
    
    x == 0 & y == 1 ~
      1 + lambda_home * rho,
    
    x == 1 & y == 0 ~
      1 + lambda_away * rho,
    
    x == 1 & y == 1 ~
      1 - rho,
    
    TRUE ~ 1
  )
}
# Test Function ----
dc_tau(
  x = c(0, 0, 1, 1, 2),
  y = c(0, 1, 0, 1, 1),
  lambda_home = 1.6,
  lambda_away = 1.2,
  rho = -0.1
)

# Estimate Historical Scoring Rates ----
create_dc_training <- function(cutoff, half_life) {
  
  training_matches <- development_data %>%
    filter(date < cutoff) %>%
    mutate(
      days_ago = as.numeric(cutoff - date),
      match_weight = 2^(-days_ago / half_life)
    )
  
  home_data <- training_matches %>%
    transmute(
      team = home_key,
      opponent = away_key,
      home = 1,
      goals = home_goals,
      match_weight
    )
  
  away_data <- training_matches %>%
    transmute(
      team = away_key,
      opponent = home_key,
      home = 0,
      goals = away_goals,
      match_weight
    )
  
  team_training <- bind_rows(
    home_data,
    away_data
  )
  
  model <- glm(
    goals ~ home + team + opponent,
    family = poisson(link = "log"),
    data = team_training,
    weights = match_weight
  )
  
  stopifnot(model$converged)
  
  training_matches %>%
    mutate(
      
      lambda_home = predict(
        model,
        newdata = home_data,
        type = "response"
      ),
      
      lambda_away = predict(
        model,
        newdata = away_data,
        type = "response"
      )
    )
}
# Estimate Optimal rho ----
estimate_rho <- function(training,
                         forecast_home,
                         forecast_away) {
  
  # Ensure valid probabilities for historical
  # and forthcoming fixtures
  
  all_home <- c(
    training$lambda_home,
    forecast_home
  )
  
  all_away <- c(
    training$lambda_away,
    forecast_away
  )
  
  lower_bound <- -0.999 / max(
    c(all_home, all_away)
  )
  
  upper_bound <- 0.999 * min(
    1,
    1 / max(all_home * all_away)
  )
  
  # Negative weighted log likelihood
  
  objective <- function(rho) {
    
    adjustment <- dc_tau(
      training$home_goals,
      training$away_goals,
      training$lambda_home,
      training$lambda_away,
      rho
    )
    
    if (any(adjustment <= 0)) {
      return(Inf)
    }
    
    -sum(
      training$match_weight *
        log(adjustment)
    )
  }
  
  result <- optimize(
    objective,
    interval = c(
      lower_bound,
      upper_bound
    )
  )
  
  result$minimum
}
# Generate Dixon-Coles Scoreline Predictions ----
predict_dc_scoreline <- function(lambda_home,
                                 lambda_away,
                                 rho) {
  
  probabilities <- expand_grid(
    
    home_goals = 0:max(
      2, floor(lambda_home)
    ),
    
    away_goals = 0:max(
      2, floor(lambda_away)
    )
    
  ) %>%
    mutate(
      
      poisson_probability =
        dpois(home_goals, lambda_home) *
        dpois(away_goals, lambda_away),
      
      adjustment = dc_tau(
        home_goals,
        away_goals,
        lambda_home,
        lambda_away,
        rho
      ),
      
      dc_probability =
        poisson_probability * adjustment,
      
      scoreline = paste0(
        home_goals, "-", away_goals
      )
    )
  
  probabilities %>%
    arrange(desc(dc_probability),
            home_goals, away_goals) %>%
    slice_head(n = 1) %>%
    pull(scoreline)
}
# Test Predictions Function ----
predict_dc_scoreline(
  lambda_home = 1.6,
  lambda_away = 1.2,
  rho = -0.1
)
# Walk-Forward Validation ----
predict_dixon_coles <- function(cutoff, h) {
  
  # Retrieve existing pre-match Poisson forecasts
  
  testing <- weighted_predictions %>%
    filter(
      prediction_date == cutoff,
      half_life == h
    )
  
  stopifnot(nrow(testing) > 0)
  
  # Estimate historical scoring rates
  
  training <- create_dc_training(
    cutoff = cutoff,
    half_life = h
  )
  
  # Estimate rho from historical matches
  
  rho <- estimate_rho(
    training = training,
    forecast_home = testing$lambda_home_m3,
    forecast_away = testing$lambda_away_m3
  )
  
  # Generate adjusted predictions
  
  testing %>%
    mutate(
      
      rho_dc = rho,
      
      prediction_dc = pmap_chr(
        
        list(
          lambda_home_m3,
          lambda_away_m3,
          rho_dc
        ),
        
        predict_dc_scoreline
      ),
      
      actual_adjustment = dc_tau(
        home_goals,
        away_goals,
        lambda_home_m3,
        lambda_away_m3,
        rho_dc
      ),
      
      log_loss_dc =
        log_loss_m3 - log(actual_adjustment)
      
    )
}
# Run Experiment ----
dc_half_lives <- c(
  180,
  365,
  Inf
)

dc_predictions <- map_dfr(
  dc_half_lives,
  
  function(h) {
    
    message(
      "Evaluating Dixon-Coles: ",
      h,
      " days"
    )
    
    map_dfr(
      
      validation_weeks,
      
      function(cutoff) {
        
        predict_dixon_coles(
          cutoff = cutoff,
          h = h
        )
      }
    )
  }
)
# Validate Output ----
stopifnot(
  
  nrow(dc_predictions) == 594 * 3,
  
  !anyNA(dc_predictions$prediction_dc),
  
  all(is.finite(dc_predictions$log_loss_dc)),
  
  all(dc_predictions$actual_adjustment > 0)
  
)
# Evaluate Dixon-Coles Performance ----
dc_performance <- dc_predictions %>%
  group_by(half_life) %>%
  summarise(
    
    matches = n(),
    
    poisson_correct =
      sum(prediction_m3 == scoreline),
    
    dc_correct =
      sum(prediction_dc == scoreline),
    
    poisson_accuracy =
      mean(prediction_m3 == scoreline) * 100,
    
    dc_accuracy =
      mean(prediction_dc == scoreline) * 100,
    
    poisson_log_loss = mean(log_loss_m3),
    
    dc_log_loss = mean(log_loss_dc),
    
    log_loss_improvement =
      poisson_log_loss - dc_log_loss,
    
    changed_predictions =
      sum(prediction_dc != prediction_m3),
    
    .groups = "drop"
    
  ) %>%
  arrange(dc_log_loss)

print(dc_performance, width = Inf)
# Investigate Estimated rho Parameter----
rho_history <- dc_predictions %>%
  distinct(
    prediction_date,
    half_life,
    rho_dc
  )

rho_summary <- rho_history %>%
  group_by(half_life) %>%
  summarise(
    
    mean_rho = mean(rho_dc),
    
    median_rho = median(rho_dc),
    
    min_rho = min(rho_dc),
    
    max_rho = max(rho_dc),
    
    negative_rho_pct =
      mean(rho_dc < 0) * 100,
    
    .groups = "drop"
  )

print(rho_summary, width = Inf)
ggplot(
  rho_history,
  aes(
    x = prediction_date,
    y = rho_dc,
    colour = factor(half_life)
  )
) +
  geom_line(linewidth = 0.8) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Evolution of the Dixon-Coles Parameter",
    subtitle = "Weekly walk-forward estimation",
    x = NULL,
    y = "Estimated rho",
    colour = "Half-life"
  ) +
  theme_minimal()
# Save Results----
write_csv(
  dc_predictions,
  here(
    "outputs", "validation",
    "dixon_coles_predictions.csv"
  )
)

write_csv(
  dc_performance,
  here(
    "outputs", "validation",
    "dixon_coles_performance.csv"
  )
)

write_csv(
  rho_history,
  here(
    "outputs", "validation",
    "dixon_coles_rho_history.csv"
  )
)
# Load Packages and Previous Predictions ----
library(tidyverse)
library(here)

read_predictions <- function(filename) {
  
  read_csv(
    here("outputs", "validation", filename),
    show_col_types = FALSE
  )
}

baseline <- read_predictions(
  "baseline_predictions.csv"
)

weighted <- read_predictions(
  "weighted_poisson_predictions.csv"
)

xg <- read_predictions(
  "xg_poisson_predictions.csv"
)

elo <- read_predictions(
  "elo_poisson_predictions.csv"
)

market <- read_predictions(
  "market_poisson_predictions.csv"
)

master <- readRDS(
  here(
    "data", "processed",
    "premier_league_master.rds"
  )
)
# Create Common Ensemble Dataset ----
match_keys <- c(
  "date",
  "home_team",
  "away_team"
)

ensemble_data <- baseline %>%
  select(
    all_of(match_keys),
    season,
    prediction_date,
    fixture_type,
    scoreline,
    lambda_home,
    lambda_away
  ) %>%
  rename(
    M2_h = lambda_home,
    M2_a = lambda_away
  ) %>%
  left_join(
    master %>%
      select(
        all_of(match_keys),
        home_goals,
        away_goals
      ),
    by = match_keys,
    relationship = "one-to-one"
  )
# Add Each Model's Prediction ----
add_model_rates <- function(
    data,
    source,
    model_name,
    home_column,
    away_column
) {
  
  rates <- source %>%
    select(
      all_of(match_keys),
      all_of(c(home_column, away_column))
    )
  
  names(rates) <- c(
    match_keys,
    paste0(model_name, "_h"),
    paste0(model_name, "_a")
  )
  
  data %>%
    left_join(
      rates,
      by = match_keys,
      relationship = "one-to-one"
    )
}
ensemble_data <- add_model_rates(
  data = ensemble_data,
  
  source = weighted %>%
    filter(half_life == 180),
  
  model_name = "M3W",
  home_column = "lambda_home_m3",
  away_column = "lambda_away_m3"
)
ensemble_data <- add_model_rates(
  data = ensemble_data,
  
  source = elo %>%
    filter(model == "M9B"),
  
  model_name = "M9B",
  home_column = "rate_1",
  away_column = "rate_0"
)
xg_named <- xg %>%
  left_join(
    master %>%
      select(
        date,
        home_key,
        away_key,
        home_team,
        away_team
      ),
    by = c(
      "date",
      "home_key",
      "away_key"
    ),
    relationship = "many-to-one"
  )
ensemble_data <- add_model_rates(
  data = ensemble_data,
  source = xg_named %>%
    filter(model == "M7A"),
  model_name = "M7A",
  home_column = "predicted_rate_1",
  away_column = "predicted_rate_0"
)

ensemble_data <- add_model_rates(
  data = ensemble_data,
  source = xg_named %>%
    filter(model == "M7B"),
  model_name = "M7B",
  home_column = "predicted_rate_1",
  away_column = "predicted_rate_0"
)
ensemble_data <- add_model_rates(
  data = ensemble_data,
  source = market,
  model_name = "M8",
  home_column = "market_lambda_home",
  away_column = "market_lambda_away"
)
# Validate Integrated Dataset ----
candidate_models <- c(
  "M2",
  "M3W",
  "M9B",
  "M7A",
  "M7B",
  "M8"
)

rate_columns <- c(
  paste0(candidate_models, "_h"),
  paste0(candidate_models, "_a")
)

rate_matrix <- ensemble_data %>%
  select(all_of(rate_columns)) %>%
  as.matrix()

stopifnot(
  
  nrow(ensemble_data) == 594,
  
  !anyDuplicated(
    ensemble_data[match_keys]
  ),
  
  !anyNA(rate_matrix),
  
  all(is.finite(rate_matrix)),
  
  all(rate_matrix > 0),
  
  all(
    ensemble_data$scoreline ==
      paste0(
        ensemble_data$home_goals,
        "-",
        ensemble_data$away_goals
      )
  )
)

glimpse(ensemble_data)
# Find Optimal Ensemble Weights ----
generate_weight_grid <- function(
    n_models,
    total = 10L
) {
  
  if (n_models == 1L) {
    return(matrix(total, nrow = 1L))
  }
  
  do.call(
    
    rbind,
    
    lapply(
      
      0:total,
      
      function(i) {
        
        cbind(
          i,
          
          generate_weight_grid(
            n_models - 1L,
            total - i
          )
        )
      }
    )
  )
}
weight_grid_test <- generate_weight_grid(3) / 10

head(weight_grid_test)

dim(weight_grid_test)

stopifnot(
  all(abs(rowSums(weight_grid_test) - 1) < 1e-10)
)
calculate_actual_probabilities <- function(
    data,
    models
) {
  
  probabilities <- vapply(
    
    models,
    
    function(model) {
      
      dpois(
        data$home_goals,
        data[[paste0(model, "_h")]]
      ) *
        
        dpois(
          data$away_goals,
          data[[paste0(model, "_a")]]
        )
    },
    
    numeric(nrow(data))
  )
  
  colnames(probabilities) <- models
  
  probabilities
}
optimise_ensemble <- function(data, models) {
  
  actual_probs <- calculate_actual_probabilities(
    data,
    models
  )
  
  weights <- generate_weight_grid(
    length(models)
  ) / 10
  
  colnames(weights) <- models
  
  # Calculate ensemble probabilities for
  # every fixture and weight combination
  
  ensemble_probs <- actual_probs %*% t(weights)
  
  # Evaluate each combination
  
  losses <- colMeans(
    -log(ensemble_probs)
  )
  
  results <- as_tibble(weights) %>%
    mutate(log_loss = losses) %>%
    arrange(log_loss)
  
  best_index <- which.min(losses)
  
  best_weights <- weights[best_index, ]
  
  list(
    best_weights = best_weights,
    best_log_loss = losses[best_index],
    optimisation_results = results
  )
}
ensemble_specs <- list(
  
  M10A = c(
    "M2",
    "M3W",
    "M9B"
  ),
  
  M10B = c(
    "M2",
    "M3W",
    "M9B",
    "M7A",
    "M7B"
  ),
  
  M10C = c(
    "M2",
    "M3W",
    "M9B",
    "M8"
  )
)

ensemble_optimisation <- map(
  ensemble_specs,
  
  ~ optimise_ensemble(
    ensemble_data,
    .x
  )
)
ensemble_weights <- imap_dfr(
  
  ensemble_optimisation,
  
  function(result, ensemble_name) {
    
    tibble(
      ensemble = ensemble_name,
      model = names(result$best_weights),
      weight = as.numeric(result$best_weights)
    )
  }
)

print(ensemble_weights, n = Inf)
ensemble_optimisation$M10A$
  optimisation_results %>%
  slice_head(n = 10)

ensemble_optimisation$M10B$
  optimisation_results %>%
  slice_head(n = 10)

ensemble_optimisation$M10C$
  optimisation_results %>%
  slice_head(n = 10)
# Ensemble Scorelines ----
predict_ensemble_score <- function(
    home_rates,
    away_rates,
    weights
) {
  
  score_grid <- expand_grid(
    
    home_goals = 0:max(
      2, floor(home_rates)
    ),
    
    away_goals = 0:max(
      2, floor(away_rates)
    )
  )
  
  component_probs <- vapply(
    
    seq_along(weights),
    
    function(j) {
      
      dpois(
        score_grid$home_goals,
        home_rates[j]
      ) *
        
        dpois(
          score_grid$away_goals,
          away_rates[j]
        )
    },
    
    numeric(nrow(score_grid))
  )
  
  score_grid %>%
    mutate(
      
      probability = as.numeric(
        component_probs %*% weights
      ),
      
      scoreline = paste0(
        home_goals,
        "-",
        away_goals
      )
      
    ) %>%
    arrange(
      desc(probability),
      home_goals,
      away_goals
    ) %>%
    slice_head(n = 1) %>%
    select(scoreline, probability)
}
# Generate Predictions Across all Fixtures ----
evaluate_ensemble <- function(
    data,
    models,
    weights
) {
  
  home_rates <- as.matrix(
    data[paste0(models, "_h")]
  )
  
  away_rates <- as.matrix(
    data[paste0(models, "_a")]
  )
  
  actual_probs <- calculate_actual_probabilities(
    data,
    models
  )
  
  predicted_scores <- map_dfr(
    
    seq_len(nrow(data)),
    
    function(i) {
      
      predict_ensemble_score(
        home_rates = home_rates[i, ],
        away_rates = away_rates[i, ],
        weights = weights
      ) %>%
        rename(
          prediction = scoreline,
          prediction_probability = probability
        )
    }
  )
  
  actual_ensemble_probs <- as.numeric(
    actual_probs %*% weights
  )
  
  data %>%
    select(
      all_of(match_keys),
      season,
      prediction_date,
      fixture_type,
      scoreline,
      home_goals,
      away_goals
    ) %>%
    bind_cols(predicted_scores) %>%
    mutate(
      
      actual_probability =
        actual_ensemble_probs,
      
      log_loss = -log(
        actual_probability
      ),
      
      correct = prediction == scoreline
    )
}
ensemble_predictions <- imap_dfr(
  
  ensemble_specs,
  
  function(models, ensemble_name) {
    
    weights <- ensemble_optimisation[[ensemble_name]]$best_weights
    
    evaluate_ensemble(
      data = ensemble_data,
      models = models,
      weights = weights
    ) %>%
      mutate(
        ensemble = ensemble_name
      )
  }
)
stopifnot(
  
  nrow(ensemble_predictions) == 594 * 3,
  
  all(
    count(ensemble_predictions, ensemble)$n == 594
  ),
  
  !anyNA(ensemble_predictions$prediction),
  
  all(is.finite(ensemble_predictions$log_loss)),
  
  all(ensemble_predictions$actual_probability > 0),
  
  all(ensemble_predictions$actual_probability <= 1),
  
  all(ensemble_predictions$prediction_probability > 0),
  
  all(ensemble_predictions$prediction_probability <= 1)
)
# Evaluate Ensemble Performance ----
ensemble_performance <- ensemble_predictions %>%
  
  group_by(ensemble) %>%
  
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(correct),
    
    exact_accuracy = mean(correct) * 100,
    
    log_loss = mean(log_loss),
    
    average_confidence = mean(
      prediction_probability
    ) * 100,
    
    .groups = "drop"
    
  ) %>%
  
  arrange(log_loss)

ensemble_performance %>%
  mutate(
    log_loss = sprintf("%.6f", log_loss)
  ) %>%
  print(width = Inf)
ensemble_weights %>%
  ggplot(
    aes(
      x = ensemble,
      y = weight,
      fill = model
    )
  ) +
  geom_col(width = 0.6) +
  geom_text(
    aes(
      label = if_else(
        weight > 0,
        scales::percent(weight),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Optimal Ensemble Composition",
    subtitle = "Weights selected by validation log loss",
    x = NULL,
    y = "Model weight",
    fill = "Component"
  ) +
  theme_minimal()
# Ensemble Calibration ----
ensemble_calibration <- ensemble_predictions %>%
  
  mutate(
    confidence_band = cut(
      prediction_probability,
      
      breaks = c(
        0,
        0.075,
        0.10,
        0.125,
        0.15,
        0.20,
        0.30,
        1
      ),
      
      include.lowest = TRUE
    )
  ) %>%
  
  group_by(
    ensemble,
    confidence_band
  ) %>%
  
  summarise(
    
    matches = n(),
    
    average_confidence = mean(
      prediction_probability
    ),
    
    observed_accuracy = mean(correct),
    
    calibration_gap =
      observed_accuracy - average_confidence,
    
    .groups = "drop"
  )

print(ensemble_calibration, n = Inf)
ggplot(
  ensemble_calibration,
  
  aes(
    x = average_confidence,
    y = observed_accuracy,
    colour = ensemble
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    aes(size = matches)
  ) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  coord_equal() +
  labs(
    title = "Exact Scoreline Probability Calibration",
    subtitle = "Predicted confidence versus observed accuracy",
    x = "Average predicted probability",
    y = "Observed accuracy",
    colour = "Ensemble",
    size = "Matches"
  ) +
  theme_minimal()
# Save ----
write_csv(
  ensemble_predictions,
  here(
    "outputs", "validation",
    "ensemble_predictions.csv"
  )
)

write_csv(
  ensemble_performance,
  here(
    "outputs", "validation",
    "ensemble_performance.csv"
  )
)

write_csv(
  ensemble_weights,
  here(
    "outputs", "validation",
    "ensemble_weights.csv"
  )
)

write_csv(
  ensemble_calibration,
  here(
    "outputs", "validation",
    "ensemble_calibration.csv"
  )
)
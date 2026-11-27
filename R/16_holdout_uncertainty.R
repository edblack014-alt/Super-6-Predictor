# Load Holdout Predictions and Packages ----
library(tidyverse)
library(here)

holdout_predictions <- read_csv(
  here(
    "outputs", "holdout",
    "holdout_predictions.csv"
  ),
  show_col_types = FALSE
)
# Reusable Paired Block Bootstrap Function ----
compare_holdout_models <- function(
    candidate,
    benchmark,
    simulations = 5000,
    block_length = 4
) {
  
  paired <- holdout_predictions %>%
    filter(model %in% c(candidate, benchmark)) %>%
    mutate(
      comparison = if_else(
        model == candidate,
        "candidate",
        "benchmark"
      )
    ) %>%
    select(
      match_id,
      season,
      week,
      comparison,
      log_loss,
      correct
    ) %>%
    pivot_wider(
      names_from = comparison,
      values_from = c(log_loss, correct)
    ) %>%
    mutate(
      loss_gain =
        log_loss_benchmark - log_loss_candidate,
      
      accuracy_gain =
        as.integer(correct_candidate) -
        as.integer(correct_benchmark)
    )
  
  stopifnot(
    nrow(paired) == 396,
    !anyNA(paired)
  )
  
  weekly <- paired %>%
    group_by(season, week) %>%
    summarise(
      matches = n(),
      loss_gain = sum(loss_gain),
      accuracy_gain = sum(accuracy_gain),
      .groups = "drop"
    ) %>%
    arrange(season, week)
  
  seasons <- split(weekly, weekly$season)
  
  sample_blocks <- function(data) {
    
    n <- nrow(data)
    
    starts <- sample.int(
      n,
      size = ceiling(n / block_length),
      replace = TRUE
    )
    
    indices <- unlist(
      lapply(
        starts,
        function(s) {
          ((s - 1 + seq_len(block_length) - 1) %% n) + 1
        }
      )
    )
    
    data[indices[seq_len(n)], ]
  }
  
  bootstrap <- map_dfr(
    seq_len(simulations),
    
    function(i) {
      
      sampled <- map_dfr(
        seasons,
        sample_blocks
      )
      
      tibble(
        loss_gain =
          sum(sampled$loss_gain) /
          sum(sampled$matches),
        
        accuracy_gain =
          100 * sum(sampled$accuracy_gain) /
          sum(sampled$matches)
      )
    }
  )
  
  tibble(
    candidate = candidate,
    benchmark = benchmark,
    
    loss_gain = mean(paired$loss_gain),
    loss_lower = quantile(bootstrap$loss_gain, 0.025),
    loss_upper = quantile(bootstrap$loss_gain, 0.975),
    
    accuracy_gain = mean(paired$accuracy_gain) * 100,
    accuracy_lower = quantile(
      bootstrap$accuracy_gain, 0.025
    ),
    accuracy_upper = quantile(
      bootstrap$accuracy_gain, 0.975
    )
  )
}

# Visualise Uncertainty ----
holdout_uncertainty %>%
  mutate(
    comparison = paste(
      candidate,
      "vs",
      benchmark
    )
  ) %>%
  ggplot(
    aes(
      x = loss_gain,
      y = comparison
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(
      xmin = loss_lower,
      xmax = loss_upper
    ),
    orientation = "y",
    width = 0.15
  ) +
  geom_point(
    size = 4,
    colour = "#398469"
  ) +
  labs(
    title = "Do Complex Models Improve Forecasts?",
    subtitle = "95% paired block bootstrap intervals",
    x = "Improvement in log loss",
    y = NULL
  ) +
  theme_minimal()
# Market Holdout Experiments ----
market_holdout_rates <- holdout_rates %>%
  select(
    odds_home,
    odds_draw,
    odds_away,
    odds_over_25,
    odds_under_25
  ) %>%
  pmap_dfr(estimate_market_rates)

holdout_market_data <- bind_cols(
  holdout_rates,
  market_holdout_rates
) %>%
  rename(
    M8_h = market_lambda_home,
    M8_a = market_lambda_away
  )

# Evaluate Using General Probabilty Evaluation Function
market_holdout_specs <- list(
  
  M8 = c(
    M8 = 1
  ),
  
  M10C = c(
    M3W = 0.1,
    M8 = 0.9
  )
)

market_holdout_predictions <- imap_dfr(
  
  market_holdout_specs,
  
  function(weights, model_name) {
    
    evaluate_holdout_model(
      data = holdout_market_data,
      weights = weights,
      model_name = model_name
    )
  }
)

# Calculate Performance
market_holdout_performance <-
  market_holdout_predictions %>%
  group_by(model) %>%
  summarise(
    matches = n(),
    correct_scorelines = sum(correct),
    exact_accuracy = mean(correct) * 100,
    top3_accuracy = mean(top3_correct) * 100,
    top5_accuracy = mean(top5_correct) * 100,
    outcome_accuracy = mean(outcome_correct) * 100,
    log_loss = mean(log_loss),
    average_confidence =
      mean(prediction_probability) * 100,
    .groups = "drop"
  )

market_holdout_performance %>%
  mutate(
    log_loss = sprintf("%.6f", log_loss)
  ) %>%
  print(width = Inf)

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

baseline_predictions <- read_csv(
  here(
    "outputs", "validation",
    "baseline_predictions.csv"
  ),
  show_col_types = FALSE
)
# Retrieve Market Odds ----
market_data <- baseline_predictions %>%
  left_join(
    
    premier_league_master %>%
      select(
        date,
        home_team,
        away_team,
        home_goals,
        away_goals,
        odds_home,
        odds_draw,
        odds_away,
        odds_over_25,
        odds_under_25
      ),
    
    by = c(
      "date",
      "home_team",
      "away_team"
    ),
    
    relationship = "one-to-one"
  )
stopifnot(
nrow(market_data) == 594,

!anyNA(
  market_data %>%
    select(starts_with("odds_"))
),

all(
  as.matrix(
    market_data %>%
      select(starts_with("odds_"))
  ) > 1
)
)

# Margin Removal Function ----
remove_margin <- function(odds) {
  
  probabilities <- 1 / odds
  
  probabilities / sum(probabilities)
}
remove_margin(c(1.60, 4.20, 5.50))
# Estimate Total xG  ----
estimate_total_goals <- function(p_under) {
  
  objective <- function(lambda) {
    
    ppois(2, lambda) - p_under
    
  }
  
  uniroot(
    objective,
    interval = c(0, 40),
    tol = 1e-10
  )$root
}
estimate_total_goals(0.50)
# Calculate Match Outcome Probabilities ----
calculate_outcome_probs <- function(
    lambda_home,
    lambda_away
) {
  
  home_goals <- 0:qpois(
    1 - 1e-12,
    lambda_home
  )
  
  home_probability <- dpois(
    home_goals,
    lambda_home
  )
  
  p_home <- sum(
    home_probability *
      ppois(home_goals - 1, lambda_away)
  )
  
  p_draw <- sum(
    home_probability *
      dpois(home_goals, lambda_away)
  )
  
  p_away <- sum(
    home_probability *
      ppois(
        home_goals,
        lambda_away,
        lower.tail = FALSE
      )
  )
  
  probabilities <- c(
    home = p_home,
    draw = p_draw,
    away = p_away
  )
  
  probabilities / sum(probabilities)
}
calculate_outcome_probs(
  lambda_home = 2.1,
  lambda_away = 0.9
)
# Estimate Market-Implied Scoring Rates ----
estimate_market_rates <- function(
    odds_home,
    odds_draw,
    odds_away,
    odds_over_25,
    odds_under_25
) {
  
  # Remove margin from the 1X2 market
  
  market_1x2 <- remove_margin(
    c(
      odds_home,
      odds_draw,
      odds_away
    )
  )
  
  # Remove margin from the total goals market
  
  market_totals <- remove_margin(
    c(
      odds_over_25,
      odds_under_25
    )
  )
  
  p_under <- market_totals[2]
  
  # Estimate total scoring rate
  
  lambda_total <- estimate_total_goals(
    p_under
  )
  
  # Optimise home team's share
  
  objective <- function(share) {
    
    predicted_probs <- calculate_outcome_probs(
      
      lambda_home = share * lambda_total,
      
      lambda_away = (1 - share) * lambda_total
    )
    
    -sum(
      market_1x2 *
        log(pmax(predicted_probs, 1e-15))
    )
  }
  
  result <- optimize(
    objective,
    interval = c(0.0001, 0.9999),
    tol = 1e-8
  )
  
  lambda_home <- result$minimum *
    lambda_total
  
  lambda_away <- (1 - result$minimum) *
    lambda_total
  
  fitted_probs <- calculate_outcome_probs(
    lambda_home,
    lambda_away
  )
  
  tibble(
    
    market_lambda_home = lambda_home,
    
    market_lambda_away = lambda_away,
    
    market_lambda_total = lambda_total,
    
    market_home_share = result$minimum,
    
    market_p_home = market_1x2[1],
    
    market_p_draw = market_1x2[2],
    
    market_p_away = market_1x2[3],
    
    market_p_under = p_under,
    
    fitted_p_home = fitted_probs[1],
    
    fitted_p_draw = fitted_probs[2],
    
    fitted_p_away = fitted_probs[3],
    
    market_fit_error = max(
      abs(fitted_probs - market_1x2)
    )
  )
}
estimate_market_rates(
  odds_home = 1.60,
  odds_draw = 4.20,
  odds_away = 5.50,
  odds_over_25 = 1.80,
  odds_under_25 = 2.00
) %>%
  print(width = Inf)

# Generate Predictions for All Validation Fixtures ----
market_rates <- market_data %>%
  
  select(
    odds_home,
    odds_draw,
    odds_away,
    odds_over_25,
    odds_under_25
  ) %>%
  
  pmap_dfr(estimate_market_rates)
# Combine with Historical Results ----
market_predictions <- bind_cols(
  market_data,
  market_rates
) %>%
  
  mutate(
    
    prediction_m8 = paste0(
      floor(market_lambda_home),
      "-",
      floor(market_lambda_away)
    ),
    
    prediction_probability_m8 =
      
      dpois(
        floor(market_lambda_home),
        market_lambda_home
      ) *
      
      dpois(
        floor(market_lambda_away),
        market_lambda_away
      ),
    
    log_loss_m8 = -(
      
      dpois(
        home_goals,
        market_lambda_home,
        log = TRUE
      ) +
        
        dpois(
          away_goals,
          market_lambda_away,
          log = TRUE
        )
    )
  )
# Validate with Actual Goals ----
stopifnot(
  
  nrow(market_predictions) == 594,
  
  !anyNA(market_predictions$prediction_m8),
  
  all(is.finite(
    market_predictions$log_loss_m8
  )),
  
  all(market_predictions$market_lambda_home > 0),
  
  all(market_predictions$market_lambda_away > 0),
  
  all(
    abs(
      market_predictions$market_lambda_home +
        market_predictions$market_lambda_away -
        market_predictions$market_lambda_total
    ) < 1e-8
  )
)
# Evaluate Market Model Performance ----
market_performance <- market_predictions %>%
  
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(
      prediction_m8 == scoreline
    ),
    
    exact_accuracy = mean(
      prediction_m8 == scoreline
    ) * 100,
    
    log_loss = mean(log_loss_m8),
    
    expected_home_mae = mean(
      abs(home_goals - market_lambda_home)
    ),
    
    expected_away_mae = mean(
      abs(away_goals - market_lambda_away)
    ),
    
    average_market_fit_error = mean(
      market_fit_error
    ),
    
    max_market_fit_error = max(
      market_fit_error
    )
  )

print(market_performance, width = Inf)
# Performance Across Seasons ----
market_seasonal_performance <- market_predictions %>%
  
  group_by(season) %>%
  
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(
      prediction_m8 == scoreline
    ),
    
    exact_accuracy = mean(
      prediction_m8 == scoreline
    ) * 100,
    
    log_loss = mean(log_loss_m8),
    
    .groups = "drop"
  )

print(market_seasonal_performance)
# Visualise Market-Implied Scoring Rates ----
ggplot(
  market_predictions,
  aes(
    x = market_lambda_home,
    y = market_lambda_away,
    colour = fixture_type
  )
) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  coord_equal() +
  labs(
    title = "Market-Implied Scoring Rates",
    subtitle = "Premier League Big Six validation fixtures",
    x = "Expected home goals",
    y = "Expected away goals",
    colour = "Fixture type"
  ) +
  theme_minimal()
# Save Results ----
write_csv(
  market_predictions,
  here(
    "outputs", "validation",
    "market_poisson_predictions.csv"
  )
)

write_csv(
  market_performance,
  here(
    "outputs", "validation",
    "market_poisson_performance.csv"
  )
)
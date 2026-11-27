# Load Data and Packages- Freeze Methodology ----
library(tidyverse)
library(lubridate)
library(here)

master <- readRDS(
  here(
    "data", "processed",
    "premier_league_master.rds"
  )
)

holdout_start <- as.Date("2024-07-01")
holdout_end <- as.Date("2026-07-01")

matches <- master %>%
  filter(date < holdout_end) %>%
  arrange(date, home_key, away_key) %>%
  mutate(
    match_id = row_number(),
    week = floor_date(
      date,
      unit = "week",
      week_start = 1
    )
  )

frozen_half_life <- 180
frozen_window <- 10
frozen_prior <- 5
frozen_history_days <- 365

frozen_ensembles <- list(
  
  M10A = c(
    M2 = 0.2,
    M3W = 0.8
  ),
  
  M10B = c(
    M2 = 0.1,
    M3W = 0.3,
    M7A = 0.2,
    M7B = 0.4
  ),
  
  M10C = c(
    M3W = 0.1,
    M8 = 0.9
  )
)
# Identify Holdout Fixtures----
holdout_fixtures <- matches %>%
  filter(
    date >= holdout_start,
    date < holdout_end,
    big_six_fixture
  )

holdout_weeks <- holdout_fixtures %>%
  distinct(week) %>%
  arrange(week) %>%
  pull(week)

stopifnot(
  nrow(holdout_fixtures) == 396
)

holdout_fixtures %>%
  count(season)
# Reconstruct Team Level Dataset ----
team_history <- bind_rows(
  
  matches %>%
    transmute(
      match_id,
      date,
      week,
      team = home_key,
      opponent = away_key,
      home = 1L,
      goals = home_goals,
      conceded = away_goals,
      xg = home_xg_understat,
      xga = away_xg_understat,
      totals_unreliable
    ),
  
  matches %>%
    transmute(
      match_id,
      date,
      week,
      team = away_key,
      opponent = home_key,
      home = 0L,
      goals = away_goals,
      conceded = home_goals,
      xg = away_xg_understat,
      xga = home_xg_understat,
      totals_unreliable
    )
  
) %>%
  arrange(date, match_id)

# Rebuild Historical Features ----
build_holdout_features <- function(cutoff) {
  
  history <- team_history %>%
    filter(
      date < cutoff,
      date >= cutoff - frozen_history_days
    )
  
  xg_history <- history %>%
    filter(
      !is.na(xg),
      !is.na(xga),
      totals_unreliable == FALSE
    )
  
  stopifnot(
    nrow(history) > 0,
    nrow(xg_history) > 0
  )
  
  league_gf <- mean(history$goals)
  league_xg <- mean(xg_history$xg)
  
  # Rolling goals
  
  goal_form <- history %>%
    group_by(team) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_tail(n = frozen_window) %>%
    summarise(
      
      recent_gf = (
        sum(goals) +
          frozen_prior * league_gf
      ) / (n() + frozen_prior),
      
      recent_ga = (
        sum(conceded) +
          frozen_prior * league_gf
      ) / (n() + frozen_prior),
      
      .groups = "drop"
    )
  
  # Rolling expected goals
  
  xg_form <- xg_history %>%
    group_by(team) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_tail(n = frozen_window) %>%
    summarise(
      
      recent_xgf = (
        sum(xg) +
          frozen_prior * league_xg
      ) / (n() + frozen_prior),
      
      recent_xga = (
        sum(xga) +
          frozen_prior * league_xg
      ) / (n() + frozen_prior),
      
      .groups = "drop"
    )
  
  team_history %>%
    filter(week == cutoff) %>%
    
    left_join(goal_form, by = "team") %>%
    left_join(xg_form, by = "team") %>%
    
    left_join(
      goal_form %>%
        rename(
          opponent = team,
          opponent_gf = recent_gf,
          opponent_ga = recent_ga
        ),
      by = "opponent"
    ) %>%
    
    left_join(
      xg_form %>%
        rename(
          opponent = team,
          opponent_xgf = recent_xgf,
          opponent_xga = recent_xga
        ),
      by = "opponent"
    ) %>%
    
    mutate(
      across(
        c(
          recent_gf,
          recent_ga,
          opponent_gf,
          opponent_ga
        ),
        ~ coalesce(.x, league_gf)
      ),
      
      across(
        c(
          recent_xgf,
          recent_xga,
          opponent_xgf,
          opponent_xga
        ),
        ~ coalesce(.x, league_xg)
      )
    )
}
# Generate Features for all Applicable Historical Weeks ----
feature_weeks <- team_history %>%
  filter(week >= as.Date("2018-07-01")) %>%
  distinct(week) %>%
  arrange(week) %>%
  pull(week)

holdout_features <- map_dfr(
  feature_weeks,
  build_holdout_features
)

stopifnot(
  
  nrow(holdout_features) == 6080,
  
  !anyNA(holdout_features$recent_xgf),
  
  !anyNA(holdout_features$opponent_xga),
  
  all(holdout_features$recent_xgf > 0),
  
  all(holdout_features$opponent_xga > 0)
)
# Generate Walk-Forward Historical Predictions ----
fit_weighted_poisson <- function(
    formula,
    training,
    cutoff
) {
  
  training <- training %>%
    mutate(
      match_weight = 2^(
        -as.numeric(cutoff - date) /
          frozen_half_life
      )
    )
  
  model <- glm(
    formula,
    family = poisson(link = "log"),
    data = training,
    weights = match_weight
  )
  
  stopifnot(model$converged)
  
  model
}
# Handle Previously Unseen Teams ----
predict_team_effects <- function(
    model,
    training,
    testing,
    fallback
) {
  
  known_teams <- unique(training$team)
  
  known_fixture <-
    testing$team %in% known_teams &
    testing$opponent %in% known_teams
  
  rates <- fallback
  
  if (any(known_fixture)) {
    
    rates[known_fixture] <- predict(
      model,
      newdata = testing[known_fixture, ],
      type = "response"
    )
  }
  
  rates
}
# Create Weekly Forecast Engine ----
predict_holdout_week <- function(cutoff) {
  
  # Identify this week's fixtures
  
  fixtures <- holdout_fixtures %>%
    filter(week == cutoff)
  
  # M2: Fixture-based historical scoring rates
  
  baseline_rates <- matches %>%
    filter(
      date < cutoff,
      big_six_fixture
    ) %>%
    group_by(fixture_type) %>%
    summarise(
      M2_h = mean(home_goals),
      M2_a = mean(away_goals),
      .groups = "drop"
    )
  
  fixtures <- fixtures %>%
    left_join(
      baseline_rates,
      by = "fixture_type"
    )
  
  # Prepare training datasets
  
  full_training <- team_history %>%
    filter(date < cutoff)
  
  xg_training <- holdout_features %>%
    filter(date < cutoff)
  
  testing <- holdout_features %>%
    filter(match_id %in% fixtures$match_id) %>%
    left_join(
      fixtures %>%
        select(match_id, M2_h, M2_a),
      by = "match_id"
    )
  
  stopifnot(
    nrow(testing) == 2 * nrow(fixtures)
  )
  
  # M3-W: Original weighted team-strength model
  
  model_m3w <- fit_weighted_poisson(
    goals ~ home + team + opponent,
    full_training,
    cutoff
  )
  
  baseline_fallback <- if_else(
    testing$home == 1L,
    testing$M2_h,
    testing$M2_a
  )
  
  testing$M3W <- predict_team_effects(
    model_m3w,
    full_training,
    testing,
    baseline_fallback
  )
  
  # M7-A: Expected goals form model
  
  model_m7a <- fit_weighted_poisson(
    goals ~ home +
      log(recent_xgf) +
      log(opponent_xga),
    xg_training,
    cutoff
  )
  
  testing$M7A <- predict(
    model_m7a,
    newdata = testing,
    type = "response"
  )
  
  # M6-A: Original fallback for M7-B
  
  fallback_model <- fit_weighted_poisson(
    goals ~ home +
      log(recent_gf) +
      log(opponent_ga),
    xg_training,
    cutoff
  )
  
  xg_fallback <- predict(
    fallback_model,
    newdata = testing,
    type = "response"
  )
  
  # M7-B: Hybrid expected goals model
  
  model_m7b <- fit_weighted_poisson(
    goals ~ home +
      team +
      opponent +
      log(recent_xgf) +
      log(opponent_xga),
    xg_training,
    cutoff
  )
  
  testing$M7B <- predict_team_effects(
    model_m7b,
    xg_training,
    testing,
    xg_fallback
  )
  
  # Convert team-level rates to match-level
  
  predicted_rates <- testing %>%
    mutate(
      side = if_else(home == 1L, "h", "a")
    ) %>%
    select(
      match_id,
      side,
      M3W,
      M7A,
      M7B
    ) %>%
    pivot_wider(
      names_from = side,
      values_from = c(M3W, M7A, M7B),
      names_glue = "{.value}_{side}"
    )
  
  fixtures %>%
    left_join(
      predicted_rates,
      by = "match_id",
      relationship = "one-to-one"
    )
}
# Execute Predictions ----
holdout_rates <- map_dfr(
  holdout_weeks,
  predict_holdout_week
)

stopifnot(
  
  nrow(holdout_rates) == 396,
  
  !anyDuplicated(holdout_rates$match_id),
  
  all(
    as.matrix(
      holdout_rates %>%
        select(
          M2_h, M2_a,
          M3W_h, M3W_a,
          M7A_h, M7A_a,
          M7B_h, M7B_a
        )
    ) > 0
  )
)
# General Probability Evaluation Function ----
evaluate_distribution <- function(
    home_rates,
    away_rates,
    weights,
    actual_home,
    actual_away
) {
  
  stopifnot(
    length(home_rates) == length(away_rates),
    length(weights) == length(home_rates),
    all(is.finite(home_rates)),
    all(is.finite(away_rates)),
    all(home_rates > 0),
    all(away_rates > 0),
    all(weights >= 0),
    abs(sum(weights) - 1) < 1e-10
  )
  
  # Cover virtually all probability mass
  
  max_home <- max(
    5,
    qpois(1 - 1e-10, home_rates)
  )
  
  max_away <- max(
    5,
    qpois(1 - 1e-10, away_rates)
  )
  
  score_grid <- expand_grid(
    h = 0:max_home,
    a = 0:max_away
  )
  
  component_probs <- vapply(
    
    seq_along(weights),
    
    function(j) {
      
      dpois(
        score_grid$h,
        home_rates[j]
      ) *
        
        dpois(
          score_grid$a,
          away_rates[j]
        )
    },
    
    numeric(nrow(score_grid))
  )
  
  score_grid$probability <- as.numeric(
    component_probs %*% weights
  )
  
  stopifnot(
    abs(sum(score_grid$probability) - 1) <
      1e-8
  )
  
  score_grid <- score_grid %>%
    mutate(
      scoreline = paste0(h, "-", a)
    ) %>%
    arrange(desc(probability), h, a)
  
  # Preserve the original floor-based mode
  # convention for standalone models
  
  if (length(weights) == 1L) {
    
    original_mode <- paste0(
      floor(home_rates),
      "-",
      floor(away_rates)
    )
    
    score_grid <- score_grid %>%
      arrange(
        desc(scoreline == original_mode),
        desc(probability),
        h,
        a
      )
  }
  
  actual_score <- paste0(
    actual_home,
    "-",
    actual_away
  )
  
  actual_probability <- sum(
    
    weights *
      
      dpois(actual_home, home_rates) *
      
      dpois(actual_away, away_rates)
  )
  
  # Match outcome probabilities
  
  outcome_probs <- c(
    H = sum(
      score_grid$probability[
        score_grid$h > score_grid$a
      ]
    ),
    
    D = sum(
      score_grid$probability[
        score_grid$h == score_grid$a
      ]
    ),
    
    A = sum(
      score_grid$probability[
        score_grid$h < score_grid$a
      ]
    )
  )
  
  actual_outcome <- case_when(
    actual_home > actual_away ~ "H",
    actual_home == actual_away ~ "D",
    TRUE ~ "A"
  )
  
  predicted_outcome <- names(
    which.max(outcome_probs)
  )
  
  tibble(
    
    prediction = score_grid$scoreline[1],
    
    prediction_probability =
      score_grid$probability[1],
    
    top_three = paste(
      head(score_grid$scoreline, 3),
      collapse = ", "
    ),
    
    top_five = paste(
      head(score_grid$scoreline, 5),
      collapse = ", "
    ),
    
    correct =
      prediction == actual_score,
    
    top3_correct =
      actual_score %in%
      head(score_grid$scoreline, 3),
    
    top5_correct =
      actual_score %in%
      head(score_grid$scoreline, 5),
    
    actual_probability = actual_probability,
    
    log_loss = -log(actual_probability),
    
    predicted_outcome = predicted_outcome,
    
    outcome_correct =
      predicted_outcome == actual_outcome
    
  )
}
# Evaluate Frozen Historical Models ----
holdout_specs <- list(
  
  M2 = c(M2 = 1),
  
  M3W = c(M3W = 1),
  
  M7A = c(M7A = 1),
  
  M7B = c(M7B = 1),
  
  M10A = frozen_ensembles$M10A,
  
  M10B = frozen_ensembles$M10B
)

evaluate_holdout_model <- function(
    data,
    weights,
    model_name
) {
  
  components <- names(weights)
  
  home_rates <- as.matrix(
    data[paste0(components, "_h")]
  )
  
  away_rates <- as.matrix(
    data[paste0(components, "_a")]
  )
  
  predictions <- map_dfr(
    
    seq_len(nrow(data)),
    
    function(i) {
      
      evaluate_distribution(
        
        home_rates = home_rates[i, ],
        
        away_rates = away_rates[i, ],
        
        weights = weights,
        
        actual_home = data$home_goals[i],
        
        actual_away = data$away_goals[i]
        
      )
    }
  )
  
  data %>%
    select(
      match_id,
      date,
      week,
      season,
      home_team,
      away_team,
      fixture_type,
      scoreline,
      home_goals,
      away_goals
    ) %>%
    bind_cols(predictions) %>%
    mutate(model = model_name)
}

# Evaluate
holdout_predictions <- imap_dfr(
  
  holdout_specs,
  
  function(weights, model_name) {
    
    evaluate_holdout_model(
      data = holdout_rates,
      weights = weights,
      model_name = model_name
    )
    
  }
)

# Validate
stopifnot(
  
  nrow(holdout_predictions) == 396 * 6,
  
  all(
    count(holdout_predictions, model)$n == 396
  ),
  
  all(is.finite(holdout_predictions$log_loss)),
  
  all(
    holdout_predictions$prediction_probability > 0
  ),
  
  all(
    holdout_predictions$correct <=
      holdout_predictions$top3_correct
  ),
  
  all(
    holdout_predictions$top3_correct <=
      holdout_predictions$top5_correct
  )
)

# Final Holdout Performance ----
holdout_performance <- holdout_predictions %>%
  group_by(model) %>%
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(correct),
    
    exact_accuracy = mean(correct) * 100,
    
    top3_accuracy = mean(top3_correct) * 100,
    
    top5_accuracy = mean(top5_correct) * 100,
    
    outcome_accuracy = mean(outcome_correct) * 100,
    
    log_loss = mean(log_loss),
    
    average_confidence = mean(
      prediction_probability
    ) * 100,
    
    overconfidence =
      average_confidence - exact_accuracy,
    
    .groups = "drop"
    
  ) %>%
  arrange(log_loss)

holdout_performance %>%
  mutate(
    log_loss = sprintf("%.6f", log_loss)
  ) %>%
  print(width = Inf)

# Evaluate Across Seasons
holdout_seasonal_performance <-
  holdout_predictions %>%
  
  group_by(model, season) %>%
  
  summarise(
    
    matches = n(),
    
    correct_scorelines = sum(correct),
    
    exact_accuracy = mean(correct) * 100,
    
    top3_accuracy = mean(top3_correct) * 100,
    
    top5_accuracy = mean(top5_correct) * 100,
    
    log_loss = mean(log_loss),
    
    .groups = "drop"
    
  )

print(
  holdout_seasonal_performance,
  n = Inf,
  width = Inf
)

# Evaluate by Fixture Type
holdout_fixture_performance <-
  holdout_predictions %>%
  
  group_by(model, fixture_type) %>%
  
  summarise(
    
    matches = n(),
    
    exact_accuracy = mean(correct) * 100,
    
    top3_accuracy = mean(top3_correct) * 100,
    
    top5_accuracy = mean(top5_correct) * 100,
    
    log_loss = mean(log_loss),
    
    .groups = "drop"
    
  )

print(
  holdout_fixture_performance,
  n = Inf,
  width = Inf
)
# Save Results ----
dir.create(
  here("outputs", "holdout"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  holdout_rates,
  here(
    "outputs", "holdout",
    "holdout_rates.csv"
  )
)

write_csv(
  holdout_predictions,
  here(
    "outputs", "holdout",
    "holdout_predictions.csv"
  )
)

write_csv(
  holdout_performance,
  here(
    "outputs", "holdout",
    "holdout_performance.csv"
  )
)

write_csv(
  holdout_seasonal_performance,
  here(
    "outputs", "holdout",
    "holdout_seasonal_performance.csv"
  )
)

write_csv(
  holdout_fixture_performance,
  here(
    "outputs", "holdout",
    "holdout_fixture_performance.csv"
  )
)

saveRDS(
  frozen_ensembles,
  here(
    "outputs", "holdout",
    "frozen_ensemble_weights.rds"
  )
)
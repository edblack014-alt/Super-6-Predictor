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

# Define Validation and Development Periods ----
development_data <- premier_league_master %>%
  filter(date < as.Date("2024-07-01"))

validation_start <- as.Date("2021-07-01")

validation_end <- as.Date("2024-07-01")

validation_weeks <- development_data %>%
  filter(
    date >= validation_start,
    date < validation_end,
    big_six_fixture
  ) %>%
  mutate(
    week = floor_date(
      date,
      unit = "week",
      week_start = 1
    )
  ) %>%
  distinct(week) %>%
  arrange(week) %>%
  pull(week)


# Create Weekly Prediction Function ----
predict_baselines <- function(cutoff) {
  
  # Only use information available before the week
  training <- development_data %>%
    filter(
      date < cutoff,
      big_six_fixture
    )
  
  testing <- development_data %>%
    filter(
      date >= cutoff,
      date < cutoff + 7,
      big_six_fixture
    )
  
  # M0: Unconditional scoreline frequency
  
  most_common <- training %>%
    count(scoreline, sort = TRUE) %>%
    arrange(desc(n), scoreline) %>%
    slice_head(n = 1) %>%
    pull(scoreline)
  
  # M1: Conditional scoreline frequency
  
  conditional_scores <- training %>%
    count(fixture_type, scoreline) %>%
    group_by(fixture_type) %>%
    arrange(desc(n), scoreline) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      fixture_type,
      prediction_m1 = scoreline
    )
  
  # M2: Independent Poisson by fixture type
  
  poisson_rates <- training %>%
    group_by(fixture_type) %>%
    summarise(
      lambda_home = mean(home_goals),
      lambda_away = mean(away_goals),
      .groups = "drop"
    ) %>%
    mutate(
      prediction_m2 = paste0(
        floor(lambda_home),
        "-",
        floor(lambda_away)
      )
    )
  
  # Combine predictions
  
  testing %>%
    select(
      date,
      season,
      home_team,
      away_team,
      fixture_type,
      scoreline
    ) %>%
    mutate(
      prediction_date = cutoff,
      prediction_m0 = most_common
    ) %>%
    left_join(
      conditional_scores,
      by = "fixture_type"
    ) %>%
    left_join(
      poisson_rates,
      by = "fixture_type"
    )
}
# Walk Forward Validation ----
baseline_predictions <- map_dfr(
  validation_weeks,
  predict_baselines
)

View(baseline_predictions)
# Evaluate Exact Scoreline Accuracy ----
baseline_performance <- baseline_predictions %>%
  pivot_longer(
    cols = c(
      prediction_m0,
      prediction_m1,
      prediction_m2
    ),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  mutate(
    correct = prediction == scoreline
  ) %>%
  group_by(model) %>%
  summarise(
    matches = n(),
    correct_scorelines = sum(correct),
    accuracy = mean(correct) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(accuracy))

print(baseline_performance)
  
  testing <- development_data %>%
    filter(
      date >= cutoff,
      date < cutoff + 7,
      big_six_fixture
    )
  
  # M0: Unconditional scoreline frequency
  
  most_common <- training %>%
    count(scoreline, sort = TRUE) %>%
    arrange(desc(n), scoreline) %>%
    slice_head(n = 1) %>%
    pull(scoreline)
  
  # M1: Conditional scoreline frequency
  
  conditional_scores <- training %>%
    count(fixture_type, scoreline) %>%
    group_by(fixture_type) %>%
    arrange(desc(n), scoreline) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      fixture_type,
      prediction_m1 = scoreline
    )
  
  # M2: Independent Poisson by fixture type
  
  poisson_rates <- training %>%
    group_by(fixture_type) %>%
    summarise(
      lambda_home = mean(home_goals),
      lambda_away = mean(away_goals),
      .groups = "drop"
    ) %>%
    mutate(
      prediction_m2 = paste0(
        floor(lambda_home),
        "-",
        floor(lambda_away)
      )
    )
  
  # Combine predictions
  
  testing %>%
    select(
      date,
      season,
      home_team,
      away_team,
      fixture_type,
      scoreline
    ) %>%
    mutate(
      prediction_date = cutoff,
      prediction_m0 = most_common
    ) %>%
    left_join(
      conditional_scores,
      by = "fixture_type"
    ) %>%
    left_join(
      poisson_rates,
      by = "fixture_type"
    )
# Evaluate Performance by Fixture Type ----
  baseline_fixture_performance <- baseline_predictions %>%
    pivot_longer(
      cols = c(
        prediction_m0,
        prediction_m1,
        prediction_m2
      ),
      names_to = "model",
      values_to = "prediction"
    ) %>%
    mutate(
      correct = prediction == scoreline
    ) %>%
    group_by(model, fixture_type) %>%
    summarise(
      matches = n(),
      correct_scorelines = sum(correct),
      accuracy = mean(correct) * 100,
      .groups = "drop"
    )
  
  print(baseline_fixture_performance, n = Inf)
# Statistical Comparison of Baseline Models ----
  
  model_comparison <- baseline_predictions %>%
    mutate(
      m1_correct = prediction_m1 == scoreline,
      m2_correct = prediction_m2 == scoreline
    )
  
  comparison_table <- table(
    M1 = factor(
      model_comparison$m1_correct,
      levels = c(FALSE, TRUE)
    ),
    M2 = factor(
      model_comparison$m2_correct,
      levels = c(FALSE, TRUE)
    )
  )
  
  print(comparison_table)
  
  mcnemar.test(comparison_table)
# Performance by Fixture Type ----
  
  baseline_fixture_performance <- baseline_predictions %>%
    pivot_longer(
      cols = c(
        prediction_m0,
        prediction_m1,
        prediction_m2
      ),
      names_to = "model",
      values_to = "prediction"
    ) %>%
    mutate(
      correct = prediction == scoreline
    ) %>%
    group_by(model, fixture_type) %>%
    summarise(
      matches = n(),
      correct_scorelines = sum(correct),
      accuracy = mean(correct) * 100,
      .groups = "drop"
    )
  
  print(baseline_fixture_performance, n = Inf)
  ggplot(
    baseline_fixture_performance,
    aes(
      x = fixture_type,
      y = accuracy,
      fill = model
    )
  ) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = paste0(round(accuracy, 1), "%")),
      position = position_dodge(width = 0.9),
      vjust = -0.4,
      size = 3
    ) +
    labs(
      title = "Baseline Model Performance by Fixture Type",
      subtitle = "Walk-forward validation: 2021/22–2023/24",
      x = NULL,
      y = "Exact scoreline accuracy (%)",
      fill = "Model"
    ) +
    theme_minimal()
# Extended Model Evaluation ----
  
  baseline_extended_performance <- baseline_predictions %>%
    pivot_longer(
      cols = c(
        prediction_m0,
        prediction_m1,
        prediction_m2
      ),
      names_to = "model",
      values_to = "prediction"
    ) %>%
    separate(
      scoreline,
      into = c("actual_home", "actual_away"),
      sep = "-",
      convert = TRUE
    ) %>%
    separate(
      prediction,
      into = c("predicted_home", "predicted_away"),
      sep = "-",
      convert = TRUE
    ) %>%
    mutate(
      exact_correct =
        actual_home == predicted_home &
        actual_away == predicted_away,
      
      outcome_correct =
        sign(actual_home - actual_away) ==
        sign(predicted_home - predicted_away),
      
      home_error = abs(actual_home - predicted_home),
      
      away_error = abs(actual_away - predicted_away),
      
      total_error = home_error + away_error
    ) %>%
    group_by(model) %>%
    summarise(
      matches = n(),
      
      exact_accuracy = mean(exact_correct) * 100,
      
      outcome_accuracy = mean(outcome_correct) * 100,
      
      home_goal_mae = mean(home_error),
      
      away_goal_mae = mean(away_error),
      
      combined_goal_mae = mean(total_error),
      
      .groups = "drop"
    )
  
  print(baseline_extended_performance, width = Inf)
# Save Baseline Results----
  dir.create(
    here("outputs", "validation"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    baseline_predictions,
    here("outputs", "validation", "baseline_predictions.csv")
  )
  
  write_csv(
    baseline_performance,
    here("outputs", "validation", "baseline_performance.csv")
  )
  
  write_csv(
    baseline_extended_performance,
    here("outputs", "validation", "baseline_extended_performance.csv")
  )
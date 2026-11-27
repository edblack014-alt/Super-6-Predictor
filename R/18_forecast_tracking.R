# Load Archived Forecasts ----
library(tidyverse)
library(lubridate)
library(here)

forecast_directories <- list.dirs(
  here("outputs", "forecasts"),
  recursive = FALSE,
  full.names = TRUE
)

stopifnot(length(forecast_directories) > 0)

archived_forecasts <- map_dfr(
  
  forecast_directories,
  
  function(directory) {
    
    read_csv(
      file.path(directory, "forecast_summary.csv"),
      show_col_types = FALSE
    ) %>%
      mutate(run_id = basename(directory))
  }
)
# Prevent Retrospective Forecast Selection ----
eligible_forecasts <- archived_forecasts %>%
  
  filter(
    generated_at < as.POSIXct(
      paste(date, "00:00:00"),
      tz = "Europe/London"
    )
  ) %>%
  
  arrange(generated_at) %>%
  
  group_by(
    date,
    home_team,
    away_team,
    model
  ) %>%
  
  slice_head(n = 1) %>%
  
  ungroup()
# Retrieve Actual Results ----
master <- readRDS(
  here(
    "data", "processed",
    "premier_league_master.rds"
  )
)

actual_results <- master %>%
  
  filter(
    !is.na(home_goals),
    !is.na(away_goals)
  ) %>%
  
  transmute(
    date,
    home_team,
    away_team,
    actual_home_goals = home_goals,
    actual_away_goals = away_goals,
    actual_scoreline = scoreline
  )
# Compare Forecasts Against Results ----
forecast_tracking <- eligible_forecasts %>%
  
  left_join(
    actual_results,
    
    by = c(
      "date",
      "home_team",
      "away_team"
    ),
    
    relationship = "many-to-one"
  ) %>%
  
  mutate(
    
    status = case_when(
      
      is.na(actual_scoreline) ~ "Awaiting result",
      
      predicted_scoreline == actual_scoreline ~
        "Correct",
      
      TRUE ~ "Incorrect"
    ),
    
    correct = case_when(
      is.na(actual_scoreline) ~ NA,
      TRUE ~ predicted_scoreline == actual_scoreline
    )
  )

print(
  forecast_tracking %>%
    select(
      date,
      home_team,
      away_team,
      predicted_scoreline,
      actual_scoreline,
      confidence,
      status
    ),
  
  n = Inf
)
# Calculate Cumulative Prospective Performance ----
live_performance <- forecast_tracking %>%
  
  group_by(model) %>%
  
  summarise(
    
    total_forecasts = n(),
    
    completed_matches = sum(
      !is.na(correct)
    ),
    
    pending_matches = sum(
      is.na(correct)
    ),
    
    correct_scorelines = sum(
      correct,
      na.rm = TRUE
    ),
    
    exact_accuracy = if_else(
      completed_matches > 0,
      
      100 * correct_scorelines /
        completed_matches,
      
      NA_real_
    ),
    
    expected_correct_scorelines = sum(
      confidence[!is.na(correct)]
    ),
    
    .groups = "drop"
  )

print(live_performance, width = Inf)
# Save Tracking Outputs ----
dir.create(
  here("outputs", "tracking"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  forecast_tracking,
  here(
    "outputs", "tracking",
    "forecast_tracking.csv"
  )
)

write_csv(
  live_performance,
  here(
    "outputs", "tracking",
    "live_performance.csv"
  )
)
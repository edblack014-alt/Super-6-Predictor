# Load Historical Data and Packages ----
library(tidyverse)
library(lubridate)
library(here)

master <- readRDS(
  here(
    "data", "processed",
    "premier_league_master.rds"
  )
)

forecast_cutoff <- floor_date(
  Sys.Date(),
  unit = "week",
  week_start = 1
)

historical_results <- master %>%
  filter(
    date < forecast_cutoff,
    !is.na(home_goals),
    !is.na(away_goals)
  )

print(forecast_cutoff)
print(max(historical_results$date))
# Load and Validate Fixtures ----
fixtures <- read_csv(
  here("data", "raw", "fixtures_to_predict.csv"),
  
  col_types = cols(
    date = col_date(),
    home_team = col_character(),
    away_team = col_character()
  )
)

team_lookup <- bind_rows(
  
  master %>%
    transmute(
      team_name = home_team,
      team_key = home_key
    ),
  
  master %>%
    transmute(
      team_name = away_team,
      team_key = away_key
    )
  
) %>%
  distinct()

fixtures <- fixtures %>%
  
  left_join(
    team_lookup %>%
      rename(
        home_team = team_name,
        home_key = team_key
      ),
    by = "home_team",
    relationship = "many-to-one"
  ) %>%
  
  left_join(
    team_lookup %>%
      rename(
        away_team = team_name,
        away_key = team_key
      ),
    by = "away_team",
    relationship = "many-to-one"
  )

# Classify Fixtures 
big_six_keys <- c(
  "arsenal",
  "chelsea",
  "liverpool",
  "mancity",
  "manunited",
  "tottenham"
)

fixtures <- fixtures %>%
  
  mutate(
    
    home_big_six =
      home_key %in% big_six_keys,
    
    away_big_six =
      away_key %in% big_six_keys,
    
    fixture_type = case_when(
      
      home_big_six & away_big_six ~
        "Big Six vs Big Six",
      
      home_big_six ~
        "Big Six at Home",
      
      away_big_six ~
        "Big Six Away",
      
      TRUE ~ "Other"
    ),
    
    match_id = row_number()
  )

# Validate
stopifnot(
  nrow(fixtures) > 0,
  
  !anyNA(fixtures$home_key),
  
  !anyNA(fixtures$away_key),
  
  !anyNA(fixtures$date),
  
  all(fixtures$home_key != fixtures$away_key),
  
  all(fixtures$fixture_type != "Other"),
  
  all(fixtures$date >= forecast_cutoff),
  
  all(fixtures$date < forecast_cutoff + 7),
  
  !anyDuplicated(
    fixtures[c("date", "home_key", "away_key")]
  )
)
# Generate Component Forecasts ----
# M2- Fixture-based Poisson ----
baseline_rates <- historical_results %>%
  
  filter(big_six_fixture) %>%
  
  group_by(fixture_type) %>%
  
  summarise(
    
    M2_h = mean(home_goals),
    
    M2_a = mean(away_goals),
    
    .groups = "drop"
  )

forecast_rates <- fixtures %>%
  
  left_join(
    baseline_rates,
    by = "fixture_type",
    relationship = "many-to-one"
  )

# M3-W- Weighted Poisson Regression
production_training <- bind_rows(
  
  historical_results %>%
    transmute(
      date,
      team = home_key,
      opponent = away_key,
      home = 1L,
      goals = home_goals
    ),
  
  historical_results %>%
    transmute(
      date,
      team = away_key,
      opponent = home_key,
      home = 0L,
      goals = away_goals
    )
  
) %>%
  
  mutate(
    
    days_ago = as.numeric(
      forecast_cutoff - date
    ),
    
    match_weight = 2^(
      -days_ago / 180
    )
  )

# Fit Frozen Model
production_model <- glm(
  
  goals ~ home + team + opponent,
  
  family = poisson(link = "log"),
  
  data = production_training,
  
  weights = match_weight
)

stopifnot(production_model$converged)

# Generate Expected Scoring Rates ----
known_teams <- unique(production_training$team)

known_fixture <-
  forecast_rates$home_key %in% known_teams &
  forecast_rates$away_key %in% known_teams

# Default to M2 for previously unseen teams

forecast_rates$M3W_h <- forecast_rates$M2_h
forecast_rates$M3W_a <- forecast_rates$M2_a

if (any(known_fixture)) {
  
  home_newdata <- forecast_rates %>%
    filter(known_fixture) %>%
    transmute(
      team = home_key,
      opponent = away_key,
      home = 1L
    )
  
  away_newdata <- forecast_rates %>%
    filter(known_fixture) %>%
    transmute(
      team = away_key,
      opponent = home_key,
      home = 0L
    )
  
  forecast_rates$M3W_h[known_fixture] <-
    predict(
      production_model,
      newdata = home_newdata,
      type = "response"
    )
  
  forecast_rates$M3W_a[known_fixture] <-
    predict(
      production_model,
      newdata = away_newdata,
      type = "response"
    )
}

forecast_rates$fallback_used <- !known_fixture

rates <- as.matrix(
  forecast_rates[c(
    "M2_h", "M2_a",
    "M3W_h", "M3W_a"
  )]
)

stopifnot(
  all(is.finite(rates)),
  all(rates > 0)
)

print(forecast_rates, width = Inf)

# Generate Complete Probability Distributions ----
generate_score_distribution <- function(
    m2_home,
    m2_away,
    m3_home,
    m3_away
) {
  
  max_home <- max(
    6,
    qpois(
      1 - 1e-10,
      c(m2_home, m3_home)
    )
  )
  
  max_away <- max(
    6,
    qpois(
      1 - 1e-10,
      c(m2_away, m3_away)
    )
  )
  
  distribution <- expand_grid(
    home_goals = 0:max_home,
    away_goals = 0:max_away
  ) %>%
    
    mutate(
      
      probability_m2 =
        
        dpois(home_goals, m2_home) *
        dpois(away_goals, m2_away),
      
      probability_m3w =
        
        dpois(home_goals, m3_home) *
        dpois(away_goals, m3_away),
      
      probability =
        
        0.2 * probability_m2 +
        0.8 * probability_m3w,
      
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
    
    mutate(rank = row_number())
  
  stopifnot(
    abs(sum(distribution$probability) - 1) < 1e-8
  )
  
  distribution
}

# Apply Function to every Fixture ----
scoreline_distributions <- map_dfr(
  
  seq_len(nrow(forecast_rates)),
  
  function(i) {
    
    fixture <- forecast_rates[i, ]
    
    generate_score_distribution(
      
      m2_home = fixture$M2_h,
      
      m2_away = fixture$M2_a,
      
      m3_home = fixture$M3W_h,
      
      m3_away = fixture$M3W_a
      
    ) %>%
      
      mutate(
        match_id = fixture$match_id,
        date = fixture$date,
        home_team = fixture$home_team,
        away_team = fixture$away_team,
        .before = 1
      )
  }
)
# Final Forecast Summary ----
forecast_summary <- scoreline_distributions %>%
  
  group_by(
    match_id,
    date,
    home_team,
    away_team
  ) %>%
  
  summarise(
    
    predicted_scoreline = first(scoreline),
    
    confidence = first(probability),
    
    expected_home_goals = sum(
      home_goals * probability
    ),
    
    expected_away_goals = sum(
      away_goals * probability
    ),
    
    p_home = sum(
      probability[home_goals > away_goals]
    ),
    
    p_draw = sum(
      probability[home_goals == away_goals]
    ),
    
    p_away = sum(
      probability[home_goals < away_goals]
    ),
    
    top3_probability = sum(
      head(probability, 3)
    ),
    
    top6_probability = sum(
      head(probability, 6)
    ),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    model = "M10A",
    prediction_cutoff = forecast_cutoff,
    generated_at = Sys.time()
  )

print(
  forecast_summary,
  n = Inf,
  width = Inf
)

# Six Most Probable Scorelines per Fixture
top_six_scorelines <- scoreline_distributions %>%
  
  filter(rank <= 6) %>%
  
  select(
    date,
    home_team,
    away_team,
    rank,
    scoreline,
    probability
  )

print(top_six_scorelines, n = Inf)
# Archive Forecasts ----
run_id <- format(
  Sys.time(),
  "%Y%m%d_%H%M%OS6",
  tz = "UTC"
)

output_directory <- here(
  "outputs",
  "forecasts",
  paste0(
    forecast_cutoff,
    "_",
    run_id
  )
)

stopifnot(!dir.exists(output_directory))

dir.create(
  output_directory,
  recursive = TRUE
)
# Save Outputs ----
write_csv(
  forecast_summary,
  file.path(
    output_directory,
    "forecast_summary.csv"
  )
)

write_csv(
  top_six_scorelines,
  file.path(
    output_directory,
    "top_six_scorelines.csv"
  )
)

write_csv(
  scoreline_distributions,
  file.path(
    output_directory,
    "scoreline_distributions.csv"
  )
)

write_csv(
  forecast_rates,
  file.path(
    output_directory,
    "component_rates.csv"
  )
)

saveRDS(
  production_model,
  file.path(
    output_directory,
    "production_model.rds"
  )
)
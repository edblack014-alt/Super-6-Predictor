# Expected Goals Data Acquisition ----

library(tidyverse)
library(lubridate)
library(here)

# Create Directory ----

dir.create(
  here("data", "raw", "xg"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Define Available Seasons ----

xg_years <- 2017:2025

# Data Retrieval Function ----

get_historical_xg <- function(start_year) {
  
  file_path <- here(
    "data", "raw", "xg",
    paste0("Understat_EPL_", start_year, ".csv")
  )
  
  if (file.exists(file_path)) {
    
    message("Loading cached xG: ", start_year)
    
    data <- read_csv(
      file_path,
      col_types = cols(.default = col_character())
    )
    
  } else {
    
    message("Downloading historical xG: ", start_year)
    
    url <- paste0(
      "https://www.xgtables.io/history/epl/",
      start_year,
      "/fixtures.csv"
    )
    
    data <- read_csv(
      url,
      col_types = cols(.default = col_character())
    )
  }
  
  # Validate Data ----
  
  required_columns <- c(
    "season", "date", "home", "away", "score",
    "understat_id", "provider",
    "home_published_xg",
    "away_published_xg",
    "totals_unreliable"
  )
  
  if (!all(required_columns %in% names(data))) {
    stop("Missing essential columns: ", start_year)
  }
  
  if (nrow(data) != 380) {
    stop("Unexpected match count: ", start_year)
  }
  
  if (!all(data$season %in% as.character(start_year))) {
    stop("Incorrect season: ", start_year)
  }
  
  if (!file.exists(file_path)) {
    write_csv(data, file_path)
  }
  
  data
}

# Retrieve and Combine Historical Data ----

xg_raw <- map_dfr(
  xg_years,
  get_historical_xg
)

# Clean Expected Goals Data ----

xg_matches <- xg_raw %>%
  transmute(
    
    season = sprintf(
      "%02d%02d",
      as.integer(season) %% 100,
      (as.integer(season) + 1) %% 100
    ),
    
    date = ymd(date),
    
    home_team = home,
    away_team = away,
    
    xg_scoreline = score,
    
    understat_id = understat_id,
    
    home_xg_understat = as.numeric(home_published_xg),
    away_xg_understat = as.numeric(away_published_xg),
    
    xg_provider = provider,
    
    totals_unreliable = as.logical(totals_unreliable)
    
  ) %>%
  arrange(date)
# Check Seasonal Coverage ----

xg_matches %>%
  group_by(season) %>%
  summarise(
    matches = n(),
    
    home_xg_coverage =
      mean(!is.na(home_xg_understat)) * 100,
    
    away_xg_coverage =
      mean(!is.na(away_xg_understat)) * 100,
    
    unreliable_matches =
      sum(totals_unreliable, na.rm = TRUE),
    
    .groups = "drop"
  )
# Check Duplicates ----
xg_matches %>%
  count(season, home_team, away_team) %>%
  filter(n > 1)
# Save Datasets ----
write_csv(
  xg_matches,
  here("data", "processed", "xg_matches.csv")
)

saveRDS(
  xg_matches,
  here("data", "processed", "xg_matches.rds")
)

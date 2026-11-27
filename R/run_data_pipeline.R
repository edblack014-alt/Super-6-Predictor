# Master Data Acquisition Pipeline ----

library(here)

start_time <- Sys.time()

message("Starting Premier League data pipeline")

# Stage 1: Historical match data
source(here("R", "01_data_acquisition.R"))

# Stage 2: Historical expected goals
source(here("R", "02_xg_acquisition.R"))

# Stage 3: Data integration
source(here("R", "03_data_integration.R"))

end_time <- Sys.time()

message("Data pipeline successfully completed")

print(end_time - start_time)
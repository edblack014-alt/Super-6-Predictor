# Run from the existing R project root: source("dashboard/smoke_test.R")
local({
  target <- if (file.exists("dashboard/app.R")) "dashboard" else "."
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(target)
  parse(file = "app.R")
  env <- new.env(parent = globalenv())
  sys.source("app.R", envir = env)
  f <- env$reference_forecast()
  t <- env$read_top("reference", f)
  stopifnot(nrow(f) == 5, nrow(t) == 30, all(table(t$match_id) == 6),
    all(abs(rowSums(f[c("p_home", "p_draw", "p_away")]) - 1) < .002))
  a <- env$load_archive()
  stopifnot(is.data.frame(a))
  synthetic <- f[1, ]
  synthetic$run_id <- "synthetic_smoke_test"
  actual <- tibble::tibble(date = synthetic$date, home_team = synthetic$home_team,
    away_team = synthetic$away_team, home_goals = 1L, away_goals = 1L)
  scored <- env$track_archive(synthetic, actual)
  stopifnot(nrow(scored) == 1, isTRUE(scored$correct[1]), is.na(scored$log_loss[1]))
  synthetic$generated_at <- as.POSIXct(paste(synthetic$date, "12:00:00"), tz = "Europe/London")
  stopifnot(nrow(env$track_archive(synthetic, actual)) == 0)
  message("Smoke test passed: app parsed, references and prospective eligibility checked.")
})

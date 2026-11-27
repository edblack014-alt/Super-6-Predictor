# Super 6 Predictor | read-only Shiny interface to the existing R pipeline
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
root_override <- Sys.getenv("FOOTBALL_PROJECT_ROOT", unset = "")
project_root <- if (nzchar(root_override)) normalizePath(root_override, winslash = "/", mustWork = FALSE) else dirname(app_dir)
ref <- function(name) file.path(app_dir, "reference", name)
project <- function(...) file.path(project_root, ...)
safe_csv <- function(path) {
  if (!file.exists(path)) return(tibble())
  tryCatch(read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) { warning(conditionMessage(e)); tibble() })
}
safe_rds <- function(path) {
  if (!file.exists(path)) return(tibble())
  tryCatch(readRDS(path), error = function(e) { warning(conditionMessage(e)); tibble() })
}
has <- function(x, cols) all(cols %in% names(x)) && nrow(x) > 0
pct <- function(x, digits = 1) ifelse(is.na(x), "—", paste0(formatC(100*x, digits = digits, format = "f"), "%"))
fmt <- function(x, digits = 2) ifelse(is.na(x), "—", formatC(x, digits = digits, format = "f"))
labels <- c(M0="Historical frequency", M1="Conditional frequency", M2="Fixture Poisson", M3="Team Poisson", M3W="Weighted Poisson", M5A="Weighted Dixon-Coles", M6A="Goals form", M6B="Hybrid goals", M7A="xG form", M7B="Hybrid xG", M8="Market Poisson", M9A="Elo Poisson", M9B="Hybrid Elo", M10A="Core ensemble", M10B="xG ensemble", M10C="Market ensemble")
model_label <- function(x) ifelse(x %in% names(labels), paste0(x, " · ", unname(labels[x])), x)
market_models <- c("M8", "M10C")
match_keys <- c("date", "home_team", "away_team")
summary_cols <- c("match_id", match_keys, "predicted_scoreline", "confidence", "expected_home_goals", "expected_away_goals", "p_home", "p_draw", "p_away", "top3_probability", "top6_probability", "model", "prediction_cutoff", "generated_at")
dist_cols <- c("match_id", "home_goals", "away_goals", "scoreline", "probability")
parse_stamp <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x))
  # readr writes POSIXct archives in ISO-8601 UTC; snapshot is GMT.
  suppressWarnings(readr::parse_datetime(as.character(x), locale = locale(tz = "UTC")))
}
load_run <- function(directory) {
  x <- safe_csv(file.path(directory, "forecast_summary.csv"))
  if (!has(x, summary_cols)) return(NULL)
  x <- x %>% mutate(date = as.Date(date), generated_at = parse_stamp(generated_at), prediction_cutoff = as.Date(prediction_cutoff), match_id = as.character(match_id), run_id = basename(directory))
  if (anyNA(x[, c("date", "generated_at", "match_id", "prediction_cutoff")]) || anyDuplicated(x$match_id)) return(NULL)
  if (any(!is.finite(x$confidence) | x$confidence <= 0 | x$confidence > 1)) return(NULL)
  x
}
load_archive <- function() {
  folder <- project("outputs", "forecasts")
  dirs <- if (dir.exists(folder)) list.dirs(folder, recursive = FALSE, full.names = TRUE) else character()
  bind_rows(lapply(dirs, load_run))
}
read_distribution <- function(run_id) {
  if (identical(run_id, "reference")) return(tibble())
  x <- safe_csv(project("outputs", "forecasts", run_id, "scoreline_distributions.csv"))
  if (!has(x, dist_cols)) return(tibble())
  x %>% mutate(match_id = as.character(match_id))
}
read_top <- function(run_id, summary) {
  if (identical(run_id, "reference")) return(safe_csv(ref("top_six_scorelines.csv")) %>% mutate(match_id = as.character(match_id)))
  x <- read_distribution(run_id)
  if (nrow(x)) return(x %>% group_by(match_id) %>% arrange(desc(probability), home_goals, away_goals, .by_group = TRUE) %>% mutate(rank = row_number()) %>% filter(rank <= 6) %>% ungroup())
  x <- safe_csv(project("outputs", "forecasts", run_id, "top_six_scorelines.csv"))
  if (!has(x, c(match_keys, "rank", "scoreline", "probability"))) return(tibble())
  x %>% mutate(date = as.Date(date)) %>% inner_join(summary %>% select(all_of(match_keys), match_id), by = match_keys)
}
reference_forecast <- function() safe_csv(ref("forecast_summary.csv")) %>% mutate(date = as.Date(date), prediction_cutoff = as.Date(prediction_cutoff), generated_at = parse_stamp(generated_at), match_id = as.character(match_id), run_id = "reference")
# The supplied frozen research snapshot is supplemented by local files, if available.
read_holdout <- function() {
  fallback <- safe_csv(ref("holdout.csv"))
  x <- bind_rows(safe_csv(project("outputs", "holdout", "holdout_performance.csv")), safe_csv(project("outputs", "holdout", "market_holdout_performance.csv")))
  if (!has(x, c("model", "matches", "exact_accuracy", "log_loss"))) return(fallback %>% mutate(source = "Reported snapshot"))
  bind_rows(x %>% distinct(model, .keep_all = TRUE) %>% mutate(source = "Local output"), fallback %>% filter(!model %in% x$model) %>% mutate(source = "Reported snapshot"))
}
read_detail <- function(local, fallback, required) {
  x <- safe_csv(project("outputs", "holdout", local))
  if (has(x, required)) x else safe_csv(ref(fallback))
}
# Only archive runs, never the supplied reference forecasts, enter live tracking.
track_archive <- function(archive, master) {
  if (!has(archive, summary_cols)) return(tibble())
  eligible <- archive %>% filter(generated_at < as.POSIXct(paste(date, "00:00:00"), tz = "Europe/London")) %>% arrange(generated_at, run_id) %>% group_by(across(all_of(c(match_keys, "model")))) %>% slice_head(n = 1) %>% ungroup()
  if (!nrow(eligible)) return(tibble())
  actual <- if (has(master, c(match_keys, "home_goals", "away_goals"))) master %>% filter(!is.na(home_goals), !is.na(away_goals)) %>% transmute(date = as.Date(date), home_team, away_team, actual_home = home_goals, actual_away = away_goals) else tibble(date = as.Date(character()), home_team = character(), away_team = character(), actual_home = numeric(), actual_away = numeric())
  if (anyDuplicated(actual[match_keys])) stop("Duplicate historical result keys: inspect master dataset.")
  out <- eligible %>% left_join(actual, by = match_keys) %>% mutate(actual_scoreline = ifelse(is.na(actual_home), NA_character_, paste0(actual_home, "-", actual_away)), correct = ifelse(is.na(actual_scoreline), NA, predicted_scoreline == actual_scoreline), status = case_when(is.na(actual_scoreline) ~ "Awaiting result", correct ~ "Correct", TRUE ~ "Incorrect"), top3_correct = NA, top6_correct = NA, actual_probability = NA_real_)
  for (run in unique(out$run_id)) {
    idx <- which(out$run_id == run & !is.na(out$actual_scoreline))
    if (!length(idx)) next
    dist <- read_distribution(run)
    top <- read_top(run, archive %>% filter(run_id == run))
    rates <- safe_csv(project("outputs", "forecasts", run, "component_rates.csv"))
    if ("match_id" %in% names(rates)) rates$match_id <- as.character(rates$match_id)
    for (i in idx) {
      id <- out$match_id[i]; score <- out$actual_scoreline[i]
      if (has(top, c("match_id", "rank", "scoreline"))) {
        t <- top[top$match_id == id, , drop = FALSE]
        if (sum(t$rank <= 3) == 3) out$top3_correct[i] <- score %in% t$scoreline[t$rank <= 3]
        if (sum(t$rank <= 6) == 6) out$top6_correct[i] <- score %in% t$scoreline[t$rank <= 6]
      }
      if (nrow(dist)) {
        p <- dist$probability[dist$match_id == id & dist$scoreline == score]
        if (length(p) == 1 && is.finite(p) && p > 0) out$actual_probability[i] <- p
      }
      # Exact tail probability if the realised score is outside the archived grid.
      if (is.na(out$actual_probability[i]) && out$model[i] == "M10A" && has(rates, c("match_id", "M2_h", "M2_a", "M3W_h", "M3W_a"))) {
        r <- rates[rates$match_id == id, , drop = FALSE]
        if (nrow(r) == 1) out$actual_probability[i] <- .2*dpois(out$actual_home[i], r$M2_h)*dpois(out$actual_away[i], r$M2_a) + .8*dpois(out$actual_home[i], r$M3W_h)*dpois(out$actual_away[i], r$M3W_a)
      }
    }
  }
  out %>% mutate(log_loss = ifelse(actual_probability > 0, -log(actual_probability), NA_real_)) %>% arrange(date, home_team, model)
}

# Visual design -------------------------------------------------------------
ink <- "#142E36"; teal <- "#087F73"; blue <- "#547C9A"; amber <- "#B1772B"
css <- "
body{background:#f2f5f4;color:#18323a;font-family:Arial,sans-serif}
.navbar{background:#142e36!important;border:0;box-shadow:0 2px 12px #0001}.navbar-brand{font-weight:800;letter-spacing:-.5px;color:white!important}.navbar .nav-link,.navbar-nav>li>a{color:#d2e0df!important}.navbar .active .nav-link,.navbar .nav-link.active{color:white!important;border-bottom:2px solid #48c8ae}
.container-fluid{max-width:1550px}.hero{padding:24px 26px;background:#142e36;color:#fff;border-radius:15px;margin:14px 0 20px;position:relative}.hero h1{font-size:29px;font-weight:800;margin:5px 0 8px;letter-spacing:-.8px}.hero p{color:#d1e1df;max-width:900px;margin:0}.eyebrow{font-size:11px;letter-spacing:2px;text-transform:uppercase;color:#74d4be;font-weight:700}.pagehead{margin:22px 0 14px}.pagehead h2{font-size:25px;font-weight:800;letter-spacing:-.5px;margin:0 0 5px}.pagehead p{color:#5c7276;margin:0}.panel-card{background:white;border:1px solid #e0e8e5;border-radius:13px;padding:18px;margin-bottom:15px;box-shadow:0 3px 8px #18323a06}.panel-card h3{font-size:16px;font-weight:750;margin:0 0 12px}.kpi{background:white;border:1px solid #dfe8e5;border-radius:12px;padding:16px;min-height:111px;margin-bottom:15px}.kpi-label{color:#62777a;font-size:11px;letter-spacing:.8px;text-transform:uppercase;font-weight:700}.kpi-value{font-size:28px;font-weight:800;color:#142e36;margin:4px 0;letter-spacing:-.7px}.kpi-note{font-size:12px;color:#63787b}.match-card{background:white;border:1px solid #dfe8e5;border-radius:12px;padding:16px;margin-bottom:13px}.match-top{display:flex;justify-content:space-between;align-items:center;color:#63787b;font-size:12px}.score-row{display:grid;grid-template-columns:1fr 90px 1fr;align-items:center;text-align:center;gap:8px;padding:14px 0}.score-team{font-weight:700;font-size:15px}.score{font-size:30px;font-weight:800;line-height:1.1}.micro{font-size:12px;color:#62777a}.outcomes{display:flex;height:23px;border-radius:5px;overflow:hidden;color:white;font-size:11px;text-align:center;line-height:23px}.outcomes>span{min-width:0;overflow:hidden}.outcome-labels{display:flex;justify-content:space-between;font-size:11px;color:#62777a;margin-top:4px}.source-note{padding:10px 13px;border-left:3px solid #087f73;background:#e8f2ef;margin:0 0 15px;color:#355b59;font-size:13px;border-radius:3px}.muted{font-size:12px;color:#60777a}.shiny-output-error-validation{color:#64777a;padding:15px}.dataTables_wrapper{font-size:13px}.btn-primary{background:#087f73;border-color:#087f73}.form-control,.form-select{border-radius:7px}.footer-note{font-size:12px;color:#62777a;margin:20px 0}.split-bar{display:flex;height:24px;border-radius:6px;overflow:hidden;color:white;font-weight:bold;font-size:12px;text-align:center;line-height:24px}.split-bar span{overflow:hidden}.method-copy{line-height:1.65}.method-copy h4{font-weight:750;margin-top:20px}.method-copy p{color:#40595d}.table> :not(caption)>*>*{vertical-align:middle}
"
section <- function(title, subtitle) div(class="pagehead", h2(title), p(subtitle))
card <- function(title, ...) div(class="panel-card", h3(title), ...)
kpi <- function(label, value, note="") div(class="kpi", div(class="kpi-label",label), div(class="kpi-value",value), div(class="kpi-note",note))
source_note <- function(...) div(class="source-note", ...)
plot_theme <- theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank(), panel.grid.major.y=element_blank(), plot.title=element_text(face="bold"), axis.title=element_text(colour=ink), axis.text=element_text(colour=ink), legend.position="bottom", plot.background=element_rect(fill="white", colour=NA), panel.background=element_rect(fill="white", colour=NA))
make_dt <- function(x, page=12) DT::datatable(x, rownames=FALSE, filter="none", options=list(pageLength=page, scrollX=TRUE, autoWidth=TRUE, dom="tip", order=list()), escape=TRUE)
prob_bar <- function(h,d,a) {
  ps <- c(h,d,a); ps <- ps/sum(ps)
  div(div(class="outcomes", lapply(seq_along(ps), function(i) tags$span(style=sprintf("width:%.8f%%;background:%s",100*ps[i],c(blue,"#738387",teal)[i]),pct(c(h,d,a)[i])))), div(class="outcome-labels", span("Home"),span("Draw"),span("Away")))
}
ui <- navbarPage(title="Super 6 Predictor", id="page", theme=bs_theme(version=5, bootswatch="flatly", primary=teal), collapsible=TRUE,
  header=tagList(tags$head(tags$style(HTML(css))), div(class="d-flex flex-wrap align-items-center justify-content-between gap-2 mt-3", div(class="muted", textOutput("connection", inline=TRUE)), actionButton("refresh","Refresh data", icon=icon("refresh"), class="btn-sm"))),
  tabPanel("Matchweek", value="forecasts",
    uiOutput("archive_notice"), fluidRow(column(8, selectInput("run", "Forecast run", choices=NULL)), column(4, selectInput("fixture_sort", "Sort fixtures", c("Date"="date", "Most concentrated top six"="concentration")))),
    uiOutput("forecast_kpis"), uiOutput("fixture_cards"), card("Forecast summary", DTOutput("forecast_table")),
    div(class="footer-note", "A scoreline is a model estimate, not a guarantee. The six ranked scores refer to alternatives for each fixture, not six different matches.")),
  tabPanel("Match explorer", value="explorer", div(class="pagehead", h2("Probability distribution")),
    fluidRow(column(8, selectInput("match", "Select a fixture", choices=NULL)), column(4, selectInput("component", "Probability model", c("M10-A ensemble"="probability", "M2 fixture Poisson"="probability_m2", "M3-W weighted Poisson"="probability_m3w")))),
    uiOutput("match_header"), fluidRow(column(7, card("Scoreline probability matrix · 0–6 goals", plotlyOutput("heatmap", height="410px"), textOutput("matrix_note"))), column(5, card("Highest-ranked scorelines", plotlyOutput("top_scores", height="410px"), textOutput("top_note")))),
    fluidRow(column(6, card("Match outcome probabilities", plotlyOutput("outcome_plot", height="260px"))), column(6, card("Component scoring rates", DTOutput("component_table"), p(class="muted", "Scoring rates are expected goals from the forecast distribution, not observed shot-based xG."))))),
  tabPanel("Team intelligence", value="teams", section("The changing strength of clubs", "Elo and scoring trends"),
    fluidRow(column(6, selectInput("team", "Team", choices=NULL)), column(6, sliderInput("form_window", "Rolling matches for display", min=5,max=20,value=10,step=5))),
    uiOutput("team_kpis"), card("Elo history · development period", plotlyOutput("elo_plot", height="330px")), card("Rolling league scoring and chance quality", plotlyOutput("team_form", height="360px"), p(class="muted", "Descriptive rolling averages include each match displayed; they are not the lagged features used to train the forecasts. Understat xG is kept separate from other providers."))),
  tabPanel("Model laboratory", value="lab", section("Model evaluation", "Compare the frozen model specifications and their performance on unseen seasons."),
    uiOutput("research_kpis"), fluidRow(column(4, radioButtons("period", "Evaluation period", c("Final holdout"="holdout","Development validation"="validation"), inline=TRUE)), column(4, selectInput("metric","Metric", c("Log loss (lower is better)"="log_loss","Exact accuracy (%)"="exact_accuracy","Top-three accuracy (%)"="top3_accuracy","Top-five accuracy (%)"="top5_accuracy","Outcome accuracy (%)"="outcome_accuracy"))), column(4, checkboxInput("show_market","Include later-information market benchmarks", FALSE))),
    source_note("Market odds may have been recorded after the Monday cutoff. They are a separate benchmark, not a like-for-like comparison. Validation uses the frozen reported snapshot; holdout tables use local output where available and otherwise the reported snapshot."),
    card("Model leaderboard", plotlyOutput("leaderboard", height="430px"), DTOutput("leaderboard_table")),
    fluidRow(column(6, card("Seasonal accuracy · holdout", plotlyOutput("seasonal_plot", height="350px"))), column(6, card("Exact accuracy by fixture type · holdout", plotlyOutput("fixture_plot", height="350px")))),
    card("Frozen ensemble composition", plotlyOutput("weights_plot", height="230px")),
    card("Paired block bootstrap · holdout", radioButtons("unc_metric","Improvement metric",c("Log loss reduction"="loss","Exact accuracy gain (pp)"="accuracy"),inline=TRUE), plotlyOutput("uncertainty_plot", height="260px"), p(class="muted","Positive values favour the candidate. Approximate 95% intervals are exploratory, unadjusted for multiple comparisons, and do not establish equivalence when they cross zero.")),
    card("Calibration · selected scoreline", plotlyOutput("calibration_plot", height="330px"), textOutput("calibration_note"))),
  tabPanel("Live performance", value="tracking", section("Forecast vs actual outcomes", "Only timestamped local forecasts issued before match day are eligible for prospective scoring."), uiOutput("tracking_notice"), uiOutput("tracking_kpis"), uiOutput("tracking_rank_kpis"), card("Cumulative correct scorelines", plotlyOutput("tracking_plot",height="340px")), card("Forecast ledger", DTOutput("tracking_table")), downloadButton("download_tracking","Export tracking CSV")),
  tabPanel("Methodology", value="method", section("How it works", "A reproducible, chronology-aware sports analytics project."),
    fluidRow(column(7, card("Experimental design", div(class="method-copy", h4("Training and evaluation"), p("Initial training: 2016/17–2020/21. Development validation: 594 Big Six fixtures in 2021/22–2023/24. Independent historical holdout: 396 fixtures in 2024/25–2025/26. Models are refit using only results before each Monday cutoff; hyperparameters and ensemble weights were frozen before holdout evaluation."), h4("Production · M10-A"), p("20% fixture-based independent Poisson (M2) + 80% time-weighted attack/defence Poisson (M3-W). The regression uses a 180-day half-life. Probabilities are pooled at scoreline level, not by averaging scoreline predictions."), h4("Research · M10-B"), p("10% M2, 30% M3-W, 20% rolling xG form and 40% hybrid xG. Understat rolling features use ten prior league matches, a 365-day maximum age and a five-match shrinkage prior. Current-season xG provider consistency is unresolved, so this is not the deployed model."), h4("External benchmark · M8"), p("Normalise historical 1X2 and over/under 2.5 odds and reconstruct Poisson scoring rates. Collection timing may differ from our Monday cutoff.")))),
    column(5, card("Limitations and interpretation", div(class="method-copy", p("Exact scorelines are low-probability outcomes. A model can improve log loss without improving the number of correct first-choice scores."), p("The 2.47% holdout log loss reduction of M10-B versus M2 is an observed comparison, not a guarantee of future performance. Exact accuracy improvements remain statistically inconclusive."), p("The model is validated for Premier League Big Six fixtures. It has not reconstructed actual six-match competition rounds. Forecasts are not betting advice."), p("Prospective tracking takes the earliest archived run generated before midnight UK time on match day. Rescheduled fixtures and same-day forecasts require verified kickoff identifiers/timestamps."), p("Data sources: Football-Data historical results and market odds, Understat xG via xGTables historical archive. No new external requests are made by this dashboard."))), card("Data connection", verbatimTextOutput("paths"))))),
  footer=div(class="footer-note", "Super 6 Predictor · R / Shiny · Frozen research and prospective forecasting kept separate")
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  observeEvent(input$refresh, {
    a <- load_archive()
    m <- safe_rds(project("data", "processed", "premier_league_master.rds"))
    e <- safe_csv(project("data", "processed", "elo_rating_history.csv"))
    data(list(archive=a, master=m, elo=e, holdout=read_holdout(),
      seasonal=read_detail("holdout_seasonal_performance.csv","seasonal.csv",c("model","season","exact_accuracy")),
      fixture=read_detail("holdout_fixture_performance.csv","fixture.csv",c("model","fixture_type","exact_accuracy")),
      uncertainty=read_detail("holdout_uncertainty.csv","uncertainty.csv",c("candidate","benchmark","loss_gain","loss_lower","loss_upper","accuracy_gain","accuracy_lower","accuracy_upper")),
      updated=Sys.time()))
    choices <- if (nrow(a)) {
      runs <- a %>% group_by(run_id) %>% summarise(stamp=max(generated_at), n=n(), .groups="drop") %>% arrange(desc(stamp))
      setNames(runs$run_id, paste(format(runs$stamp, "%d %b %Y %H:%M", tz="Europe/London"), "UK ·",runs$n,"fixtures ·",runs$run_id))
    } else c("26 Nov 2026 · supplied reference snapshot"="reference")
    prev <- isolate(input$run)
    updateSelectInput(session,"run",choices=choices,selected=if (!is.null(prev) && prev %in% choices) prev else unname(choices[1]))
    teams <- if (has(m,c("home_team","away_team"))) sort(unique(c(m$home_team,m$away_team))) else if (has(e,"team")) sort(unique(e$team)) else character()
    prev_team <- isolate(input$team)
    updateSelectInput(session,"team",choices=teams,selected=if (!is.null(prev_team) && prev_team %in% teams) prev_team else if ("Arsenal" %in% teams) "Arsenal" else if(length(teams)) teams[1] else character())
  }, ignoreNULL=FALSE)
  output$connection <- renderText({d<-req(data()); paste(if(nrow(d$archive)) "Local forecast archive connected" else "Reference mode · no local archive", "|",format(d$updated,"%d %b %H:%M",tz="Europe/London"),"UK")})
  output$paths <- renderText({d<-req(data()); paste("Project root:",project_root,"\nArchive forecasts:",nrow(d$archive),"\nHistorical results:",nrow(d$master),"\nElo rows:",nrow(d$elo),"\nReference files:",file.path(app_dir,"reference"),sep=" ")})
  current <- reactive({d<-req(data()); run<-req(input$run); if(run=="reference") reference_forecast() else d$archive %>% filter(run_id==run)})
  observeEvent(current(), {
    x<-current(); req(nrow(x)>0)
    choices<-setNames(x$match_id,paste(format(x$date,"%d %b"),"·",x$home_team,"vs",x$away_team))
    prev<-isolate(input$match)
    updateSelectInput(session,"match",choices=choices,selected=if(!is.null(prev)&&prev %in% choices) prev else unname(choices[1]))
  })
  selected <- reactive({x<-current(); m<-req(input$match); y<-x[x$match_id==m,,drop=FALSE]; req(nrow(y)==1); y})
  distribution <- reactive({req(data()); read_distribution(req(input$run))})
  top_data <- reactive({req(data()); read_top(req(input$run),current())})
  selected_dist <- reactive({x<-distribution(); if(!nrow(x)) return(x); x %>% filter(match_id==selected()$match_id)})
  output$archive_notice <- renderUI({x<-current(); if(!nrow(x)) return(source_note("No valid forecast runs found.")); if(input$run=="reference") source_note("Reference snapshot: your five reported forecasts from 26 November 2026. Rounded values only; full distributions and component rates are not supplied. These are excluded from prospective tracking.") else source_note(paste("Generated",format(min(x$generated_at),"%d %B %Y %H:%M:%S",tz="Europe/London"),"UK · Information cutoff",format(min(x$prediction_cutoff),"%d %B %Y"),"· Archive",input$run))})
  output$forecast_kpis <- renderUI({x<-current(); req(nrow(x)); fluidRow(column(3,kpi("Fixtures in run",nrow(x),"Premier League Big Six")),column(3,kpi("Mean score confidence",pct(mean(x$confidence)),"First-ranked exact score")),column(3,kpi("Mean top-six coverage",pct(mean(x$top6_probability)),"Combined alternative probability")),column(3,kpi("Production model",paste(unique(x$model),collapse=", "),"Frozen forecast specification")))})
  output$fixture_cards <- renderUI({x<-current(); req(nrow(x)); if(identical(input$fixture_sort,"concentration")) x<-arrange(x,desc(top6_probability)) else x<-arrange(x,date,match_id); fluidRow(lapply(seq_len(nrow(x)),function(i){r<-x[i,]; column(6,div(class="match-card",div(class="match-top",span(format(r$date,"%A · %d %b")),span(paste("Top six:",pct(r$top6_probability)))),div(class="score-row",div(class="score-team",r$home_team),div(div(class="score",gsub("-","–",r$predicted_scoreline)),div(class="micro",pct(r$confidence))),div(class="score-team",r$away_team)),prob_bar(r$p_home,r$p_draw,r$p_away),div(class="micro mt-2",paste("Expected goals",fmt(r$expected_home_goals),"–",fmt(r$expected_away_goals))),actionButton(paste0("explore_",i),"Explore distribution",class="btn-sm btn-outline-primary mt-2",onclick=sprintf("Shiny.setInputValue('explore_match', %s, {priority:'event'})",jsonlite::toJSON(r$match_id,auto_unbox=TRUE)))))}))})
  observeEvent(input$explore_match,{updateSelectInput(session,"match",selected=input$explore_match); updateNavbarPage(session,"page",selected="explorer")})
  output$forecast_table <- renderDT({x<-current(); validate(need(nrow(x),"No forecasts found.")); make_dt(x %>% transmute(Date=date,Home=home_team,Away=away_team,Score=predicted_scoreline,Confidence=pct(confidence),`Home win`=pct(p_home),Draw=pct(p_draw),`Away win`=pct(p_away),`Top six`=pct(top6_probability)),page=10)})
  output$match_header <- renderUI({r<-selected(); fluidRow(column(3,kpi("Ensemble forecast",r$predicted_scoreline,paste(r$home_team,"vs",r$away_team))),column(3,kpi("Exact score confidence",pct(r$confidence),"Ensemble probability")),column(3,kpi("Expected home goals",fmt(r$expected_home_goals),r$home_team)),column(3,kpi("Expected away goals",fmt(r$expected_away_goals),r$away_team)))})
  output$heatmap <- renderPlotly({
    x <- selected_dist(); col <- req(input$component)
    if (!nrow(x) && col == "probability") {
      t <- top_data(); validate(need(has(t,c("match_id","scoreline","probability")),"No scoreline probabilities available."))
      x <- t %>% filter(match_id == selected()$match_id) %>% tidyr::extract(scoreline, into=c("home_goals","away_goals"), regex="^([0-9]+)-([0-9]+)$", remove=FALSE, convert=TRUE)
    }
    validate(need(nrow(x)>0,"Full component grid unavailable. Connect your archived scoreline_distributions.csv."),need(col %in% names(x),"This component is unavailable in the archive."))
    grid <- x %>% filter(home_goals<=6,away_goals<=6)
    z <- matrix(NA_real_,7,7); z[cbind(grid$home_goals+1,grid$away_goals+1)] <- grid[[col]]
    hover <- matrix(sprintf("Home %d – Away %d<br>Probability: %.2f%%",rep(0:6,7),rep(0:6,each=7),100*as.vector(z)),7,7)
    plot_ly(x=0:6,y=0:6,z=100*z,type="heatmap",colorscale=list(c(0,"#edf5f1"),c(1,"#087f73")),text=hover,hoverinfo="text",hoverongaps=FALSE,colorbar=list(title="%")) %>% layout(xaxis=list(title="Away goals",dtick=1),yaxis=list(title="Home goals",dtick=1,autorange="reversed"),plot_bgcolor="#eef1f0",margin=list(l=55,r=15,b=50,t=10)) %>% config(displayModeBar=FALSE)
  })
  output$matrix_note <- renderText({x<-selected_dist(); col<-req(input$component); if(!nrow(x)) return(if(col=="probability") "Partial grid: only the archived top six are shown. Blank cells are unknown, not zero." else "No full component grid available."); if(!col %in% names(x)) return("No component grid available."); paste("Displayed cells contain",pct(sum(x[[col]][x$home_goals<=6 & x$away_goals<=6]),2),"of the probability mass. Remaining mass lies outside the displayed grid.")})
  ranked <- reactive({col<-req(input$component); x<-selected_dist(); if(nrow(x)&&col %in% names(x)) return(x %>% mutate(p=.data[[col]]) %>% arrange(desc(p),home_goals,away_goals) %>% slice_head(n=6)); if(col!="probability") return(tibble()); t<-top_data(); if(!has(t,c("match_id","scoreline","probability","rank"))) return(tibble()); t %>% filter(match_id==selected()$match_id) %>% arrange(rank) %>% mutate(p=probability)})
  output$top_scores <- renderPlotly({x<-ranked(); validate(need(nrow(x),"Ranked scores unavailable for this component.")); plot_ly(x=100*x$p,y=factor(x$scoreline,levels=rev(x$scoreline)),type="bar",orientation="h",marker=list(color=teal),text=pct(x$p,2),textposition="auto",hovertemplate="%{y}: %{x:.2f}%<extra></extra>") %>% layout(xaxis=list(title="Probability (%)",rangemode="tozero"),yaxis=list(title="",categoryorder="array",categoryarray=rev(x$scoreline)),margin=list(l=50,r=20,t=10,b=50)) %>% config(displayModeBar=FALSE)})
  output$top_note <- renderText({x<-ranked(); if(!nrow(x)) return(""); paste("Combined displayed probability:",pct(sum(x$p),2),if(input$run=="reference") "(rounded inputs)." else "")})
  output$outcome_plot <- renderPlotly({r<-selected(); x<-selected_dist(); col<-req(input$component); p<-if(nrow(x)&&col %in% names(x)) c(sum(x[[col]][x$home_goals>x$away_goals]),sum(x[[col]][x$home_goals==x$away_goals]),sum(x[[col]][x$home_goals<x$away_goals])) else if(col=="probability") c(r$p_home,r$p_draw,r$p_away) else numeric(); validate(need(length(p)==3,"Component outcome probabilities unavailable.")); plot_ly(x=c("Home win","Draw","Away win"),y=100*p,type="bar",marker=list(color=c(blue,"#738387",teal)),text=pct(p),textposition="auto",hovertemplate="%{x}: %{y:.2f}%<extra></extra>") %>% layout(yaxis=list(title="Probability (%)",range=c(0,100)),xaxis=list(title=""),margin=list(l=55,t=10,b=40)) %>% config(displayModeBar=FALSE)})
  output$component_table <- renderDT({r<-selected(); validate(need(input$run!="reference","Component rates were not included in the supplied snapshot.")); x<-safe_csv(project("outputs","forecasts",input$run,"component_rates.csv")); validate(need(has(x,c("match_id","M2_h","M2_a","M3W_h","M3W_a")),"Missing component_rates.csv.")); x<-x[as.character(x$match_id)==r$match_id,,drop=FALSE]; validate(need(nrow(x)==1,"Component rates missing for this match.")); make_dt(tibble(Model=c("M2","M3-W","M10-A"),Home=fmt(c(x$M2_h,x$M3W_h,r$expected_home_goals)),Away=fmt(c(x$M2_a,x$M3W_a,r$expected_away_goals))),page=3)})
  team_key <- reactive({d<-req(data()); t<-req(input$team); m<-d$master; if(has(m,c("home_team","home_key","away_team","away_key"))){v<-unique(c(m$home_key[m$home_team==t],m$away_key[m$away_team==t])); if(length(v)) return(v[1])}; t})
  team_matches <- reactive({d<-req(data()); m<-d$master; key<-team_key(); validate(need(has(m,c("date","home_key","away_key","home_goals","away_goals")),"Connect premier_league_master.rds to view team match history.")); if(!"home_xg_understat" %in% names(m)) m$home_xg_understat<-NA_real_; if(!"away_xg_understat" %in% names(m)) m$away_xg_understat<-NA_real_; if("totals_unreliable" %in% names(m)){m$home_xg_understat[is.na(m$totals_unreliable)|m$totals_unreliable]<-NA_real_;m$away_xg_understat[is.na(m$totals_unreliable)|m$totals_unreliable]<-NA_real_}; bind_rows(m %>% filter(home_key==key) %>% transmute(date=as.Date(date),gf=home_goals,ga=away_goals,xgf=home_xg_understat,xga=away_xg_understat),m %>% filter(away_key==key) %>% transmute(date=as.Date(date),gf=away_goals,ga=home_goals,xgf=away_xg_understat,xga=home_xg_understat)) %>% filter(!is.na(gf),!is.na(ga)) %>% arrange(date)})
  output$team_kpis <- renderUI({x<-team_matches(); validate(need(nrow(x)>0,"No completed matches for this team.")); last<-tail(x,input$form_window); fluidRow(column(3,kpi("Historical matches",nrow(x),input$team)),column(3,kpi("Recent goals / match",fmt(mean(last$gf)),paste(nrow(last),"league matches"))),column(3,kpi("Recent conceded / match",fmt(mean(last$ga)),paste(nrow(last),"league matches"))),column(3,kpi("Latest result date",format(max(x$date),"%d %b %Y"),"Historical data")))})
  output$elo_plot <- renderPlotly({d<-req(data()); x<-d$elo; key<-team_key(); validate(need(has(x,c("team","week","elo")),"Elo history missing. Run R/13_elo_ratings.R.")); x<-x %>% filter(team==key) %>% mutate(week=as.Date(week)) %>% arrange(week); validate(need(nrow(x),"No Elo observations for this team.")); ggplotly(ggplot(x,aes(week,elo))+geom_hline(yintercept=1500,linetype=2,colour="#9aabaa")+geom_line(colour=teal,linewidth=1)+labs(x=NULL,y="Elo rating")+plot_theme) %>% config(displayModeBar=FALSE)})
  output$team_form <- renderPlotly({x<-team_matches(); n<-req(input$form_window); validate(need(nrow(x)>=n,"Not enough league results for this rolling window.")); roll<-function(v) vapply(seq_along(v),function(i){if(i<n) return(NA_real_); z<-v[(i-n+1):i]; if(anyNA(z)) NA_real_ else mean(z)},numeric(1)); x<-x %>% mutate(across(c(gf,ga,xgf,xga),roll)) %>% pivot_longer(c(gf,ga,xgf,xga),names_to="metric",values_to="value") %>% mutate(metric=recode(metric,gf="Goals scored",ga="Goals conceded",xgf="Understat xG",xga="Understat xGA")); ggplotly(ggplot(x,aes(date,value,colour=metric))+geom_line(linewidth=.8,na.rm=TRUE)+scale_colour_manual(values=c("Goals scored"=teal,"Goals conceded"=blue,"Understat xG"=amber,"Understat xGA"="#8866a5"))+labs(x=NULL,y="Rolling goals per match",colour=NULL)+plot_theme) %>% config(displayModeBar=FALSE)})
  output$research_kpis <- renderUI({req(data()); fluidRow(column(3,kpi("Holdout fixtures",396,"2024/25–2025/26")),column(3,kpi("M10-B log loss", "2.943232","Historical-data leader")),column(3,kpi("Log loss improvement","2.47%","M10-B vs M2")),column(3,kpi("Best exact accuracy","12.88%","M7-A · 51 / 396")))})
  lab_data <- reactive({d<-req(data()); x<-if(input$period=="validation") safe_csv(ref("validation.csv")) %>% mutate(source="Reported snapshot") else d$holdout; if(!isTRUE(input$show_market)) x<-filter(x,!model %in% market_models); x %>% mutate(timing=ifelse(model %in% market_models,"Later-information market","Monday historical"),label=model_label(model))})
  output$leaderboard <- renderPlotly({x<-lab_data(); metric<-req(input$metric); validate(need(metric %in% names(x),"This metric was not recorded for this evaluation period.")); x<-x %>% filter(is.finite(.data[[metric]])); validate(need(nrow(x),"No values available for this metric.")); x<-if(metric=="log_loss") arrange(x,.data[[metric]]) else arrange(x,desc(.data[[metric]])); p<-ggplot(x,aes(x=.data[[metric]],y=factor(label,levels=rev(label)),fill=timing,text=paste(label,"<br>",source,"<br>",if(metric=="log_loss") fmt(.data[[metric]],6) else paste0(fmt(.data[[metric]],2),"%"))))+geom_col(width=.68)+scale_fill_manual(values=c("Monday historical"=teal,"Later-information market"=amber))+labs(x=if(metric=="log_loss") "Log loss (lower is better)" else "Accuracy (%)",y=NULL,fill=NULL)+plot_theme; ggplotly(p,tooltip="text") %>% config(displayModeBar=FALSE)})
  output$leaderboard_table <- renderDT({x<-lab_data(); metric<-req(input$metric); if(metric %in% names(x)) x<-if(metric=="log_loss") arrange(x,.data[[metric]]) else arrange(x,desc(.data[[metric]])); x<-x %>% select(any_of(c("model","matches","correct_scorelines","exact_accuracy","top3_accuracy","top5_accuracy","outcome_accuracy","log_loss","source","timing"))) %>% mutate(across(any_of(c("exact_accuracy","top3_accuracy","top5_accuracy","outcome_accuracy")),~round(.x,2)),log_loss=round(log_loss,6)); make_dt(x,16)})
  output$seasonal_plot <- renderPlotly({x<-req(data())$seasonal; validate(need(nrow(x),"Seasonal results unavailable.")); x<-x %>% mutate(season=as.character(season),season=ifelse(nchar(season)==4,paste0(substr(season,1,2),"/",substr(season,3,4)),season)); ggplotly(ggplot(x,aes(model,exact_accuracy,fill=season))+geom_col(position="dodge")+scale_fill_manual(values=c("#547c9a","#087f73"))+labs(x=NULL,y="Exact accuracy (%)",fill="Season")+plot_theme) %>% config(displayModeBar=FALSE)})
  output$fixture_plot <- renderPlotly({x<-req(data())$fixture; validate(need(nrow(x),"Fixture type results unavailable.")); ggplotly(ggplot(x,aes(fixture_type,model,fill=exact_accuracy,text=paste(model,fixture_type,paste0(fmt(exact_accuracy,2),"%"),sep="<br>")))+geom_tile(colour="white",linewidth=.6)+scale_fill_gradient(low="#eaf4ef",high=teal)+labs(x=NULL,y=NULL,fill="Accuracy %")+plot_theme+theme(axis.text.x=element_text(angle=20,hjust=1)),tooltip="text") %>% config(displayModeBar=FALSE)})
  output$weights_plot <- renderPlotly({x<-tibble(ensemble=c("M10A","M10A","M10B","M10B","M10B","M10B","M10C","M10C"),component=c("M2","M3W","M2","M3W","M7A","M7B","M3W","M8"),weight=c(.2,.8,.1,.3,.2,.4,.1,.9)); ggplotly(ggplot(x,aes(ensemble,100*weight,fill=component))+geom_col(width=.6)+coord_flip()+scale_fill_manual(values=c(M2="#9aa9b6",M3W=teal,M7A=blue,M7B="#8b70aa",M8=amber))+labs(x=NULL,y="Weight (%)",fill=NULL)+plot_theme) %>% config(displayModeBar=FALSE)})
  output$uncertainty_plot <- renderPlotly({x<-req(data())$uncertainty; kind<-req(input$unc_metric); x<-x %>% mutate(comparison=paste(candidate,"vs",benchmark),gain=.data[[paste0(kind,"_gain")]],lower=.data[[paste0(kind,"_lower")]],upper=.data[[paste0(kind,"_upper")]]); ggplotly(ggplot(x,aes(gain,comparison))+geom_vline(xintercept=0,linetype=2,colour="#82918e")+geom_segment(aes(x=lower,xend=upper,yend=comparison),linewidth=1,colour=blue)+geom_point(size=3,colour=teal)+labs(x=if(kind=="loss") "Log loss reduction" else "Accuracy gain (percentage points)",y=NULL)+plot_theme) %>% config(displayModeBar=FALSE)})
  holdout_cal <- reactive({req(data()); x<-safe_csv(project("outputs","holdout","holdout_predictions.csv")); if(!has(x,c("model","prediction_probability","correct"))) return(tibble()); x %>% mutate(band=cut(prediction_probability,breaks=c(0,.075,.10,.125,.15,.20,.30,1),include.lowest=TRUE)) %>% group_by(model,band) %>% summarise(matches=n(),confidence=mean(prediction_probability),observed=mean(as.logical(correct)),.groups="drop")})
  output$calibration_plot <- renderPlotly({x<-holdout_cal(); validate(need(nrow(x),"Local holdout_predictions.csv is needed for reliability bands; aggregate confidence alone cannot establish calibration.")); ggplotly(ggplot(x,aes(100*confidence,100*observed,colour=model,size=matches,text=paste(model,"<br>Matches:",matches)))+geom_abline(slope=1,intercept=0,linetype=2,colour="#879894")+geom_point(alpha=.8)+labs(x="Mean predicted confidence (%)",y="Observed exact accuracy (%)",colour="Model",size="Matches")+plot_theme,tooltip=c("text","x","y")) %>% config(displayModeBar=FALSE)})
  output$calibration_note <- renderText({x<-holdout_cal(); if(!nrow(x)) "Aggregate calibration bias is available in your reported holdout results, but bands require match-level predictions." else "Sparse bands are noisy; this measures top-ranked scoreline calibration only."})
  tracking <- reactive({d<-req(data()); track_archive(d$archive,d$master)})
  output$tracking_notice <- renderUI({d<-req(data()); if(!nrow(d$archive)) source_note("No local archive. Reference forecasts are not scored as live predictions.") else if(!nrow(d$master)) source_note("Historical master dataset is missing. Results cannot yet be matched.") else source_note("Earliest eligible archive per fixture and model · generated before midnight UK time on match day. A missing result may indicate a pending or rescheduled fixture.")})
  output$tracking_kpis <- renderUI({x<-tracking(); n<-if(nrow(x)) sum(!is.na(x$correct)) else 0; k<-if(n) sum(x$correct,na.rm=TRUE) else 0; losses<-if(n) x$log_loss[is.finite(x$log_loss)] else numeric(); fluidRow(column(3,kpi("Scored forecasts",n,paste(nrow(x)-n,"awaiting result"))),column(3,kpi("Exact accuracy",if(n) pct(k/n) else "—",paste(k,"correct"))),column(3,kpi("Expected correct",if(n) fmt(sum(x$confidence[!is.na(x$correct)])) else "—","Sum of forecast confidence")),column(3,kpi("Mean log loss",if(length(losses)) fmt(mean(losses),4) else "—",paste(length(losses),"scored distributions"))))})
  output$tracking_rank_kpis <- renderUI({x<-tracking(); calc<-function(v) {n<-sum(!is.na(v)); if(n) pct(mean(v,na.rm=TRUE)) else "—"}; n3<-if(nrow(x)) sum(!is.na(x$top3_correct)) else 0; n6<-if(nrow(x)) sum(!is.na(x$top6_correct)) else 0; fluidRow(column(6,kpi("Top-three accuracy",if(n3) calc(x$top3_correct) else "—",paste(n3,"scored forecasts with ranking data"))),column(6,kpi("Top-six accuracy",if(n6) calc(x$top6_correct) else "—",paste(n6,"scored forecasts with ranking data"))))})
  output$tracking_plot <- renderPlotly({x<-tracking(); validate(need(nrow(x),"No eligible local forecasts yet.")); x<-x %>% filter(!is.na(correct)) %>% group_by(date) %>% summarise(actual=sum(correct),expected=sum(confidence),.groups="drop") %>% arrange(date) %>% mutate(actual=cumsum(actual),expected=cumsum(expected)) %>% pivot_longer(c(actual,expected),names_to="series",values_to="correct"); validate(need(nrow(x),"Awaiting completed matches.")); ggplotly(ggplot(x,aes(date,correct,colour=series))+geom_line(linewidth=1)+geom_point(size=2)+scale_colour_manual(values=c(actual=teal,expected=blue))+labs(x=NULL,y="Cumulative correct",colour=NULL)+plot_theme) %>% config(displayModeBar=FALSE)})
  output$tracking_table <- renderDT({x<-tracking(); validate(need(nrow(x),"No eligible local forecasts yet.")); make_dt(x %>% transmute(Date=date,Home=home_team,Away=away_team,Model=model,Forecast=predicted_scoreline,Actual=actual_scoreline,Confidence=pct(confidence),Status=status,`Top 3`=top3_correct,`Top 6`=top6_correct,`Log loss`=round(log_loss,4),`Generated (UK)`=format(generated_at,"%Y-%m-%d %H:%M:%S",tz="Europe/London"),Run=run_id),15)})
  output$download_tracking <- downloadHandler(filename=function() paste0("forecast_tracking_",Sys.Date(),".csv"),content=function(file) write_csv(tracking(),file))
}
shinyApp(ui,server)

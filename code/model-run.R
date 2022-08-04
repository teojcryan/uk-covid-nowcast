library(epinowcast)
library(data.table)
library(scoringutils)
library(purrr)
library(here)
source(here("code", "model.R"))
#source(here("code", "model-tools.R"))
#source(here("code", "utils.R"))

# Load data ------
  # Default reporting schedule
obs_dt <- readRDS(here::here("data", "observations", "all.rds"))[
  , c('reference_date', 'report_date', 'confirm')
]

  # Weekly reporting schedule
obs_wk <- readRDS(here::here("data", "observations", "all_wkrep.rds"))[
  , c('reference_date', 'report_date', 'confirm')
]

# Set parameters
d_max <- 10                          # max delay
days_included <- 21                  # length of training set
date_latest <- max(obs_dt$report_date)   # latest report date available, "ground truth"

date_start <- as.Date("2022-02-01") + days_included
date_end <- as.Date("2022-07-01")

# Complete dates
  # Default
obs_dt <- enw_complete_dates(obs_dt, max_delay = d_max)
obs_dt[, holiday := FALSE]

  # Weekly
obs_wk <- enw_complete_dates(obs_dt, max_delay = d_max)
obs_wk[, holiday := FALSE]

date_list <- seq(date_start, date_end, by = "days")

for (i in 1:length(date_list[1:5])){
  date_nowcast <- date_list[i]
  
  obs <- obs_dt |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  
  # Model 1: Reference fixed, report fixed
  nowcast <- nowcast_model(pobs, 
                           report = "fixed", 
                           reference = "fixed", 
                           max_delay = d_max)
  
  priors <- summary(
    nowcast,
    type = "fit",
    variables = c( "refp_mean_int", "refp_sd_int", "sqrt_phi")
  )
  priors[, sd := sd * 5]
  
  # Model 2: Reference fixed, report dow
  dow_nowcast <- nowcast_model(pobs, 
                               report = "dow", 
                               reference = "fixed", 
                               priors = priors, 
                               max_delay = d_max)
  
  # Model 3: Model 2 + Holidays
  # pobs_hols
  
  # Model 4: Model 2 on weekly reported data
  # pobs_week
  
  
  
  # Summarise nowcasts
  nowcasts <- list(
    "Reference: Fixed, Report: Fixed" = nowcast,
    "Reference: Fixed, Report: Day of Week" = dow_nowcast
  )
  
  summarised_nowcasts <- map(
    nowcasts, summary,
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
    ) 
  
  summarised_nowcasts <- rbindlist(summarised_nowcasts, 
                                   idcol = "model", use.names = TRUE)
  summarised_nowcasts[, nowcast_date := date_nowcast]
  
  # Store nowcasts
  write.csv(summarised_nowcasts,
            here("data", "nowcasts", "daily", paste0(date_nowcast, ".csv")),
            row.names = F)
  
  # Update latest data
  latest <- obs_dt |>
    enw_filter_report_dates(latest_date ="2022-07-16") |>
    enw_latest_data() |>
    enw_filter_reference_dates(latest_date = date_nowcast, include_days = d_max)
  
  # Store scores
  score <- enw_score_nowcast(
    summarised_nowcasts,
    latest[reference_date > (max(reference_date) - 7)]
  )
  
  write.csv(score,
            here("data", "scores", paste0(date_nowcast, ".csv")),
            row.names = F)
  
  # Store diagnostics
  diag <- map(nowcasts, 
              function(x){
                x <- copy(x)
                out <- x[, nowcast_date := max_date
                ][,`samples`:`nowcast_date`]
                return(out)
              })
  diag <- rbindlist(diag, idcol = "model", use.names = TRUE)
  
  write.csv(diag,
            here("data", "diagnostics",paste0(date_nowcast, ".csv")),
            row.names = F)
}

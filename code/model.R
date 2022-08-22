# Utility functions for model runs

# filter latest date
filter_obs <- function(obs, nowcast_date, days_included){
  # filter data based on estimation (nowcast) date
  obs_i <- obs |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  return(obs_i)
}

# summarise samples for 7-day moving average
summarise_week <- function(nowcast, probs = c(0.025, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75,
                                              0.8, 0.9, 0.95, 0.975)){
  samples <- summary(nowcast, type = "nowcast_samples")
  
  samples[sample < confirm, sample := confirm]
  samples[is.na(sample), sample := confirm]
  
  cols <- c("confirm", "sample")
  samples[, (cols) := lapply(.SD, data.table::frollsum, n = 7),
          .SDcols = cols, by = c(".draw")
  ]
  samples <- samples[!is.na(sample)]

  # Summarise 7 day nowcast
  seven_day <- enw_summarise_samples(samples, probs = probs)
  
  return(seven_day)
}

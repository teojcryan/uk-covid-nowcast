  # Packages ----------------------------------------------------------------
require(epinowcast, quietly = TRUE)
require(data.table, quietly = TRUE)
require(purrr, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(scoringutils, quietly = TRUE))
suppressMessages(require(here, quietly = TRUE))
source(here("code", "model.R"))

# Load data ---------------------------------------------------------------
# Merged dataset
obs_all <- readRDS(here::here("data", "observations", "all.rds"))[
  , c('reference_date', 'report_date', 'confirm')]

# Filter weekly reports on Wednesday only
obs_wk <- copy(obs_all)[, dow := wday(report_date)
][dow == 4][, dow := NULL]

# Holidays
holidays <- readRDS(here::here("data", "observations", "holidays.rds"))

# Set parameters ----------------------------------------------------------
d_max <- 10                              # max delay
days_included <- 21                      # length of training set
date_latest <- max(obs_all$report_date)  # latest report date available, "ground truth"

date_start <- as.Date("2022-02-01") + days_included
date_end <- as.Date("2022-07-01")
date_list <- seq(date_start, date_end, by = "days") # list of dates in range

# Prepare dates -----------------------------------------------------------
obs_all <- enw_complete_dates(obs_all, max_delay = d_max)

# Week day reporting, incl holidays
obs_hol <- merge(obs_all, holidays, by.x = "report_date", by.y = "date")

# Week day reporting, excl holidays
obs_all[, holiday := FALSE]

# Week day reporting, excl holidays
obs_wk <- enw_complete_dates(obs_wk, max_delay = d_max)
obs_wk[, holiday := FALSE]

# Run models -------------------------------------------------------------

time_start <- Sys.time()
print(paste("Model fitting started at", Sys.time()))
# Iterate over dates in range
for (i in 1:length(date_list)){
  date_nowcast <- date_list[i] 
  
  # Update observations based on nowcast date
  obs_all_i <- obs_all |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  
  obs_hol_i <- obs_hol |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  
  obs_wk_i <- obs_wk |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  
  # Model 1: Reference fixed, report fixed
  nowcast <- nowcast_model(obs_all_i, 
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
  dow_nowcast <- nowcast_model(obs_all_i, 
                               report = "dow", 
                               reference = "fixed", 
                               priors = priors, 
                               max_delay = d_max)
  
  # Model 3: Model 2 + Holidays
  hol_nowcast <- nowcast_model(obs_hol_i, 
                               report = "dow", 
                               reference = "fixed", 
                               priors = priors, 
                               max_delay = d_max)
  
  # Model 4: Model 2 on weekly reported data
  wkrep_nowcast <- nowcast_model(obs_wk_i, 
                                 report = "dow", 
                                 reference = "fixed", 
                                 priors = priors, 
                                 max_delay = d_max)
  
  # Summarise nowcasts
  nowcasts <- list(
    "Reference: Fixed, Report: Fixed" = nowcast,
    "Reference: Fixed, Report: Day of Week" = dow_nowcast,
    "Holidays" = hol_nowcast,
    "Weekly reporting" = wkrep_nowcast
  )
  
  summarised_nowcasts <- map(
    nowcasts, summary,
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
  ) 
  
  summarised_nowcasts <- rbindlist(summarised_nowcasts, 
                                   idcol = "model", use.names = TRUE)
  summarised_nowcasts[, `:=`(nowcast_date = date_nowcast,
                             max_confirm = NULL,
                             cum_prop_reported = NULL,
                             prop_reported = NULL)]
  
  # Store nowcasts
  write.table(summarised_nowcasts,
              file = here("data", "nowcasts", "all.csv"),
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("data", "nowcasts.csv")))
  
  # Update latest data
  latest <- obs_all |>
    enw_filter_report_dates(latest_date =date_latest) |>
    enw_latest_data() |>
    enw_filter_reference_dates(latest_date = date_nowcast, include_days = d_max)
  
  # Store scores
  score <- enw_score_nowcast(
    summarised_nowcasts,
    latest[reference_date > (max(reference_date) - 7)]
  )
  
  write.table(score,
              file = here("data", "scores", "all.csv"),
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("data", "scores.csv")))
  
  # Extract diagnostics
  diagnostics <- map(nowcasts, 
                     function(x){
                       x <- copy(x)
                       out <- x[, nowcast_date := max_date][,`samples`:`nowcast_date`]
                       return(out)
                     })
  diagnostics <- rbindlist(diag, idcol = "model", use.names = TRUE)
  
  # Store diagnostics
  write.table(diagnostics,
              file = here("data", "diagnostics", "all.csv"),
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("data", "diagnostics.csv")))
  
  # print progress
  cat(paste0("Progress: ", round(100*i/length(date_list), 3), "%", "\n",
             "Time elapsed: ", round(as.numeric(difftime(Sys.time(), time_start, units = "min")),3), " mins")) 
}

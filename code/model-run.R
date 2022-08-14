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
obs_wk <- copy(obs_all)[, dow := wday(report_date)][dow == 4][, dow := NULL]

# Holidays
holidays <- readRDS(here::here("data", "observations", "holidays.rds"))

# Set parameters ----------------------------------------------------------
d_max <- 10                              # max delay
days_included <- 21                      # length of training set
date_latest <- max(obs_all$report_date)  # latest report date available, "ground truth"
run_name <- "run4"   # Name of run

date_start <- as.Date("2022-02-01") + days_included
date_end <- as.Date("2022-07-01")
date_list <- seq(date_start, date_end, by = "days") # list of dates in range

# Prepare dates -----------------------------------------------------------
obs_all <- enw_complete_dates(obs_all, max_delay = d_max)

# 1. Week day reporting, incl holidays
obs_hol <- merge(obs_all, holidays, by.x = "report_date", by.y = "date")
obs_hol[, weekend := ifelse(wday(report_date) %in% c(1,7), TRUE, FALSE)]

# 2. Week day reporting, excl holidays
obs_all[, holiday := FALSE]
  # apply variable to identify weekends
obs_all[, weekend := ifelse(wday(report_date) %in% c(1,7), TRUE, FALSE)]

# 3. Weekly reporting, excl holidays
obs_wk <- enw_complete_dates(obs_wk, max_delay = d_max)
obs_wk[, holiday := FALSE]
obs_wk[, weekend := ifelse(wday(report_date) %in% c(1,7), TRUE, FALSE)]

  # include variable to track reporting dates (Wednesdays)
obs_wk[, report_possible := ifelse(wday(report_date) == 4, TRUE, FALSE)]

# Run models -------------------------------------------------------------
# Set up multithreading
ncores <- parallel::detectCores()
nchains <- 4
threads <- ncores/nchains
options(mc.cores = ncores)

# Model fitting options
fit <- enw_fit_opts(
  save_warmup = FALSE, output_loglik = TRUE, pp = TRUE,
  chains = nchains, threads_per_chain = threads, 
  iter_sampling = 1000, iter_warmup = 1000,
  show_messages = FALSE, refresh = 0, max_treedepth = 15, adapt_delta = .99
)

# Compile nowcasting model
multithread_model <- enw_model(threads = TRUE, verbose = FALSE)

time_start <- Sys.time()
print(paste("Model fitting started at", time_start))

# Iterate over dates in range
for (i in 1:length(date_list)){
  date_nowcast <- date_list[i] 
  cat(paste("===== Nowcast Date:", date_nowcast, "=====", "\n"))
  
  # Filter observations based on nowcast date
  obs_all_i <- filter_obs(obs_all, date_nowcast, days_included)
  obs_hol_i <- filter_obs(obs_hol, date_nowcast, days_included)
  obs_wk_i <- filter_obs(obs_wk, date_nowcast, days_included)
  
  # # Model 1: Reference fixed, report fixed
  # cat(paste("===== Model 1 =====", "\n"))
  # nowcast <- nowcast_model(obs_all_i, "fixed", d_max, fit, multithread_model)
  # 
  # # Model 2: Reference fixed, report weekend
  # cat(paste("===== Model 2 =====", "\n"))
  # wknd_nowcast <- nowcast_model(obs_all_i, "wknd", d_max, fit, multithread_model)

  # Model 3: Reference fixed, report day of week
  cat(paste("===== Model 3 =====", "\n"))
  dow_nowcast <- nowcast_model(obs_all_i, "dow", d_max, fit, multithread_model)
  
  # # Model 4: Reference fixed, report day of week + holidays
  # cat(paste("===== Model 4 =====", "\n"))
  # hol_nowcast <- nowcast_model(obs_hol_i, "dow", d_max, fit, multithread_model)
  # 
  # # Model 5: Reference fixed, report on reporting date
  # cat(paste("===== Model 5 =====", "\n"))
  # wkly_nowcast <- nowcast_model(obs_wk_i, "wkly", d_max, fit, multithread_model)
  
  # Summarise nowcasts
  nowcasts <- list(
    # "Fixed" = nowcast,
    # "Weekend" = wknd_nowcast,
    "Dayofweek" = dow_nowcast
    # "Holiday" = hol_nowcast,
    # "Weekly" = wkly_nowcast
  )
  
  summarised_nowcasts <- map(
    nowcasts, summary,
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
  ) 
  
  summarised_nowcasts <- rbindlist(summarised_nowcasts, idcol = "model", fill = TRUE, use.names = TRUE)
  summarised_nowcasts[, `:=`(nowcast_date = date_nowcast,
                             max_confirm = NULL,
                             cum_prop_reported = NULL,
                             prop_reported = NULL)]
  
  # if directory doesn't exist, create it
  if (!dir.exists(here("data", run_name))) { dir.create(here("data", run_name)) } 
  
  # Store nowcasts
  write.table(summarised_nowcasts,
              file = here("data", run_name, "nowcasts.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("data", run_name, "nowcasts.csv")))
  
  # Update latest data
  latest <- obs_all |>
    enw_filter_report_dates(latest_date = date_latest) |>
    enw_latest_data() |>
    enw_filter_reference_dates(latest_date = date_nowcast, include_days = d_max)
  
  # Store scores
  natscore <- enw_score_nowcast(
    summarised_nowcasts,
    latest[reference_date > (max(reference_date) - 7)],
    log = FALSE
  )
  
  logscore <- enw_score_nowcast(
    summarised_nowcasts,
    latest[reference_date > (max(reference_date) - 7)],
    log = TRUE
  )
  
  score <- rbind(natscore[, scale := "natural"], logscore[, scale := "log"]) 
  
  write.table(score,
              file = here("data", run_name, "scores.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("data", run_name, "scores.csv")))
  
  # Extract diagnostics
  diagnostics <- map(nowcasts, 
                     function(x){
                       x <- copy(x)
                       out <- x[, nowcast_date := max_date][,`samples`:`nowcast_date`]
                       return(out)
                     })
  diagnostics <- rbindlist(diagnostics, idcol = "model", use.names = TRUE)
  
  # Store diagnostics
  write.table(diagnostics,
              file = here("data", run_name, "diagnostics.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("data", run_name, "diagnostics.csv")))
  
  # print progress
  cat(paste0("Progress: ", round(100*i/length(date_list), 3), "%", "\n",
             "Time elapsed: ", round(as.numeric(difftime(Sys.time(), time_start, units = "min")),3), " mins", "\n")) 
}

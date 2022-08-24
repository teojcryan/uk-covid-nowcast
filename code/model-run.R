# Packages ----------------------------------------------------------------
require(epinowcast, quietly = TRUE)
require(data.table, quietly = TRUE)
require(purrr, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(scoringutils, quietly = TRUE))
suppressMessages(require(here, quietly = TRUE))
source(here::here("code", "model.R"))

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
run_name <- "run9"                       # Name of run

date_start <- as.Date("2022-02-01") + days_included
date_end <- as.Date("2022-07-01")
date_list <- seq(date_start, date_end, by = "days") # list of dates in range

# Prepare input data ------------------------------------------------------
# 1. Week day reporting, incl holidays
obs_all <- enw_complete_dates(obs_all, max_delay = d_max)

obs_all <- merge(obs_all, holidays, by.x = "report_date", by.y = "date") # apply holidays
obs_all[, weekend := ifelse(wday(report_date) %in% c(1,7), TRUE, FALSE)] # apply weekends
obs_all[, holiday_none := FALSE]                                         # apply no-holidays
obs_all[, dow_custom := ifelse(wday(report_date) == 7, 1, wday(report_date))] # variable for custom 6 day weeks

# 2. Weekly reporting, excl holidays
obs_wk <- enw_complete_dates(obs_wk, max_delay = d_max)
obs_wk[, report_possible := ifelse(wday(report_date) == 4, TRUE, FALSE)] # reporting only possible on Wednesdays
obs_wk[, holiday_none := FALSE]    

# Model set-up ------------------------------------------------------------
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
# multithread_model <- enw_model(model = "code/dow_exp.stan", threads = TRUE, verbose = FALSE)

# Variable list to extract posteriors
var_list <- c("refp", "rep", "phi", "leobs_init", "eobs_lsd", "obs_dow", "srdlh")

# Run models --------------------------------------------------------------
time_start <- Sys.time()
print(paste("Model fitting started at", time_start))

for (i in 1:length(date_list)){
  date_nowcast <- date_list[i]
  cat(paste("===== Nowcast Date:", date_nowcast, "=====", "\n"))

  # Filter observations based on nowcast date
  obs_all_i <- filter_obs(obs_all, date_nowcast, days_included)
  obs_wk_i <- filter_obs(obs_wk, date_nowcast, days_included)
  
  # Preprocess data
  pobs <- enw_preprocess_data(obs_all_i, max_delay = d_max, holidays = "holiday_none")   # excl holidays
  pobs_hol <- enw_preprocess_data(obs_all_i, max_delay = d_max, holidays = "holiday")    # incl holidays
  pobs_wk <- enw_preprocess_data(obs_wk_i, max_delay = d_max, holidays = "holiday_none") # weekly reporting
  
  # Define all reporting modules
  report_wknd <- enw_report(~ 1 + weekend, data = pobs)                  # fixed weekend effect
  report_dow <- enw_report(~ (1 | dow_custom), data = pobs)              # random day of week effect
  report_dow_hol <- enw_report(~ (1 | dow_custom), data = pobs_hol)      # random day of week effect with holidays -> Sundays
  report_wkly <- enw_report(~ 1 + report_possible, data = pobs_wk)       # fixed weekly report effect
  
  # Run nowcasts
  # Model 1: Reference fixed, report fixed
  # cat(paste("===== Model 1 =====", "\n"))
  # nowcast <- epinowcast(pobs, fit = fit, model = multithread_model)

  # Model 2: Reference fixed, report weekend
  # cat(paste("===== Model 2 =====", "\n"))
  # wknd_nowcast <- epinowcast(pobs, fit = fit, model = multithread_model, report = report_wknd)

  # # Model 3: Reference fixed, report day of week
  # cat(paste("===== Model 3 =====", "\n"))
  # dow_nowcast <- epinowcast(pobs, fit = fit, model = multithread_model, report = report_dow)
  
  # # Model 4: Reference fixed, report day of week + holidays
  # cat(paste("===== Model 4 =====", "\n"))
  # hol_nowcast <- epinowcast(pobs_hol, fit = fit, model = multithread_model, report = report_dow_hol)

  # # Model 5: Reference fixed, report on reporting date
  # cat(paste("===== Model 5 =====", "\n"))
  # wkly_nowcast <- epinowcast(pobs_wk, fit = fit, model = multithread_model, report = report_wkly)

  # Store results as list
  nowcasts <- list(
    # "Fixed" = nowcast,
    # "Weekend" = wknd_nowcast,
    "Dayofweek" = dow_nowcast
    # "Holiday" = hol_nowcast,
    # "Weekly" = wkly_nowcast
  )
  
  # if directory doesn't exist, create it
  if (!dir.exists(here("output", run_name))) { dir.create(here("output", run_name)) } 
  
  # Summarise daily nowcasts
  summarised_nowcasts <- map(
    nowcasts, summary,
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
  ) 
  
  summarised_nowcasts <- rbindlist(summarised_nowcasts, idcol = "model", fill = TRUE, use.names = TRUE)
  summarised_nowcasts[, `:=`(nowcast_date = date_nowcast)]
  
  # Store nowcasts
  write.table(summarised_nowcasts,
              file = here("output", run_name, "nowcasts.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("output", run_name, "nowcasts.csv")))
  
  # Summarise 7-day average nowcasts
  summarised_nowcasts_wk <- map(
    nowcasts, summarise_week,
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
  ) 
  
  summarised_nowcasts_wk <- rbindlist(summarised_nowcasts_wk, idcol = "model", fill = TRUE, use.names = TRUE)
  summarised_nowcasts_wk[, `:=`(nowcast_date = date_nowcast)]
  
  # Store nowcasts
  write.table(summarised_nowcasts_wk,
              file = here("output", run_name, "nowcasts_wk.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("output", run_name, "nowcasts_wk.csv")))
  
  # Extract diagnostics
  diagnostics <- map(nowcasts, 
                     function(nwc){
                       out <- copy(nwc)[, nowcast_date := max_date][,`samples`:`nowcast_date`]
                       return(out)
                     })
  diagnostics <- rbindlist(diagnostics, idcol = "model", use.names = TRUE)
  
  # Store diagnostics
  write.table(diagnostics,
              file = here("output", run_name, "diagnostics.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("output", run_name, "diagnostics.csv")))
  
  # Extract posteriors
  posteriors <- map(nowcasts,
      function(nwc){
        fit_summary <- data.table(nwc$fit[[1]]$summary())
        out <- fit_summary |>
          DT(variable %in% grep(paste(var_list, collapse = "|"), fit_summary$variable, value = TRUE)) |>
          DT(, `:=`(nowcast_date = date_nowcast))
        return(out)
      })
  posteriors <- rbindlist(posteriors, idcol = "model", use.names = TRUE)
  
  # Store diagnostics
  write.table(posteriors,
              file = here("output", run_name, "posteriors.csv"),
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = !file.exists(here("output", run_name, "posteriors.csv")))
  
  # print progress
  cat(paste0("Progress: ", round(100*i/length(date_list), 3), "%", "\n",
             "Time elapsed: ", round(as.numeric(difftime(Sys.time(), time_start, units = "hour")), 3), " hours", "\n")) 
}


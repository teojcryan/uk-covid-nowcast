library(here)
library(epinowcast)
library(data.table)
library(purrr)
library(ggplot2)
library(loo)
library(scoringutils)
library(knitr)
source(here("code", "model.R"))
source(here("code", "model-tools.R"))
source(here("code", "utils.R"))

# Set up parallel
ncores <- parallel::detectCores()
threads <- ncores / 2
options(mc.cores = ncores / 2)

# Data
dt <- readRDS(here::here("data", "cases", "national", "merged.rds"))[
  , c('specimen_date', 'cases', 'report_date')
]

names(dt) <- c("reference_date", "confirm", "report_date")

max_d <- 10
days_included <- 35

dt <- enw_complete_dates(dt, max_delay = max_d)

days <- seq(as.Date("2022-03-08"), as.Date("2022-06-30"), by = "days")

for (i in 1:length(days)){
  date_nowcast <- days[i]
  
  retro <- dt |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  
  # Plot of data by delay
  retro[, delay := as.numeric(report_date - reference_date)
  ][, ggplot(.SD, aes(x=reference_date, y=confirm, col=factor(delay))) + 
      geom_point() + 
      theme_bw() + 
      theme(legend.position = 'bottom')]
  
  # latest data
  latest <- dt |>
    enw_filter_report_dates(latest_date ="2022-07-16") |>
    enw_latest_data() |>
    enw_filter_reference_dates(latest_date = date_nowcast, include_days = max_d)
  
  pobs <- enw_preprocess_data(retro, max_delay = max_d)
  
  # Models
  nowcast <- my_epinowcast(retro, report = "fixed", reference = "fixed")
  dow_nowcast <- my_epinowcast(retro, report = "dow", reference = "fixed")
  wk_dow_nowcast <- my_epinowcast(retro, report = "dow", reference = "week")
  
  # #
  # plot(dow_nowcast, latest_obs = latest) 
  # 
  # plot(dow_nowcast, type = "posterior") +
  #   facet_wrap(~reference_date, ncol=7, scales = "free")
  
  # Nowcasts ------------
  nowcasts <- list(
    "Reference: Fixed, Report: Fixed" = nowcast,
    "Reference: Fixed, Report: Day of Week" = dow_nowcast,
    "Reference: Week, Report: Day of Week" = wk_dow_nowcast # 23% hit max treedepth
  )
  
  summarised_nowcasts <- map(
    nowcasts, summary,
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
  )
  
  summarised_nowcasts <- rbindlist(summarised_nowcasts, idcol = "model",
                                   use.names = TRUE)[, nowcast_date := date_nowcast]
  
  write.csv(summarised_nowcasts,
            here("data", "nowcasts", "daily", paste0(date_nowcast, ".csv")),
            row.names = F)
  
  # Scores -----------------
  score <- enw_score_nowcast(
    summarised_nowcasts,
    latest[reference_date > (max(reference_date) - 7)]
  )
  
  write.csv(score,
            here("data", "scores", paste0(date_nowcast, ".csv")),
            row.names = F)
  
  #score |>
  #  summarise_scores(by = "model")
}
# Sys.getenv("HOME")
# # Loading holidays -------
# install.packages('rjson')
# library(rjson)
# 
# uk_holidays <- fromJSON(file = "https://www.gov.uk/bank-holidays.json")
# eng_holidays <- uk_holidays$`england-and-wales`$events
# 
# holidays_dt <- rbindlist(eng_holidays)
# holidays_2022 <- holidays_dt[, date := as.Date(date)
# ][date >= "2022-01-01" & date <= "2022-12-31"]
# 
# nat_germany_hosp <- epinowcast::germany_covid19_hosp[location == "DE"]
# 
# retro_nat_germany <- nat_germany_hosp |>
#   epinowcast::enw_filter_report_dates(latest_date = "2021-09-01") |>
#   epinowcast::enw_filter_reference_dates(include_days = 40)
# 
# epinowcast::enw_preprocess_data(retro_nat_germany,
#                                 max_delay = 20,
#                                 by = "age_group",
#                                 holidays = c("2021-07-23"))
# 
# library(reprex)
# reprex()

library(epinowcast)
library(data.table)
library(ggplot2)

dt <- readRDS(here::here("data", "cases", "national", "merged.RDS"))[
  , c("specimen_date", "cases", "report_date")
]

dt <- dt[, cases := cumsum(cases), by = specimen_date]

names(dt) <- c("reference_date", "confirm", "report_date")

dt <- enw_complete_dates(dt)

eng_cases <- dt |>
  enw_filter_report_dates(latest_date = "2022-06-01")

retro_eng_cases <- eng_cases |>
  enw_filter_report_dates(remove_days = 30) |>
  enw_filter_reference_dates(include_days = 30)

latest_eng_cases <- eng_cases |>
  enw_latest_data() |>
  enw_filter_reference_dates(remove_days = 30, include_days = 10)

pobs <- enw_preprocess_data(retro_eng_cases, max_delay = 21)

reference_module <- enw_reference(~1, distribution = "lognormal", data = pobs)

report_module <- enw_report(~ (1 | day_of_week), data = pobs)

model <- enw_model(threads = TRUE)

options(mc.cores = 4)
nowcast <- epinowcast(pobs,
                      reference = reference_module,
                      report = report_module,
                      fit = enw_fit_opts(,
                                         save_warmup = FALSE, pp = TRUE,
                                         chains = 4, threads_per_chain = 2,
                                         iter_sampling = 1000, iter_warmup = 1000,
                                         show_messages = T, refresh = 0
                      ),
                      model = model
)

nowcast

nowcast |>
  summary(probs = c(0.05, 0.95)) |>
  head(n = 10)

plot(nowcast, latest_obs = latest_eng_cases)

plot(nowcast, type = "posterior") +
  facet_wrap(vars(reference_date), scale = "free")

# extract samples
samples <- summary(nowcast, type = "nowcast_samples")

# Take a 7 day rolling sum of both samples and observations
cols <- c("confirm", "sample")
samples[, (cols) := lapply(.SD, frollsum, n = 7),
        .SDcols = cols, by = ".draw"
][!is.na(sample)]

latest_eng_cases_7day <- copy(latest_eng_cases)[
  ,confirm := frollsum(confirm, n = 7)
][!is.na(confirm)]

# Summarise samples
sum_across_last_7_days <- enw_summarise_samples(samples)

# Plot samples
enw_plot_nowcast_quantiles(sum_across_last_7_days, latest_eng_cases_7day)

# Nowcast models
nowcast_model <- function(obs, report, max_delay, fit, model) {
  pobs <- enw_preprocess_data(
    obs,
    max_delay = max_delay,
    holidays = "holiday"
  )
  
  if (report == "fixed") {
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      model = multithread_model
    )
  } else if (report == "wknd") {
    #TODO enw_report(~ 1 + weekend, data = pobs)
    report_wknd <- enw_report(~ (1 | weekend), data = pobs)
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      model = multithread_model,
      report = report_wknd
    )
  } else if (report == "dow"){
    #TODO 7 days to 6 days?
    report_dow <- enw_report(~ (1 | day_of_week), data = pobs)
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      report = report_dow,
      model = multithread_model
    )
  } else if (report == "wkly") {
    #TODO enw_report(~ 1 + report_possible, data = pobs)
    report_wkly <- enw_report(~ (1 | report_possible), data = pobs)
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      report = report_wkly,
      model = multithread_model
    )
  }
  return(nowcast)
}

filter_obs <- function(obs, nowcast_date, days_included){
  # filter data based on estimation (nowcast) date
  obs_i <- obs |>
    enw_filter_report_dates(latest_date = date_nowcast) |>
    enw_filter_reference_dates(include_days = days_included)
  return(obs_i)
}

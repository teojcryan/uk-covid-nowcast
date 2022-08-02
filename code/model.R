#' Nowcast models
#' 
my_epinowcast <- function(obs, reference, report, max_delay = 10, ...) {
  # Set up parallel
  ncores <- parallel::detectCores()
  threads <- ncores / 2
  options(mc.cores = ncores / 2)
  
  pobs <- enw_preprocess_data(
    obs,
    #rep_holidays = "holiday",
    max_delay = max_delay
  )
  
  multithread_model <- enw_model(threads = TRUE)
  report_module_dow <- enw_report(~ (1 | day_of_week), data = pobs)
  reference_module_week <- enw_reference(~ 1 + rw(week), data = pobs)
  
  fit <- enw_fit_opts(
    save_warmup = FALSE, output_loglik = TRUE, pp = TRUE,
    chains = 2, threads_per_chain = threads, 
    iter_sampling = 1000, iter_warmup = 1000,
    show_messages = TRUE, refresh = 0, max_treedepth = 12,
    ...
  )
  
  if (reference == "fixed" & report == "fixed") {
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      model = multithread_model
    )
  } else if (reference == "fixed" & report == "dow"){
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      report = report_module_dow,
      model = multithread_model
    )
  } else if (reference == "week" & report == "dow"){
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      report = report_module_dow,
      reference = reference_module_week,
      model = multithread_model
    )
  }
  
  return(nowcast)
}
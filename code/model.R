#' Nowcast models
nowcast_model <- function(obs, reference, report, max_delay, priors, ...) {
  
  # Set up parallel
  ncores <- parallel::detectCores(logical = FALSE)
  threads <- ncores / 2
  options(mc.cores = ncores / 2)
  
  pobs <- enw_preprocess_data(
    obs,
    max_delay = max_delay,
    holidays = "holiday"
  )
  
  multithread_model <- enw_model(threads = TRUE)
  report_module_dow <- enw_report(~ (1 | day_of_week), data = pobs)
  
  fit <- enw_fit_opts(
    save_warmup = FALSE, output_loglik = TRUE, pp = TRUE,
    chains = ncores / 2, threads_per_chain = ncores / 2, 
    iter_sampling = 1000, iter_warmup = 1000,
    show_messages = FALSE, refresh = 0, max_treedepth = 12, adapt_delta = .975,
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
      model = multithread_model,
      priors = priors
    )
  }
  
  return(nowcast)
}
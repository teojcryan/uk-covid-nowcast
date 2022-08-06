# Nowcast models
nowcast_model <- function(obs, reference, report, max_delay, priors = NULL, ...) {
  
  # Set up parallel
  #ncores <- parallel::detectCores(logical = FALSE)
  #threads <- 1
  options(mc.cores = 16)
  
  pobs <- enw_preprocess_data(
    obs,
    max_delay = max_delay,
    holidays = "holiday"
  )
  
  multithread_model <- enw_model(threads = TRUE, verbose = FALSE)
  report_module_dow <- enw_report(~ (1 | day_of_week), data = pobs)
  
  fit <- enw_fit_opts(
    save_warmup = FALSE, output_loglik = TRUE, pp = TRUE,
    chains = 4, threads_per_chain = 4, 
    iter_sampling = 1000, iter_warmup = 500,
    show_messages = FALSE, refresh = 0, max_treedepth = 15, adapt_delta = .99,
    ...
  )
  
  if (reference == "fixed" & report == "fixed") {
    nowcast <- epinowcast(
      pobs,
      fit = fit,
      model = multithread_model
    )
  } else if (reference == "fixed" & report == "dow"){
    if (!is.null(priors)){
      nowcast <- epinowcast(
        pobs,
        fit = fit,
        report = report_module_dow,
        model = multithread_model,
        priors = priors
      )
    } else {
      nowcast <- epinowcast(
        pobs,
        fit = fit,
        report = report_module_dow,
        model = multithread_model
      )
    }
  }
  
  return(nowcast)
}
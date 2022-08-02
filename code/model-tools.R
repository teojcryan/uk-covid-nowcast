# model-tools

compile_model <- function(...) {
  model <- epinowcast::enw_model(
    thread = TRUE, dir = here::here("data"),
    ...
  )
  return(model$stan_file())
}

load_model <- function() {
  model <- epinowcast::enw_model(
    dir = here::here("data"), thread = TRUE
  )
  return(model)
}

prior_epinowcast <- function(obs, priors, max_delay = 40, scale = 5, ...) {
  pobs <- enw_preprocess_data(obs,
                              max_delay = max_delay,
                              rep_holidays = "holiday"
  )
  
  model <- load_model()
  
  nowcast <- epinowcast(
    pobs,
    model = model,
    ...
  )
  
  priors <- enw_posterior_as_prior(
    nowcast,
    priors = priors,
    variables = c("logmean_int", "logsd_int", "sqrt_phi"),
    scale = scale
  )
  
  return(priors)
}

summarise_nowcast <- function(nowcast, model,
                              probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)) {
  samples <- summary(nowcast, type = "nowcast_samples")
  
  samples[sample < confirm, sample := confirm]
  samples[is.na(sample), sample := confirm]
  
  daily <- enw_summarise_samples(samples, probs)
  
  cols <- c("confirm", "sample")
  samples[, (cols) := lapply(.SD, data.table::frollsum, n = 7),
          .SDcols = cols, by = c(".draw")
  ]
  samples <- samples[!is.na(sample)]
  
  # Summarise 7 day nowcast
  seven_day <- enw_summarise_samples(samples, probs = probs)
  
  out <- data.table::data.table(
    model = model,
    nowcast_date = max(daily$reference_date),
    daily = list(daily),
    seven_day = list(seven_day)
  )
  out <- cbind(
    out,
    nowcast[
      ,
      .(
        max_rhat, divergent_transitions, per_divergent_transitions,
        max_treedepth, no_at_max_treedepth, per_at_max_treedepth, run_time,
        failure
      )
    ]
  )
  return(out[])
}

default_nowcast_on_error <- function(nowcast, pobs, model,
                                     rhat_bound = 1.5, ...) {
  
  if (nowcast$max_rhat[[1]] >= rhat_bound) {
    failure <- TRUE
    message("Fitting failed - fitting fallback model")
  }else{
    failure <- FALSE
  }
  
  if (failure) {
    nowcast <- epinowcast(
      pobs,
      model = model,
      ...
    )
    nowcast[, failure := TRUE]
  }else{
    nowcast[, failure := FALSE]
  }
  
  return(nowcast[])
}

# nowcast <- function(obs, tar_loc, model,
#                     max_delay, priors, settings) {
#   cast <- do.call(
#     model, c(
#       list(
#         obs = obs[location == tar_loc],
#         max_delay = max_delay,
#         priors = priors
#       ),
#       settings
#     )
#   )
#   return(cast)
# }

score_nowcast <- function(nowcast, obs, by = "model") {
  scores <- epinowcast::enw_score_nowcast(
    nowcast, obs, log = FALSE,
    summarise_by = by
  )
  log_scores <- epinowcast::enw_score_nowcast(
    nowcast, obs, log = TRUE,
    summarise_by = by
  )
  scores <- rbind(scores[, scale := "natural"], log_scores[, scale := "log"])
  return(scores[])
}




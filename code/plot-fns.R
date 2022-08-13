# apply delay grouping variable to dt
apply_delay_grp <- function(dt, delay_max){
  # create binning for delays > delay_max
  delay_max_grp <- paste0(">",as.character(delay_max))
  dt_out <- copy(dt)[, delay_grp := factor(ifelse(delay > delay_max, delay_max_grp, delay),
                                           levels = c(1:delay_max, delay_max_grp))]
  return(dt_out[])
}

# Plot barchart of cases by delay
plot_case_delay <- function(dt, delay_max, prop = TRUE){
  # extract names of columns containing case data
  new_cases_cols <- names(dt)[str_detect(names(dt), "new")]
  cases_cols <- str_remove(new_cases_cols, "new_")
  
  dt <- apply_delay_grp(dt, delay_max)
  
  # calculate mean delay by specimen date
  delay_mean <- copy(dt)[, prop := new_confirm/sum(new_confirm), by = reference_date
  ][, .(mean = sum(prop*delay)), by = reference_date] 
  
  # sum case data by bins
  dt_new <- copy(dt)[, lapply(.SD, sum), 
                     keyby = .(reference_date, delay_grp),
                     .SDcols = new_cases_cols]
  
  # cases by delay
  p_cases_delay <- dt_new |>
    ggplot() + 
    aes(x=reference_date, y=new_confirm, fill=delay_grp) +
    geom_bar(position = position_stack(reverse = TRUE), stat = 'identity', width=1) + 
    scale_y_continuous("Cases", labels = scales::unit_format(unit = "k", scale = 1e-3, sep="")) + 
    scale_x_date("Specimen date", expand = c(0,0)) +
    scale_fill_discrete("Delay (days)") +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  if (prop == TRUE){
    p_cases_delay_prop <- dt_new %>%
      filter(new_confirm > 0) %>%
      ggplot(aes(x=reference_date)) +
      geom_bar(aes(y=new_confirm, fill=delay_grp), 
               position = position_fill(reverse = TRUE), stat = 'identity', width=1) + 
      geom_line(data = delay_mean, aes(y = mean/max(mean))) +
      scale_y_continuous(name = "Proportion of cases",
                         sec.axis = sec_axis(trans = ~.*max(delay_mean$mean),
                                             name = 'Mean delay (days)')) + 
      scale_x_date("Specimen date", expand = c(0,0)) +
      scale_fill_discrete("Delay (days)") +
      guides(fill = guide_legend(nrow = 1)) +
      theme_bw() +
      theme(legend.position = 'bottom')
    
    p_cases_delay <- egg::ggarrange(p_cases_delay + 
                                      rremove("xlab") + 
                                      theme(legend.position = "none"),
                                    p_cases_delay_prop, ncol=1)
  }
  
  return(p_cases_delay)
}

# plot delay distribution
plot_delay_dist <- function(dt, delay_max, dist = "both", byday = FALSE){
  dt <- apply_delay_grp(dt, delay_max)
  
  if (byday == FALSE){
    p_dist <- copy(dt)[report_date <= "2022-07-01" & new_confirm >= 0, 
                       prop := new_confirm/sum(new_confirm), 
                       by = reference_date
    ][, ggplot(.SD, aes(x=delay_grp, y=prop)) + 
        geom_boxplot(width = .3, notch = TRUE,
                     outlier.size = 1, outlier.shape = 4) + 
        xlab("Delay d (days)") + ylab(expression("P(delay = d)")) +
        theme_bw()]
    
    p_cdist <- copy(dt)[report_date <= "2022-07-01", 
                        prop := confirm/max(confirm), 
                        by = reference_date
    ][, ggplot(.SD, aes(x=delay_grp, y=prop)) + 
        geom_boxplot(width = .3, notch = TRUE,
                     outlier.size = 1, outlier.shape = 4) + 
        xlab("Delay d (days)") + ylab(expression("P(delay "<=" d)")) + 
        theme_bw()]
    
  } else if (byday == TRUE){
    dt[, wday := factor(lubridate::wday(reference_date, week_start = 1))]
    dow_label <- as_labeller(c(`1` = "Monday",
                               `2` = "Tuesday",
                               `3` = "Wednesday",
                               `4` = "Thursday",
                               `5` = "Friday",
                               `6` = "Saturday",
                               `7` = "Sunday"))
    
    p_dist <- copy(dt)[report_date <= "2022-07-01" & new_confirm >= 0, 
                       prop := new_confirm/sum(new_confirm), 
                       by = c("reference_date", "wday")
                       ][, ggplot(.SD, aes(x=delay_grp, y=prop)) + 
                            geom_boxplot(width = .3, outlier.size = 1, outlier.shape = 4) + 
                            facet_wrap(~wday, nrow=2, label = dow_label) +
                            xlab("Delay d (days)") + ylab(expression("P(delay = d)")) +
                            theme_bw()]
    
    p_cdist <- copy(dt)[report_date <= "2022-07-01", 
                        prop := confirm/max(confirm), 
                        by = c("reference_date", "wday")
    ][, ggplot(.SD, aes(x=delay_grp, y=prop)) + 
        geom_boxplot(width = .3, outlier.size = 1, outlier.shape = 4) + 
        facet_wrap(~wday, nrow=2, label = dow_label) +
        xlab("Delay d (days)") + ylab(expression("P(delay "<=" d)")) + 
        theme_bw()]
  }
  
  if (dist == "both"){
    p_out <- ggarrange(p_dist, p_cdist, ncol=1)
  } else if (dist == "inc"){
    p_out <- p_dist
  } else if (dist == "cum"){
    p_out <- p_cdist
  }
  return(p_out)
}

# plot delay distribution by test type
plot_delay_test <- function(dt, delay_max, dist = "both", plot = "confint", scales = "fixed"){
  new_cases_cols <- names(dt)[str_detect(names(dt), "new_cases")]
  cases_cols <- str_remove(new_cases_cols, "new_")
  
  dt <- apply_delay_grp(dt, 7)
  
  dt_inc <- dt[reference_date <= "2022-07-01",
               melt(.SD,
                    id.vars = c('reference_date', 'delay_grp'),
                    measure.vars = new_cases_cols,
                    variable.name = "test_type")
  ][, prop := value/sum(value, na.rm=T),
    by = reference_date
  ]
  
  dt_cum <- dt[reference_date <= "2022-07-01",
             melt(.SD,
                  id.vars = c('reference_date', 'delay_grp'),
                  measure.vars = cases_cols,
                  variable.name = "test_type")
  ][, prop := value/max(value, na.rm=T),
    by = reference_date
  ]
  
  if (plot == "confint") {
    p_dist <- dt_inc[, .(mean = mean(prop, na.rm=T),
          u = quantile(prop, na.rm=T, prob = .975),
          l = quantile(prop, na.rm=T, prob = .025)),
      by = .(delay_grp, test_type)] |>
      ggplot(aes(x=delay_grp, y=mean, ymin=l, ymax=u)) + 
        geom_errorbar(width = .5, alpha = .5,position = position_dodge(width = 0.50)) + 
        geom_point(position = position_dodge(width = 0.50)) + 
        facet_wrap(~test_type, scales = scales,
                   label = as_labeller(c(`new_cases_lfd_pcr`="LFD-PCR", 
                                         `new_cases_lfd`="LFD Only",
                                         `new_cases_pcr`="PCR Only"))) +
        xlab("Delay (days)") + ylab(expression("P(delay = d)")) + 
        theme_bw()
    
    p_cdist <- dt_cum[, .(mean = mean(prop, na.rm=T),
                          u = quantile(prop, na.rm=T, prob = .975),
                          l = quantile(prop, na.rm=T, prob = .025)),
                      by = .(delay_grp, test_type)] |>
      ggplot(aes(x=delay_grp, y=mean, ymin=l, ymax=u)) + 
        geom_errorbar(width = .5, alpha = .5,position = position_dodge(width = 0.50)) + 
        geom_point(position = position_dodge(width = 0.50)) + 
        facet_wrap(~test_type, scales = scales,
                   label = as_labeller(c(`cases_lfd_pcr`="LFD-PCR", 
                                         `cases_lfd`="LFD Only",
                                         `cases_pcr`="PCR Only"))) +
        xlab("Delay (days)") + ylab(expression("P(delay" <="d)")) + 
        theme_bw()
    
  } else if (plot == "boxplot") {
    p_dist <- dt_inc |> 
      ggplot(aes(x=delay_grp, y=prop)) + 
      geom_boxplot(width = .3, notch = TRUE,
                   outlier.size = 1, outlier.shape = 4) +
      facet_wrap(~test_type, scales = scales,
                 label = as_labeller(c(`new_cases_lfd_pcr`="LFD-PCR", 
                                       `new_cases_lfd`="LFD Only",
                                       `new_cases_pcr`="PCR Only"))) +
      xlab("Delay (days)") + ylab(expression("P(delay = d)")) + 
      theme_bw()
    
    p_cdist <- dt_cum |>
      ggplot(aes(x=delay_grp, y=prop)) + 
      geom_boxplot(width = .3, notch = FALSE,
                   outlier.size = 1, outlier.shape = 4) + 
      facet_wrap(~test_type, scales = scales,
                 label = as_labeller(c(`cases_lfd_pcr`="LFD-PCR", 
                                       `cases_lfd`="LFD Only",
                                       `cases_pcr`="PCR Only"))) +
      xlab("Delay (days)") + ylab(expression("P(delay" <="d)")) + 
      theme_bw()
  }
  
  if (dist == "both"){
    p_out <- ggarrange(p_dist + xlab(""), p_cdist, ncol=1)
  } else if (dist == "inc"){
    p_out <- p_dist
  } else if (dist == "cum"){
    p_out <- p_cdist
  }
  return(p_out)
}

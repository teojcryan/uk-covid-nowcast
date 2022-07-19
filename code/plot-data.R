require(data.table, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(ggpubr, quietly = TRUE)
suppressMessages(require(here, quietly = TRUE))

# load pre-processed data
dt <- readRDS(here("data", "cases", "national", "merged.rds"))

# data wrangling --------------------------
# extract names of columns containing case data
cases_cols <- names(dt)[grep("cases", names(dt))]

#dt <- dt[specimen_date < "2022-07-01"]
delay_max <- 20 # max delay in days
delay_max_grp <- paste0(">",as.character(delay_max))

# create binning for delays > delay_max
dt[, delay_grp := factor(ifelse(delay > delay_max, delay_max_grp, delay),
                         levels = c(1:delay_max, delay_max_grp))]

# calculate mean delay by specimen date
delay_mean <- dt[, prop := cases/sum(cases), by = specimen_date
                 ][, .(mean = sum(prop*delay)), by = specimen_date] 

# sum case data by bins
dt <- dt[, lapply(.SD, sum, na.rm=T), 
         keyby = .(specimen_date, delay_grp),
         .SDcols = cases_cols]

# group by day of week
dt[, dow := factor(lubridate::wday(specimen_date, week_start = 1),
                   levels = 1:7)]
dow_label <- as_labeller(c(`1` = "Monday",
                           `2` = "Tuesday",
                           `3` = "Wednesday",
                           `4` = "Thursday",
                           `5` = "Friday",
                           `6` = "Saturday",
                           `7` = "Sunday"))

#
# cases by delay -----------------------
p_cases_delay <- dt |>
  ggplot() + 
  aes(x=specimen_date, y=cases, fill=delay_grp) +
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity', width=1) + 
  scale_y_continuous("Cases", labels = scales::unit_format(unit = "K", scale = 1e-3)) + 
  scale_x_date("Specimen date", expand = c(0,0)) +
  scale_fill_discrete("Delay (days)") +
  theme_bw() +
  theme(legend.position = 'bottom')

p_cases_delay_prop <- dt %>%
  filter(cases > 0) %>%
  ggplot(aes(x=specimen_date)) +
  geom_bar(aes(y=cases, fill=delay_grp), 
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

# ggsave(
#  here::here("output", "cases-delay-england.png"),
#  p_cases_delay,
#  dpi = 330, height = 8, width = 12
# )

#TODO identify distinct periods in time where delay distribution looks different

# delay distribution -----------------------------
p_delay_dist <- dt[, prop := cases/sum(cases),
   by=specimen_date
][, .(mean = mean(prop, na.rm=T),
      u = quantile(prop, na.rm=T, prob = .975),
      l = quantile(prop, na.rm=T, prob = .025)),
  by = delay_grp
][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u)) +
    geom_errorbar(width = .5) + 
    geom_point() +
    xlab("delay d (days)") + ylab(expression("P(delay = d)")) + 
    theme_bw()]

p_delay_dist_period <- dt[, year := factor(year(specimen_date))
   ][, ':='(prop = cases/sum(cases)),
     by = .(specimen_date, year)
     ][, .(mean = mean(prop, na.rm=T),
           u = quantile(prop, na.rm=T, prob = .975),
           l = quantile(prop, na.rm=T, prob = .025)),
       by = .(delay_grp, year)
       ][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u, col=year)) + 
           geom_errorbar(width = .5, position = position_dodge(width = 0.3)) +
           geom_point(position = position_dodge(width = 0.3)) + 
           xlab("delay d (days)") + ylab(expression("P(delay = d)")) + 
           scale_color_discrete("Year") +
           theme_bw() + 
           theme(legend.position = 'bottom')]

ggsave(
 here::here("output", "cases-delay-dist.png"),
 ggarrange(p_delay_dist, p_delay_dist_period, ncol=1),
 dpi = 330, height = 8, width = 6
)

# cumulative delay distribution -----------------------------
p_delay_cdist <- dt[, prop := cases/sum(cases), by=specimen_date
   ][, prop := cumsum(prop), by=specimen_date
     ][, .(mean = mean(prop, na.rm=T),
           u = quantile(prop, na.rm=T, prob = .975),
           l = quantile(prop, na.rm=T, prob = .025)),
       by = delay_grp
       ][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u)) +
           geom_errorbar(width = .5) + 
           geom_point() +
           xlab("delay d (days)") + ylab(expression("P(delay <= d)")) + 
           theme_bw()]

p_delay_cdist_period <- dt[, year := factor(year(specimen_date))
   ][, ':='(prop = cumsum(cases/sum(cases))),
     by = .(specimen_date, year)
     ][, .(mean = mean(prop, na.rm=T),
           u = quantile(prop, na.rm=T, prob = .975),
           l = quantile(prop, na.rm=T, prob = .025)),
       by = .(delay_grp, year)
       ][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u, col=year)) + 
            geom_errorbar(width = .5, position = position_dodge(width = 0.3)) +
            geom_point(position = position_dodge(width = 0.3)) + 
            xlab("delay d (days)") + ylab(expression("P(delay = d)")) + 
            scale_color_discrete("Year") +
            theme_bw() + 
            theme(legend.position = 'bottom')]
   
ggsave(
  here::here("output", "cases-delay-cum-dist.png"),
  ggarrange(p_delay_cdist, p_delay_cdist_period, ncol=1),
  dpi = 330, height = 8, width = 6
)

# exploratory plots for negative updates -------------------
  # scatterplot of delay by reduction in cases
dt[cases < 0
][, ggplot(.SD, aes(x=delay, y = cases)) + 
    geom_point() +
    ylab("Changes in cases reported") +
    xlab("Delay (days)") +
    theme_bw()]

  # plot of negative change in cases by date
dt[cases < 0
][, .(cases = sum(cases)), date
][, ggplot(.SD, aes(x=date, y=cases)) + 
    geom_bar(stat = 'identity') +
    theme_bw()]

  # plot comparing negative to positive updates
dt[, grp := ifelse(cases >= 0, 1, 0)
][, .(cases = sum(cases)), .(date, grp)
][, ggplot(.SD, aes(x=date, y=cases, fill=factor(grp)))+
    geom_bar(stat='identity') + 
    scale_fill_discrete("", labels = c("Negative", "Positive")) +
    scale_y_continuous("Cases", labels = scales::label_number(suffix = "K", scale = 1e-3)) +
    xlab("Specimen date") +
    theme_bw() + 
    theme(legend.position = 'bottom')]

# extra-long delays -----------------
dt_raw <- readRDS(here("data", "cases", "national", "merged.rds"))

days_since <- data.table(specimen_date = unique(dt$specimen_date),
                         delay = as.numeric(max(dt$specimen_date) + 7 -unique(dt$specimen_date)))

p_delay_freq <- ggplot(dt_raw[cases != 0], aes(x=specimen_date, y=delay)) +
  geom_bin2d(bins = 71) +
  geom_line(data = days_since, col ='red', lwd=.5) +
  scale_fill_continuous("Count", type = "viridis", direction=-1) +
  theme_bw() + 
  xlab("Specimen Date") +
  ylab("Delay (days)") +
  theme(legend.position='bottom')

p_update_freq <- dt_raw[cases != 0
       ][, .N, by = report_date
         ][, ggplot(.SD, aes(x=report_date, y=N)) + 
             geom_bar(stat = 'identity') +
             geom_vline(xintercept = as.Date("2021-12-03"), col='red', lty=2) +
             geom_vline(xintercept = as.Date("2022-01-31"), col='red', lty=2) +
             ylab("Number of updates") + xlab("Report date") + 
             theme_bw()]

ggarrange(p_delay_freq, p_update_freq)

# delay by day of week -------------
dt[, lapply(.SD, sum, na.rm=T),
   keyby = .(specimen_date, delay_grp, dow),
   .SDcols = cases_cols
][, cases_prop := cumsum(cases/sum(cases)), 
  by = .(specimen_date)
][, .(mean = mean(cases_prop, na.rm=T),
      u = quantile(cases_prop, na.rm=T, prob = .975),
      l = quantile(cases_prop, na.rm=T, prob = .025)),
  by = .(dow, delay_grp)
][delay_grp != ">20"
  ][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u)) + 
    geom_point(lwd=.8) +
    geom_errorbar(width = 0.5, alpha=.5) +
    xlab("Delay (days)") + ylab(expression("P(delay = d)")) + 
    scale_x_discrete(breaks = c(1:6, seq(0,20,7), "20")) +
    scale_y_continuous(trans='logit') + 
    facet_wrap(~dow, label=dow_label, nrow = 2) +
    theme_bw()]

dt[, lapply(.SD, sum, na.rm=T),
   keyby = .(specimen_date, delay_grp),
   .SDcols = cases_cols
][, cases_prop := cumsum(cases/sum(cases)), 
  by = .(specimen_date)
][delay_grp %in% c(7,20)
  ][, dcast(.SD, specimen_date ~ delay_grp)
    ][, diff := `20`-`7`
      ][, diff] %>% mean(na.rm=T) # quantile(na.rm=T, probs = c(0.025, .5, 0.975))

# proportion of cases by delay and test type ------------
dt[, lapply(.SD, sum, na.rm=T),
   keyby = .(specimen_date, delay_grp),
   .SDcols = cases_cols
][, melt(.SD, 
         id.vars = c('specimen_date', 'delay_grp'),
         measure.vars = cases_cols[2:4],
         variable.name = "cases")
][, prop := value/sum(value, na.rm=T),
  by = specimen_date
][, .(mean = mean(prop, na.rm=T),
      u = quantile(prop, na.rm=T, prob = .975),
      l = quantile(prop, na.rm=T, prob = .025)),
  by = .(delay_grp, cases)
][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u)) +
    geom_errorbar(width = .5, alpha = .5,position = position_dodge(width = 0.50)) + 
    geom_point(position = position_dodge(width = 0.50)) + 
    facet_wrap(~cases,
               label = as_labeller(c(`cases_lfd_pcr`="LFD-PCR", 
                                     `cases_lfd`="LFD Only",
                                     `cases_pcr`="PCR Only"))) + 
    xlab("Delay (days)") + ylab(expression("P(delay = d)")) + 
    theme_bw() + 
    theme(legend.position = 'bottom')]

# cases by test type and day of week -----------
dt[, lapply(.SD, sum, na.rm=T),
   keyby = .(specimen_date, delay_grp, dow),
   .SDcols = cases_cols
][, melt(.SD, 
         id.vars = c('specimen_date', 'delay_grp', 'dow'),
         measure.vars = cases_cols[2:4],
         variable.name = "cases")
][, prop := value/sum(value, na.rm=T),
  by = specimen_date
][, .(mean = mean(prop, na.rm=T),
      u = quantile(prop, na.rm=T, prob = .975),
      l = quantile(prop, na.rm=T, prob = .025)),
  by = .(dow, delay_grp, cases)
][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u, col=cases)) +
    geom_errorbar(position = position_dodge(width = 0.90)) + 
    geom_point(position = position_dodge(width = 0.90)) + 
    xlab("Delay (days)") + ylab(expression("P(delay = d)")) + 
    facet_wrap(~dow, label=dow_label, nrow = 2, 
               scales = 'free_y') +
    scale_color_discrete("Cases", labels=c("LFD-PCR", "LFD Only", "PCR Only")) +
    theme_bw() + 
    theme(legend.position = 'bottom')]

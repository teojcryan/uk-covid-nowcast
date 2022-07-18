require(data.table, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
#require(egg, quietly = TRUE)
require(ggpubr, quietly = TRUE)
#require(stringr, quietly = TRUE)
#require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))

# load pre-processed data
dt <- readRDS(here("data", "cases", "national", "merged.rds"))

# data wranging --------------------------
# extract column names for case data
cases_cols <- names(dt)[grep("cases", names(dt))]

#dt <- dt[specimen_date < "2022-07-01"]
delay_max <- 7 # max delay in days
delay_max_grp <- paste0(">",as.character(delay_max))

# create binning for delays > delay_max
dt[, delay_grp := factor(ifelse(delay > delay_max, 
                                delay_max_grp, 
                                delay),
                         levels = c(1:delay_max, 
                                    delay_max_grp))]

# calculate mean delay
delay_mean <- dt[, prop := cases/sum(cases)
                 , by = specimen_date
][, .(mean = sum(prop*delay)),
  by = specimen_date] 

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

# plot of cases by delay -----------------------
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
  #filter(delay <= delay_max) %>%
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

# plot of delay distribution -----------------------------
dt_delay_dist <- dt %>%
  group_by(specimen_date) %>%
  mutate(cases_prop = cases/sum(cases),
         cases_cum_prop = cumsum(cases_prop)) %>%
  group_by(delay_grp) %>%
  summarise(mean = mean(cases_prop),
            l = quantile(cases_prop, prob=0.025),
            u = quantile(cases_prop, prob=0.975)) 

p_delay_dist <- dt_delay_dist %>%
  ggplot() + 
  aes(x = as.numeric(delay_grp), y = mean, ymin = l, ymax = u) + 
  geom_ribbon(col = NA, alpha = 0.3) + 
  geom_line() + 
  geom_point() +
  ylab(expression("P(delay = d)")) + xlab("time d (days)") +
  scale_x_continuous(breaks = c(1:delay_max, delay_max+1),
                     labels = levels(dt_delay_dist$delay_grp)) +
  theme_bw() +
  theme(legend.position = 'bottom')

dt_delay_dist_period <- dt %>%
  group_by(specimen_date) %>%
  mutate(cases_prop = cases/sum(cases),
         cases_cum_prop = cumsum(cases_prop),
         period = factor(case_when(
           specimen_date > as.Date("2022-01-01") ~ "2022", 
           specimen_date > as.Date("2021-01-01") ~ "2021", 
           TRUE ~ "2020"
         ))) %>%
  group_by(period, delay_grp) %>%
  summarise(mean = mean(cases_prop),
            l = quantile(cases_prop, prob=0.025),
            u = quantile(cases_prop, prob=0.975)) 

p_delay_dist_period <- dt_delay_dist_period %>%
  #filter(delay <= delay_max) %>%
  ggplot() + 
  aes(x = as.numeric(delay_grp), y = mean, ymin = l, ymax = u, col = period, fill = period) + 
  geom_ribbon(col = NA, alpha = 0.2) + 
  geom_line() + 
  geom_point() +
  ylab(expression("P(delay = d)")) + xlab("time d (days)") +
  scale_x_continuous(breaks = c(1:delay_max, delay_max+1),
                     labels = levels(dt_delay_dist$delay_grp)) +
  theme_bw() +
  theme(legend.position = 'bottom')

ggsave(
 here::here("output", "cases-delay-dist.png"),
 ggarrange(p_delay_dist, p_delay_dist_period, ncol=1),
 dpi = 330, height = 8, width = 6
)

# plot of cumulative delay distribution -----------------------------
dt_delay_dist <- dt %>%
  group_by(date) %>%
  mutate(cases_prop = cases/sum(cases),
         cases_cum_prop = cumsum(cases_prop)) %>%
  group_by(delay_grp) %>%
  summarise(mean = mean(cases_cum_prop),
            l = quantile(cases_cum_prop, prob=0.025),
            u = quantile(cases_cum_prop, prob=0.975)) 

p_delay_dist <- dt_delay_dist %>%
  ggplot() + 
  aes(x = as.numeric(delay_grp), y = mean, ymin = l, ymax = u) + 
  geom_ribbon(col = NA, alpha = 0.3) + 
  geom_line() + 
  geom_point() +
  ylab(expression("P(delay "<=" d)")) + xlab("time d (days)") +
  scale_x_continuous(breaks = c(1:delay_max, delay_max+1),
                     labels = levels(dt_delay_dist$delay_grp)) +
  theme_bw() +
  theme(legend.position = 'bottom')

dt_delay_dist_period <- dt %>%
  group_by(date) %>%
  mutate(cases_prop = cases/sum(cases),
         cases_cum_prop = cumsum(cases_prop),
         period = factor(case_when(
           date > as.Date("2022-01-01") ~ "2022", 
           date > as.Date("2021-01-01") ~ "2021", 
           TRUE ~ "2020"
         ))) %>%
  group_by(period, delay_grp) %>%
  summarise(mean = mean(cases_cum_prop),
            l = quantile(cases_cum_prop, prob=0.025),
            u = quantile(cases_cum_prop, prob=0.975)) 

p_delay_dist_period <- dt_delay_dist_period %>%
  ggplot() + 
  aes(x = as.numeric(delay_grp), y = mean, ymin = l, ymax = u, col = period, fill = period) + 
  geom_ribbon(col = NA, alpha = 0.3) + 
  geom_line() + 
  geom_point() +
  ylab(expression("P(delay "<=" d)")) + xlab("time d (days)") +
  scale_x_continuous(breaks = c(1:delay_max, delay_max+1),
                     labels = levels(dt_delay_dist$delay_grp)) +
  theme_bw() +
  theme(legend.position = 'bottom')
   
ggsave(
  here::here("output", "cases-delay-cum-dist.png"),
  ggarrange(p_delay_dist, p_delay_dist_period, ncol=1),
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

# plot of extra-long delays -----------------
days_since <- data.table(date = unique(dt$date),
                         delay = 1 + as.numeric(max(dt$date) - unique(dt$date)))

ggplot(dt, aes(x=date, y=delay)) +
  geom_bin2d(bins = 116) +
  geom_line(data = days_since, col ='red', lwd=1) +
  scale_fill_continuous("Count", type = "viridis", direction=-1) +
  theme_bw() + 
  xlab("Specimen Date") +
  ylab("Delay (days)") +
  theme(legend.position='bottom')


# delay by day of week -------------
dt[, lapply(.SD, sum, na.rm=T),
   keyby = .(specimen_date, delay_grp, dow),
   .SDcols = cases_cols
][, cases_prop := cases/sum(cases), 
  by = .(specimen_date, dow)
][, .(mean = mean(cases_prop, na.rm=T),
      u = quantile(cases_prop, na.rm=T, prob = .975),
      l = quantile(cases_prop, na.rm=T, prob = .025)),
  by = .(dow, delay_grp)
][, ggplot(.SD, aes(x=delay_grp, y=mean, ymin=l, ymax=u)) + 
    geom_point() +
    geom_errorbar(width = 0.3) +
    xlab("Delay (days)") + ylab(expression("P(delay = d)")) + 
    facet_wrap(~dow, label=dow_label, nrow = 2) +
    theme_bw()]

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

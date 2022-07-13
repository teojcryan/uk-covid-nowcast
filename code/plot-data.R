require(data.table, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(egg, quietly = TRUE)
require(ggpubr, quietly = TRUE)
require(stringr, quietly = TRUE)
require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))

# LOAD DATA --------------------------
  # load data as list of data frames
filenames <- list.files(path = here("data", "cases_specimen", "national"), 
                        pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
  # set names as dates
names(ldf) <- str_extract(str_extract(filenames, "([^/]+$)"), ".*(?=\\.)")

# calculate delay as time between reported and actual onset
date_start <- as.Date(min(names(ldf)), format="%Y_%m_%d") # earliest date of availability for archived data
dt <- data.table::rbindlist(ldf, idcol=T) # combine list of data tables into single table
dt <- dt[date >= date_start]              # filter after start date

  # compute delay
dt[,':='(delay = as.numeric(as.Date(gsub("_", "-", .id)) - date),
         .id = NULL)][
           ,":="(cases = ifelse(cases == first(cases), cases, cases - lag(cases)))
           , by = c('region','date')
         ]
dt <- dt[delay > 0 & cases != 0] # remove dates with same-day reporting or no change
dt <- dt[region == "England"] # filter only England data

delay_max <- 7

dt[, delay_grp := factor(ifelse(delay > delay_max, 
                                paste0(">",as.character(delay_max)), 
                                as.character(delay)),
                         levels = c(as.character(1:delay_max), 
                                    paste0(">",as.character(delay_max))))]

# plot of cases by delay -----------------------
p_cases_delay <- dt %>%
  ggplot() + 
  aes(x=date, y=cases, fill=delay_grp) +
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity', width=1) + 
  scale_y_continuous("Cases", labels = scales::unit_format(unit = "K", scale = 1e-3)) + 
  scale_x_date("Specimen date", expand = c(0,0)) +
  scale_fill_discrete("Delay (days)") +
  theme_bw() +
  theme(legend.position = 'bottom')

# Calculate mean delay by specimen date
delay_mean <- dt %>%
  filter(cases > 0, delay <= 100) %>%
  group_by(date) %>%
  mutate(cases = cases/sum(cases)) %>%
  summarise(delay_mean = sum(cases*delay))

p_cases_delay_prop <- dt %>%
  filter(cases > 0) %>%
  #filter(delay <= delay_max) %>%
  ggplot(aes(x=date)) +
  geom_bar(aes(y=cases, fill=delay_grp), 
           position = position_fill(reverse = TRUE), stat = 'identity', width=1) + 
  geom_line(data = delay_mean, aes(y = delay_mean/max(delay_mean))) +
  scale_y_continuous(name = "Proportion of cases",
                     sec.axis = sec_axis(trans = ~.*max(delay_mean$delay_mean),
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
  group_by(date) %>%
  mutate(cases_prop = cases/sum(cases),
         cases_cum_prop = cumsum(cases_prop)) %>%
  group_by(delay_grp) %>%
  summarise(mean = mean(cases_prop),
            l = quantile(cases_prop, prob=0.025),
            u = quantile(cases_prop, prob=0.975)) 

p_delay_dist <- dt_delay_dist %>%
  #filter(delay <= delay_max) %>%
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
  group_by(date) %>%
  mutate(cases_prop = cases/sum(cases),
         cases_cum_prop = cumsum(cases_prop),
         period = factor(case_when(
           date > as.Date("2022-01-01") ~ "2022", 
           date > as.Date("2021-01-01") ~ "2021", 
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

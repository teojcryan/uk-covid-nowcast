require(data.table, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(ggpubr, quietly = TRUE)
require(stringr, quietly = TRUE)
suppressMessages(require(here, quietly = TRUE))
source(here("code", "plot-fns.R"))

# load pre-processed data
dt <- readRDS(here("data", "observations", "all.rds"))

delay_max <- 7
plot_case_delay(dt, delay_max)

plot_delay_dist(dt, delay_max)

plot_delay_dist(dt, delay_max, dist = "inc", byday = TRUE)

plot_delay_test(dt, delay_max, dist = "inc")

# exploratory plots for negative updates -------------------
  # scatterplot of delay by reduction in cases
dt[new_confirm < 0
][, ggplot(.SD, aes(x=delay, y = new_confirm)) + 
    geom_point() +
    ylab("Changes in cases reported") +
    xlab("Delay (days)") +
    theme_bw()]

  # plot of negative change in cases by date
dt[new_confirm < 0 
][, .(cases = sum(new_confirm)), report_date
][, ggplot(.SD, aes(x=report_date, y=cases)) + 
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

ggsave(
  here::here("output", "long-delay-dist.png"),
  ggarrange(p_delay_freq, p_update_freq, ncol=2),
  dpi = 330, height = 4, width = 8
)

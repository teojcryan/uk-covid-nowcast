require(data.table, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))

# load data as list of data frames
filenames <- list.files("data/cases_specimen/", pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
names(ldf) <- substr(filenames, 21, 30)

# calculate delay as time between reported and actual onset
dt <- data.table::rbindlist(ldf, idcol=T)
dt <- dt[date >= as.Date('2022-05-16')]
dt[,':='(delay = factor(as.Date(gsub("_", "-", .id)) - date - 1),
         .id = NULL)]

# plot
cases_delay <- dt %>%
  ggplot() + 
  aes(x=date, y=cases, fill=delay) +
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity') + 
  facet_wrap(~region, scales = 'free_y') +
  theme_bw() +
  theme(legend.position = 'bottom')

ggsave(
  here::here("output", "cases-delay.png"),
  cases_delay,
  dpi = 330, height = 8, width = 12
)

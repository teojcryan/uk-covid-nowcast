require(data.table, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(stringr, quietly = TRUE)
require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))

# load data as list of data frames
filenames <- list.files(path = here("data", "cases_specimen", "national"), 
                        pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
# set names as dates
names(ldf) <- str_extract(str_extract(filenames, "([^/]+$)"), ".*(?=\\.)")

# calculate delay as time between reported and actual onset
date_start <- as.Date(min(names(ldf)), format="%Y_%m_%d")
dt <- data.table::rbindlist(ldf, idcol=T)
dt <- dt[date >= date_start]
dt[,':='(delay = as.numeric(as.Date(gsub("_", "-", .id)) - date - 1),
         .id = NULL)][
           ,":="(cases = ifelse(cases == first(cases), cases, cases - lag(cases)))
           , by = c('region','date')
         ]
dt <- dt[delay > 0 & cases != 0]

# plot
cases_delay <- dt %>%
  filter(region == "England", delay <= 10) %>%
  ggplot() + 
  aes(x=date, y=cases, fill=factor(delay)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity') + 
  #facet_wrap(~region, scales = 'free_y') +
  scale_y_continuous("Cases", labels = scales::unit_format(unit = "K", scale = 1e-3)) + 
  xlab("Date reported") + 
  scale_fill_discrete("Delay (days)") +
  theme_bw() +
  theme(legend.position = 'bottom')

ggsave(
  here::here("output", "cases-delay-england.png"),
  cases_delay,
  dpi = 330, height = 8, width = 12
)

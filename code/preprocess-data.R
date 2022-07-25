require(data.table, quietly = TRUE)
require(stringr, quietly = TRUE)
require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))

# Pre-process data 

# load data as list of data frames ------------- 
filenames <- list.files(path = here("data", "cases", "national"), 
                        pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
names(ldf) <- str_extract(str_extract(filenames, "([^/]+$)"), ".*(?=\\.)") # set names as dates

dt <- data.table::rbindlist(ldf, idcol="report_date")     # combine list of data tables into single table
date_start <- as.Date(min(names(ldf)), format="%Y-%m-%d") # earliest date of availability for archived data
dt <- dt[date >= date_start]                              # filter after start date

# rename specific dates
dt[, `:=`(report_date = as.Date(report_date),
          specimen_date = date, 
          date = NULL)]

cases_cols <- c("cases", "cases_lfd_pcr", "cases_lfd", "cases_pcr") # columns with case data
setcolorder(dt, c("specimen_date", cases_cols, "report_date"))      # reorder columns of data table
dt <- dt[order(specimen_date, report_date)]                         # sort data by specimen date

# compute delay and incremental cases from cumulative
# dt[,':='(delay = as.numeric(report_date - specimen_date))
# ][
#   , by = c('specimen_date')
#   , (cases_cols) := lapply(.SD, function(x) {ifelse(x == first(x), x, x - shift(x, type='lag'))})
#   , .SDcols = cases_cols
# ]
# 
# dt <- dt[delay > 0] # remove dates with same-day reporting or no change
#dt <- dt[region == "England"] # filter only England data

saveRDS(dt, here::here("data", "cases", "national", "merged.rds"))

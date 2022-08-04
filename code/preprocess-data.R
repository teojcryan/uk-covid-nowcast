# Packages ----------------------------------------------------------------
require(data.table, quietly = TRUE)
require(stringr, quietly = TRUE)
require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))

# Load all raw data files -------------------------------------------------
filenames <- list.files(path = here("data", "observations", "reported"), 
                        pattern="*.rds", 
                        full.names=TRUE)
ldf <- lapply(filenames, readRDS) # load as list of data.tables
names(ldf) <- str_extract(str_extract(filenames, "([^/]+$)"), ".*(?=\\.)") # set names as dates

# merge list of data.tables into a single data.table
dt <- data.table::rbindlist(ldf, idcol="report_date")     # combine list of data tables into single table
date_start <- as.Date(min(names(ldf)), format="%Y-%m-%d") # earliest date of availability for archived data
dt <- dt[specimen_date >= date_start]                              # filter after start date

# rename specific dates
dt[, `:=`(report_date = as.Date(report_date),
          reference_date = specimen_date, 
          specimen_date = NULL,
          confirm = cases,
          cases = NULL)]

cases_cols <- c("confirm", "cases_lfd_pcr", "cases_lfd", "cases_pcr") # columns with case data
setcolorder(dt, c("reference_date","report_date", cases_cols))        # reorder columns of data table
dt <- dt[order(report_date, reference_date)]                          # sort data by specimen date

# If data by update is required, compute delay and incremental cases from cumulative
# dt[,':='(delay = as.numeric(report_date - specimen_date))
# ][
#   , by = c('specimen_date')
#   , (cases_cols) := lapply(.SD, function(x) {ifelse(x == first(x), x, x - shift(x, type='lag'))})
#   , .SDcols = cases_cols
# ]

saveRDS(dt, here::here("data", "observations", "all.rds"))
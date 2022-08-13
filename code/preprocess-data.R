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

cases_cols <- c("confirm", names(dt)[grep("cases", names(dt))]) # columns with case data
new_cases_cols <- paste0("new_", cases_cols)

# To calculate incidence for test-specific cases, set NA to 0
for (j in names(dt)) {
  set(dt,which(is.na(dt[[j]])),j,0)
}

# compute delay and incremental cases from cumulative
dt[,':='(delay = as.numeric(report_date - reference_date))
   ][, by = c("reference_date")
     , c(new_cases_cols) := lapply(.SD, function(x) {
    ifelse(x == first(x), x, x - shift(x, type='lag'))
    })
    , .SDcols = cases_cols]

setcolorder(dt, c("reference_date","report_date", 
                  "confirm", "new_confirm", "delay"))        # reorder columns of data table
dt <- dt[order(reference_date, report_date)]                          # sort data by specimen date
dt <- dt[delay > 0]

saveRDS(dt, here::here("data", "observations", "all.rds"))
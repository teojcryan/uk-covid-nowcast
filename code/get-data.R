# Packages ----------------------------------------------------------------
suppressMessages(require(here, quietly = TRUE))
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(readr, quietly = TRUE)
require(data.table, quietly = TRUE)
require(rjson, quietly = TRUE)

# New cases by specimen date ----------------------------------------------
# Specify start and end report date
date_start <- as.Date("2021-02-23")
date_end <- Sys.Date() - 1
n_days <- as.numeric(date_end - date_start)

# Iterate over date range and download data if available
for (n in 1:n_days){
  print(paste0(round(100*n/n_days, 3), "%")) # print progress
  date <- date_end - n # current date
  skip_to_next <- FALSE
  
  tryCatch({
    dt <- data.table::fread(paste0(
      "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&",
      "areaCode=E92000001&", # England data only
      "metric=newCasesBySpecimenDate&",
      "metric=newCasesLFDConfirmedPCRBySpecimenDate&",
      "metric=newCasesLFDOnlyBySpecimenDate&",
      "metric=newCasesPCROnlyBySpecimenDate&",
      "format=csv&",
      "release=", date
    ))
    
    dt_clean <- dt[
        , .(specimen_date = as.Date(date),
            cases = `newCasesBySpecimenDate`,
            cases_lfd_pcr = `newCasesLFDConfirmedPCRBySpecimenDate`,
            cases_lfd = `newCasesLFDOnlyBySpecimenDate`,
            cases_pcr = `newCasesPCROnlyBySpecimenDate`
      )
    ]
    
    saveRDS(dt_clean, 
            here::here("data", "observations", "reported", paste0(date, ".rds")))
  }, error = function(e) { skip_to_next <- TRUE })
  
  if (skip_to_next) { next }
}


# New cases by publish date -----------------------------------------------
dt_pub <- data.table::fread(paste0("https://api.coronavirus.data.gov.uk/v2/data?",
                         "areaType=nation&",
                         "areaCode=E92000001&",
                         "metric=newCasesByPublishDate&",
                         "format=csv")) 
dt_pub[, `:=`(cases = `newCasesByPublishDate`,
              `newCasesByPublishDate` = NULL,
              areaCode = NULL,
              areaName = NULL,
              areaType = NULL)]

saveRDS(dt_pub, 
        here::here("data", "observations", "publishdate.rds"))


# UK bank holidays --------------------------------------------------------
# download official list of holidays
uk_holidays <- fromJSON(file = "https://www.gov.uk/bank-holidays.json")
eng_holidays <- uk_holidays$`england-and-wales`$events # England and Wales only

holidays_dt <- rbindlist(eng_holidays)[, date := as.Date(date)]

holidays_list <- data.table(date = seq(as.Date("2019-12-01"), as.Date("2022-12-31"), by = "day"))[
  , holiday := ifelse(date %in% holidays_dt$date, TRUE, FALSE)]

saveRDS(holidays_list,
        here::here("data", "observations", "holidays.rds"))           

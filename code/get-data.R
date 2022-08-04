suppressMessages(require(here, quietly = TRUE))
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(readr, quietly = TRUE)
require(data.table, quietly = TRUE)

### COVID-19 dashboard archived data (20210224 - ) ------------------- 
# set start and end date
date_start <- as.Date("2021-02-23")
date_end <- Sys.Date() - 1
n_days <- as.numeric(date_end - date_start)

# load data 
for (n in 1:n_days){
  print(paste0(round(100*n/n_days, 3), "%"))
  date <- Sys.Date() - n
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
        , .(date = as.Date(date),
            cases = `newCasesBySpecimenDate`,
            cases_lfd_pcr = `newCasesLFDConfirmedPCRBySpecimenDate`,
            cases_lfd = `newCasesLFDOnlyBySpecimenDate`,
            cases_pcr = `newCasesPCROnlyBySpecimenDate`
      )
    ]
    
    saveRDS(dt_clean, 
            here::here("data", "cases", "national",paste0(date, ".rds")))
  }, error = function(e) { skip_to_next <- TRUE })
  
  if (skip_to_next) { next }
}


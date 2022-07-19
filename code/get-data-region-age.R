suppressMessages(require(here, quietly = TRUE))
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(readr, quietly = TRUE)
require(data.table, quietly = TRUE)

age_gps <- c(paste(formatC(seq(0, 85, 5), width=2, flag='0'),
                   formatC(seq(4, 89, 5), width=2, flag='0'),
                   sep='_'), "90+")

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
      "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&",
      "metric=newCasesBySpecimenDateAgeDemographics&",
      "format=csv&",
      "release=", date
    ))
    
    dt_clean <- dt[, !c("areaCode", "areaType", "rollingSum", "rollingRate")
      ][age %in% age_gps]
    
    saveRDS(dt_clean, 
            file = here::here("data", "cases", "region_age", paste0(date, ".rds")))
  }, error = function(e) { skip_to_next <- TRUE })
  
  if (skip_to_next) { next }
}

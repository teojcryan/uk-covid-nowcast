suppressMessages(require(here, quietly = TRUE))
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(readr, quietly = TRUE)
require(data.table, quietly = TRUE)

age_gp <- data.table(age_5y = c(paste(formatC(seq(0, 85, 5), width=2, flag='0'),
                                      formatC(seq(4, 89, 5), width=2, flag='0'),
                                      sep='_'), "90+"),
                     age_gp = c("00-04", rep("05-14", 2), rep("15-34", 4),
                                rep("35-59", 5), rep("60-79", 4), rep("80+", 3)))

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
    
    all_age <- dt[age %in% age_gp$age_5y, .(cases = sum(cases)), by = .(date, areaName)
                  ][, age_gp := "00+"]
    setcolorder(all_age, c("date", "areaName", "age_gp", "cases"))
    
    by_age <- dt[, .(cases = sum(cases)), by = .(date, areaName, age)
                 ][age_gp, on = .(age = age_5y)
                   ][, .(cases = sum(cases)), by = .(date, areaName, age_gp)]
    
    saveRDS(rbind(all_age, by_age), 
            file = here::here("data", "cases", "region_age", paste0(date, ".rds")))
  }, error = function(e) { skip_to_next <- TRUE })
  
  if (skip_to_next) { next }
}



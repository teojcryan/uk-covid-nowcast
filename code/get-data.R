require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))
require(readr, quietly = TRUE)

# nhse to LTLA lookup table
#ltla_nhser <- readRDS(here::here("data", "ltla_nhser.rds"))

# set start and end date
date_start <- as.Date("2020-01-30")
date_end <- Sys.Date() - 1
n_days <- as.numeric(date_end - date_start)

# load data 
for (n in 1:n_days){
  print(paste0(round(100*n/n_days, 3), "%"))
  date <- Sys.Date() - n
  skip_to_next <- FALSE
  
  tryCatch({
    # cases_specimen_local <- read_csv(paste0(
    #   "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&",
    #   "metric=newCasesBySpecimenDate&",
    #   "format=csv&",
    #   "release=", date
    # ))
    
    cases_specimen_national <- read_csv(paste0(
      "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&",
      "metric=newCasesBySpecimenDate&",
      "format=csv&",
      "release=", date
    ))
    
    # cases_specimen <- 
    #   cases_specimen_local %>%
    #   rename(ltla_name = areaName) %>%
    #   inner_join(ltla_nhser, by = "ltla_name") %>%
    #   group_by(date, nhse_region) %>%
    #   summarise(across(where(is.numeric), sum)) %>%
    #   ungroup() %>%
    #   rename(areaName = nhse_region) %>%
    #   bind_rows(select(cases_specimen_national, -areaCode, -areaType)) %>%
    #   select(date, region = areaName, cases = newCasesBySpecimenDate) %>%
    #   filter(!is.na(cases))
    
    cases_specimen <- 
      cases_specimen_national %>%
      #rename(ltla_name = areaName) %>%
      #inner_join(ltla_nhser, by = "ltla_name") %>%
      #group_by(date, nhse_region) %>%
      #summarise(across(where(is.numeric), sum)) %>%
      #ungroup() %>%
      #rename(areaName = nhse_region) %>%
      #bind_rows(select(cases_specimen_national, -areaCode, -areaType)) %>%
      select(date, region = areaName, cases = newCasesBySpecimenDate) %>%
      filter(!is.na(cases))
    
    saveRDS(cases_specimen, 
            here::here(paste0("data/cases_specimen/national/",
                              gsub("-", "_", date),
                              ".rds")))
    }, error = function(e) { skip_to_next <- TRUE })
    
    if (skip_to_next) { next }
}


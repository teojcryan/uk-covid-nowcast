require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(require(here, quietly = TRUE))
require(readr, quietly = TRUE)

# nhse to LTLA lookup table
ltla_nhser <- readRDS(here::here("data", "ltla_nhser.rds"))

n_delay <- 15 # days before current date

# load data 
for (n in 1:n_delay){
  
  date = Sys.Date() - n
  
  if (!(format(date, "%A") %in% c("Saturday", "Sunday"))){
    cases_specimen_local <- read_csv(paste0(
      "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&",
      "metric=newCasesBySpecimenDate&",
      "format=csv&",
      "release=", date
    ))
    
    cases_specimen_national <- read_csv(paste0(
      "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&",
      "metric=newCasesBySpecimenDate&",
      "format=csv&",
      "release=", date
    ))
    
    cases_specimen <- 
      cases_specimen_local %>%
      rename(ltla_name = areaName) %>%
      inner_join(ltla_nhser, by = "ltla_name") %>%
      group_by(date, nhse_region) %>%
      summarise(across(where(is.numeric), sum)) %>%
      ungroup() %>%
      rename(areaName = nhse_region) %>%
      bind_rows(select(cases_specimen_national, -areaCode, -areaType)) %>%
      select(date, region = areaName, cases = newCasesBySpecimenDate) %>%
      filter(!is.na(cases))
    
    saveRDS(cases_specimen, 
            here::here(paste0("data/cases_specimen_",
                              gsub("-", "_", date),
                              ".rds")))
  }
}


library(lubridate)
library(tidyverse)
library(zoltr)
library(covidHubUtils)

# script to build a data frame of forecasts 
first_sat_end_date <- as.Date("2020-10-17")

most_recent_sat <- lubridate::floor_date(Sys.Date()-7, unit = "week") - 1  
all_sat_end_dates <- seq(first_sat_end_date,most_recent_sat,by = "week")
most_recent_monday <- lubridate::floor_date(Sys.Date()-7, unit = "week") + 1 
# get all mondays for each saturday end dates since first saturday end date for all targets
for(i in 1:4){
  assign(paste0("latest_pos_forecast_date_",i,"wk"),
         c(seq((first_sat_end_date-5)-(7*(i-1)),
               most_recent_monday-(7*(i-1)),by = "week"))
  )
}


# set targets for analysis
target_list <- paste0(1:4," wk ahead inc death")

# get locations based on the current criteria of death reaching 20k by march
death_truth <- load_truth("JHU", 
                     "cum death", 
                     truth_end_date = as.Date("2021-03-08"),
                     temporal_resolution="weekly",
                     data_location = "remote_hub_repo") %>%
  dplyr::filter(target_end_date=="2021-02-27",
                geo_type=="state",
                location != "US") %>%
  dplyr::arrange(desc(value))
# get top 5 and bottom 5
dlocs <- death_truth$location[1:5]

#---------------------------------------------------------------------------------------------#

# pull data
for(i in 1:4){
  assign(paste0("quantile_frame",i),
         map_dfr(get(paste0("latest_pos_forecast_date_",i,"wk")),
                 function(fdates) {
                   covidHubUtils::load_latest_forecasts(last_forecast_date = fdates,
                                                        forecast_date_window_size=6,
                                                        locations = dlocs,
                                                        types = "quantile",
                                                        targets = target_list[i],
                                                        source = "zoltar")})
  )
}

### ideally instead of running the above in a loop, I want to  be able to
### specify `targets=paste0(1:4," wk ahead inc death")`. Currently that 
### would take hours on my 2018 macbook pro and it would stop running 
### in the middle, so I have to break it into chunks 
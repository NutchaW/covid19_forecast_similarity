library(lubridate)
library(tidyverse)
library(zoltr)
library(covidHubUtils)

# script to build a dataframe of model 
# first sat end date for 1 wk ahead
first_end_date <- as.Date("2020-12-17") # change this to January some time after new year

most_recent_end_date <- as.Date("2021-06-10") # pick a date
  
# use lubridate- or maybe seq () to get a range of dates between the first end date and the most recent one
date_range <- seq.Date(from = first_end_date, to = most_recent_end_date, by = "week")
date_range <- as.Date(date_range)

# set targets for analysis
target_list <- paste0(1:28," day ahead inc hosp")


# get locations
hosp_truth <- load_truth("HealthData", #note this is the only source available
                         "inc hosp", 
                         temporal_resolution="weekly",
                         data_location = "remote_hub_repo") %>%
  dplyr::filter(target_end_date >= "2020-12-17",
                target_end_date <= "2021-06-10",
                geo_type=="state",
                location != "US") %>%
  dplyr::group_by(location) %>%
  dplyr::mutate(cum_hosp=sum(value)) %>%
  # set the date
  dplyr::select(-c("target_end_date","value"))%>%
  ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(cum_hosp))

hlocs_high <- hosp_truth$location[1:5]


# run to build
hosp_forecasts_high <- map_dfr(date_range,
                 function(x) {
                   covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                        forecast_date_window_size=6,
                                                        # pick one
                                                        locations = hlocs_high,
                                                        types = "quantile",
                                                        targets = target_list,
                                                        source = "zoltar")
                   })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_forecasts_high <- hosp_forecasts_high %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date)) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_high,file = "./data/quantile_frame_hosp_top.csv")


## --------- Low -------------------------------------------------##
hosp_truth_low <- hosp_truth %>%
  dplyr::arrange(cum_hosp) %>%
  dplyr::filter(as.numeric(location)<60)

hlocs_low <- hosp_truth_low$location[1:5]

# run to build
hosp_forecasts_low <- map_dfr(date_range,
                               function(x) {
                                 covidHubUtils::load_latest_forecasts(
                                   last_forecast_date = x,
                                   forecast_date_window_size=6,
                                   # pick one
                                   locations = hlocs_low,
                                   types = "quantile",
                                   targets = target_list,
                                   source = "zoltar")
                               })


#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_forecasts_low <- hosp_forecasts_low %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date)) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))
                  

# write large files
write_csv(hosp_forecasts_low,file = "./data/quantile_frame_hosp_bottom.csv")



###### Point Forecasts ---------------------------------------------------- ##########################
hosp_pt_high <- map_dfr(date_range,
                        function(x) {
                          covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                               forecast_date_window_size=6,
                                                               # pick one
                                                               locations = hlocs_high,
                                                               types = "point",
                                                               targets = target_list,
                                                               source = "zoltar")
                        })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_pt_high <- hosp_pt_high %>% 
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  )) 

# write large files
write_csv(hosp_pt_high,file = "./data/point_frame_hosp_top.csv")

##
hosp_pt_low <- map_dfr(date_range,
                        function(x) {
                          covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                               forecast_date_window_size=6,
                                                               # pick one
                                                               locations = hlocs_low,
                                                               types = "point",
                                                               targets = target_list,
                                                               source = "zoltar")
                        })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_pt_low <- hosp_pt_low %>% 
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  )) 

# write large files
write_csv(hosp_pt_low,file = "./data/point_frame_hosp_bottom.csv")

library(lubridate)
library(tidyverse)
library(zoltr)
library(covidHubUtils)

# script to build a dataframe of model 

# set targets for analysis
target_list <- paste0(1:28," day ahead inc hosp")

## Thursdays only ---------------------------------------------------- ##########################
first_end_date_thurs <- as.Date("2021-01-28") 
most_recent_end_date_thurs <- as.Date("2021-06-10")
  
date_range_thurs <- seq.Date(from = first_end_date_thurs, 
                             to = most_recent_end_date_thurs, by = "week")

# get locations
hosp_truth_thurs <- load_truth("HealthData", 
                         "inc hosp", 
                         temporal_resolution="weekly",
                         data_location = "remote_hub_repo") %>%
  dplyr::filter(target_end_date >= first_end_date_thurs,
                target_end_date <= most_recent_end_date_thurs,
                geo_type=="state",
                location != "US") %>%
  dplyr::group_by(location) %>%
  dplyr::mutate(cum_hosp=sum(value)) %>%
  # set the date
  dplyr::select(-c("target_end_date","value"))%>%
  ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(desc(cum_hosp))

hlocs_high_thurs <- hosp_truth_thurs$location[1:5]


# run to build
hosp_forecasts_high_thurs <- map_dfr(date_range_thurs,
                 function(x) {
                   covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                        forecast_date_window_size=6,
                                                        # pick one
                                                        locations = hlocs_high_thurs,
                                                        types = "quantile",
                                                        targets = target_list,
                                                        source = "zoltar")
                   })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_forecasts_high_thurs <- hosp_forecasts_high_thurs %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date_thurs)) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_high_thurs,file = "./data/quantile_frame_hosp_top.csv")


## --------- Low --------------------------------------------------------------------------------##
hosp_truth_thurs_low <- hosp_truth_thurs %>%
  dplyr::arrange(cum_hosp) %>%
  dplyr::filter(as.numeric(location)<60)

hlocs_low <- hosp_truth_thurs_low$location[1:5]

# run to build
hosp_forecasts_low <- map_dfr(hosp_forecasts_high_thurs,
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
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date_thurs)) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_low,file = "./data/quantile_frame_hosp_bottom.csv")



###### Point Forecasts -------------------------------------------------------------------------##
hosp_pt_high <- map_dfr(hosp_forecasts_high_thurs,
                        function(x) {
                          covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                               forecast_date_window_size=6,
                                                               # pick one
                                                               locations = hlocs_high_thurs,
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
hosp_pt_low <- map_dfr(hosp_forecasts_high_thurs,
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




## Saturdays only ---------------------------------------------------- ##########################
first_end_date_sat <- as.Date("2021-01-30") 
most_recent_end_date_sat <- as.Date("2021-06-12")

# note that hlocs_high_sat are different, but we use hlocs_high_thurs for consistency
# hlocs_high_sat = Florida, Texas, New York, California, Michigan
# PA replaces MI for thurs locs, but it is only by ~200 hospitalizations

date_range_sat <- seq.Date(from = first_end_date_sat, 
                             to = most_recent_end_date_sat, by = "week")


# run to build
hosp_forecasts_high_sat <- map_dfr(date_range_sat,
                                     function(x) {
                                       covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                                            forecast_date_window_size=6,
                                                                            locations = hlocs_high_thurs,
                                                                            types = "quantile",
                                                                            targets = target_list,
                                                                            source = "zoltar")
                                     })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_forecasts_high_sat <- hosp_forecasts_high_sat %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date_sat)) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_high_sat,file = "./data/quantile_frame_hosp_top_sat.csv")


## --------- Low --------------------------------------------------------------------------------##
hosp_truth_sat_low <- hosp_truth_sat %>%
  dplyr::arrange(cum_hosp) %>%
  dplyr::filter(as.numeric(location)<60)


hlocs_low <- hosp_truth_sat_low$location[1:5]
# note that hlocs_low_sat is the same as hlocs_low_thurs

# run to build
hosp_forecasts_low_sat <- map_dfr(date_range_sat,
                              function(x) {
                                covidHubUtils::load_latest_forecasts(
                                  last_forecast_date = x,
                                  forecast_date_window_size=6,
                                  # pick one
                                  locations = hlocs_low_thurs,
                                  types = "quantile",
                                  targets = target_list,
                                  source = "zoltar")
                              })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_forecasts_low <- hosp_forecasts_low %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date_sat)) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_low,file = "./data/quantile_frame_hosp_bottom_sat.csv")



###### Point Forecasts -------------------------------------------------------------------------##
hosp_pt_high_sat <- map_dfr(date_range_sat,
                        function(x) {
                          covidHubUtils::load_latest_forecasts(last_forecast_date = x,
                                                               forecast_date_window_size=6,
                                                               # pick one
                                                               locations = hlocs_high_thurs,
                                                               types = "point",
                                                               targets = target_list,
                                                               source = "zoltar")
                        })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_pt_high_sat <- hosp_pt_high_sat %>% 
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  )) 

# write large files
write_csv(hosp_pt_high_sat,file = "./data/point_frame_hosp_top_sat.csv")

##
hosp_pt_low_sat <- map_dfr(date_range_sat,
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
hosp_pt_low_sat <- hosp_pt_low_sat %>% 
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  )) 

# write large files
write_csv(hosp_pt_low_sat,file = "./data/point_frame_hosp_bottom_sat.csv")


library(lubridate)
library(tidyverse)
library(zoltr)
library(covidHubUtils)

# script to build a dataframe of model 

# set targets for analysis
target_list <- paste0(1:28," day ahead inc hosp")


# get locations
hosp_truth <- load_truth("HealthData", 
                         "inc hosp", 
                         temporal_resolution="weekly",
                         data_location = "remote_hub_repo") %>%
  dplyr::filter(target_end_date >= as.Date("2021-01-28"),
                target_end_date <= as.Date("2021-06-12"),
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


hosp_truth_thurs_low <- hosp_truth_thurs %>%
  dplyr::arrange(cum_hosp) %>%
  dplyr::filter(as.numeric(location)<60)

hlocs_low <- hosp_truth_thurs_low$location[1:5]


## Point Forecasts ------------------------------------------------------------------- ############

hosp_pt_high <- map_dfr(date_range_thurs, # this still gives pt forecasts thru Jun. 12
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
write_csv(hosp_pt_high, file = "./data/point_frame_hosp_top.csv")

##
hosp_pt_low <- map_dfr(date_range_thurs,
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


## Thursdays only -------------------------------------------------------------------- ############
date_range_thurs <- seq.Date(from = as.Date("2021-01-28"), 
                             to = as.Date("2021-06-10"), by = "week")

# high
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
  dplyr::filter(weekdays(`target_end_date`) == weekdays(as.Date("2021-01-28"))) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_high_thurs,file = "./data/quantile_frame_hosp_top.csv")


# low
hosp_forecasts_low <- map_dfr(date_range_thurs,
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
  dplyr::filter(weekdays(`target_end_date`) == weekdays(as.Date("2021-01-28"))) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_low,file = "./data/quantile_frame_hosp_bottom.csv")

## Saturdays only ---------------------------------------------------- ##########################
date_range_sat <- seq.Date(from = as.Date("2021-01-30"), 
                           to = as.Date("2021-06-12"), by = "week")


# high
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
  dplyr::filter(weekdays(`target_end_date`) == weekdays(as.Date("2021-01-30"))) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_high_sat,file = "./data/quantile_frame_hosp_top_sat.csv")


# low
hosp_forecasts_low_sat <- map_dfr(date_range_sat,
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
hosp_forecasts_low_sat <- hosp_forecasts_low_sat %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(as.Date("2021-01-30"))) %>%
  dplyr::mutate(horizon_week = case_when(
    horizon %in% 1:7 ~ 1,
    horizon %in% 8:14 ~ 2, 
    horizon %in% 15:21 ~ 3,
    horizon %in% 21:28 ~ 4,
  ))

# write large files
write_csv(hosp_forecasts_low_sat,file = "./data/quantile_frame_hosp_bottom_sat.csv")


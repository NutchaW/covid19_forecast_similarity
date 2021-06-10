# script to build a dataframe of model 
# first sat end date for 1 wk ahead
first_end_date <- as.Date("2021-01-07") # change this to January some time after new year

most_recent_end_date <- as.Date("2021-06-03") # pick a date
  
# use lubridate- or maybe seq () to get a range of dates between the first end date and the most recent one
date_range <- seq.Date(from = first_end_date, to = most_recent_end_date, by = "week")
date_range <- as.Date(date_range)

# set targets for analysis
target_list <- paste0(1:28," day ahead inc hosp")

# run to build
hosp_forecasts <- map_dfr(date_range,
                 function(x) {
                  covidHubUtils::load_latest_forecasts(models = c("JHUAPL-SLPHospEns", "COVIDhub-ensemble"),
                                                        last_forecast_date = x,
                                                        forecast_date_window_size=6,
                                                        # pick one
                                                        locations = c("06"),
                                                        types = "quantile",
                                                        targets = target_list,
                                                        source = "zoltar")
                   })

#create new col to make division of forecasts - by "week" (7 days of forecasts grouped together)
hosp_forecasts_test <- hosp_forecasts %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date)) %>%
  dplyr::mutate(horizon_week = ifelse(horizon %in% 8:14, 2, 
                                      ifelse(horizon %in% 15:21, 3,
                                             ifelse(horizon %in% 22:28, 4, 1))))


######################################### smaller toy data set to play around with

date_range_small <- seq.Date(from = as.Date("2021-04-22"), to = most_recent_end_date, by = "week")

hosp_forecasts_small <- map_dfr(date_range_small,
                          function(x) {
                            covidHubUtils::load_latest_forecasts(models = c("CU-scenario_low", "JHUAPL-Gecko",
                                                                            "JHUAPL-SLPHospEns", "COVIDhub-ensemble"),
                                                                 last_forecast_date = x,
                                                                 forecast_date_window_size=6,
                                                                 # pick one
                                                                 locations = c("06"),
                                                                 types = "point",
                                                                 targets = target_list,
                                                                 source = "zoltar")
                          })

# make easier to read
hosp_forecasts_small_filtered <- hosp_forecasts_small %>%
  dplyr::select(-c(`location_name`, `geo_value`, `full_location_name`, `geo_type`,
                   `location`, `quantile`, `population`)) %>%
  dplyr::filter(weekdays(`target_end_date`) == weekdays(most_recent_end_date)) %>%
  dplyr::mutate(horizon_week = ifelse(horizon %in% 8:14, 2, 
                                      ifelse(horizon %in% 15:21, 3,
                                             ifelse(horizon %in% 22:28, 4, 1))))



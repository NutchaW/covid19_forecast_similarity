# script to build a dataframe of model 
# first sat end date for 1 wk ahead
first_end_date <- as.Date("2020-10-17") # change this to January some time after new year

most_recent_end_date <- # pick a date
  
# use lubridate- or maybe seq() to get a range of dates between the first end date and the most recent one
date_range <- 

# set targets for analysis
target_list <- paste0(1:7," day ahead inc hosp")

# run to build
hosp_forecasts <- map_dfr(date_range,
                 function(x) {
                   covidHubUtils::load_latest_forecasts(models = c(.........),
                                                        last_forecast_date = x,
                                                        forecast_date_window_size=6,
                                                        # pick one
                                                        locations = ...,
                                                        types = "quantile",
                                                        targets = target_list,
                                                        source = "zoltar")})




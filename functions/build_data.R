library(lubridate)
library(tidyverse)
library(zoltr)
library(covidHubUtils)

# script to build a dataframe of model 
# first sat end date for 1 wk ahead
first_sat_end_date <- as.Date("2020-10-17")
# # for cases
# first_sat_end_date <- as.Date("2020-10-17")

#n_weeks_eval <- 10 #weeks included in evaluation
most_recent_sat <- lubridate::floor_date(Sys.Date()-7, unit = "week") - 1  
all_sat_end_dates <- seq(first_sat_end_date,most_recent_sat,by = "week")
#Important dates used (likely there is a cleaner way to code this)
most_recent_monday <- lubridate::floor_date(Sys.Date()-7, unit = "week") + 1 

# # get all mondays for each saturday end dates since first saturday end date for all targets
for(i in 1:4){
  assign(paste0("latest_pos_forecast_date_",i,"wk"),
         c(seq((first_sat_end_date-5)-(7*(i-1)),
               most_recent_monday-(7*(i-1)),by = "week"))
  )
}

# last forecast date after last run for all targets (just for info)
# week_from_last_run <- 1
# for(i in 1:4){
#   dates <- get(paste0("latest_pos_forecast_date_",i,"wk"))
#   assign(paste0("last_pos_forecast_date_",i,"wk"),
#          dates[length(dates)-(week_from_last_run-1)]
#   )
# }


# set targets for analysis
target_list <- paste0(1:4," wk ahead inc death")
target_list2 <- paste0(1:4," wk ahead inc case")

# models_primary_secondary <- get_model_designations(source = "zoltar") %>%
#   filter(designation %in% c("secondary", "primary")) %>%
#   pull(model)

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
# do the same for cases
cases_truth <- load_truth("JHU", 
                          "inc case", 
                          truth_end_date = as.Date("2021-03-08"),
                          temporal_resolution="weekly",
                          data_location = "remote_hub_repo") %>%
  # set the date
  dplyr::filter(target_end_date=="2021-02-27",
                geo_type=="state",
                location != "US")%>%
  dplyr::arrange(desc(value))
clocs <- cases_truth$location[1:5]
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


# write large files
write_csv(rbind(quantile_frame1,
                quantile_frame2,
                quantile_frame3,
                quantile_frame4),file = "./data/quantile_frame.csv")

for(i in 1:4){
  assign(paste0("quantile_frame2_",i),
         map_dfr(get(paste0("latest_pos_forecast_date_",i,"wk")),
                 function(fdates) {
                   covidHubUtils::load_latest_forecasts(last_forecast_date = fdates,
                                                        forecast_date_window_size=6,
                                                        locations = clocs,
                                                        types = "quantile",
                                                        targets = target_list2[i],
                                                        source = "zoltar")})
  )
}

  
write_csv(rbind(quantile_frame2_1,
                quantile_frame2_2,
                quantile_frame2_3,
                quantile_frame2_4),file = "./data/quantile_frame_inc.csv")



#---------------------------------------------------------- For Li ----------------------------------------------------#

# script to build a dataframe of model 
# first sat end date for 1 wk ahead
first_sat_end_date <- as.Date("2020-10-17")
# # for cases
# first_sat_end_date <- as.Date("2020-10-17")

#n_weeks_eval <- 10 #weeks included in evaluation
most_recent_sat <- lubridate::floor_date(Sys.Date()-7, unit = "week") - 1  
all_sat_end_dates <- seq(first_sat_end_date,most_recent_sat,by = "week")
#Important dates used (likely there is a cleaner way to code this)
most_recent_monday <- lubridate::floor_date(Sys.Date()-7, unit = "week") + 1 

# # get all mondays for each saturday end dates since first saturday end date for all targets
for(i in 1:4){
  assign(paste0("latest_pos_forecast_date_",i,"wk"),
         c(seq((first_sat_end_date-5)-(7*(i-1)),
               most_recent_monday-(7*(i-1)),by = "week"))
  )
}

# set targets for analysis
target_list <- paste0(1:4," wk ahead inc death")
target_list2 <- paste0(1:4," wk ahead inc case")

##########---------------------------------Here is where you need to edit-------------------------########
# get truth data for death counts
death_truth <- load_truth("JHU", 
                          "cum death", 
                          truth_end_date = as.Date("2021-03-08"),
                          temporal_resolution="weekly",
                          data_location = "remote_hub_repo") %>%
  dplyr::filter(target_end_date=="2021-02-27",
                geo_type=="state",
                location != "US") %>%
  dplyr::arrange(desc(value))
# get bottom 5
dlocs <- death_truth %>%
  dplyr::filter(location < 60) %>% 
  slice_min(n=5, order_by = value)
  
# do the same for cases
cases_truth <- load_truth("JHU", 
                          "inc case", 
                          truth_end_date = as.Date("2021-03-08"),
                          temporal_resolution="weekly",
                          data_location = "remote_hub_repo") %>%
  # set the date
  dplyr::filter(target_end_date=="2021-02-27",
                geo_type=="state",
                location != "US")%>%
  dplyr::arrange(desc(value))
# get bottom 5
clocs <- cases_truth %>%
  dplyr::filter(location < 60) %>% 
  slice_min(n=5, order_by = value)
  
# run to build
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


# write large files
write_csv(rbind(quantile_frame1,
                quantile_frame2,
                quantile_frame3,
                quantile_frame4),file = "./data/quantile_frame_bottom.csv")

for(i in 1:4){
  assign(paste0("quantile_frame2_",i),
         map_dfr(get(paste0("latest_pos_forecast_date_",i,"wk")),
                 function(fdates) {
                   covidHubUtils::load_latest_forecasts(last_forecast_date = fdates,
                                                        forecast_date_window_size=6,
                                                        locations = clocs,
                                                        types = "quantile",
                                                        targets = target_list2[i],
                                                        source = "zoltar")})
  )
}


write_csv(rbind(quantile_frame2_1,
                quantile_frame2_2,
                quantile_frame2_3,
                quantile_frame2_4),file = "./data/quantile_frame_inc_bottom.csv")


#------------------------------- append to old data previously loaded--------------------------------#
# # get newer data
# for(i in 1:4){
#   assign(paste0("rquantile_frame",i),
#          map_dfr(get(paste0("last_pos_forecast_date_",i,"wk")), 
#                  function(fdates) {
#                    covidHubUtils::load_latest_forecasts(last_forecast_date = fdates,
#                                                         forecast_date_window_size=6,
#                                                         locations = locs,
#                                                         types = "quantile",
#                                                         targets = target_list[i],
#                                                         source = "zoltar")})
#   )
#   assign(paste0("rquantile_frame2_",i),
#          map_dfr(get(paste0("last_pos_forecast_date_",i,"wk")), 
#                  function(fdates) {
#                    covidHubUtils::load_latest_forecasts(last_forecast_date = fdates,
#                                                         forecast_date_window_size=6,
#                                                         locations = locs,
#                                                         types = "quantile",
#                                                         targets = target_list2[i],
#                                                         source = "zoltar")})
#   )
# }
#   
# # write recent files
# write_csv(rbind(rquantile_frame1,
#                 rquantile_frame2,
#                 rquantile_frame3,
#                 rquantile_frame4),file = "./app_inputs/rquantile_frame.csv")
# 
# write_csv(rbind(rquantile_frame2_1,
#                 rquantile_frame2_2,
#                 rquantile_frame2_3,
#                 rquantile_frame2_4),file = "./app_inputs/rquantile_frame_inc.csv")

library(tidyverse)
library(data.table)
library(covidHubUtils)
library(lubridate)
library(zoltr)

# build metadata table
source("./functions/metadata_functions.R")
source("./functions/distance_func_script.R")

# make full list
local_path <- "/Users/ahctun_woon/git/covid19-forecast-hub/"
model_list <- basename(list.dirs(paste0(local_path,
                                        "data-processed/"), recursive=FALSE))
full_list <- map_dfr(model_list, 
                     function(x) {
                       get_model_metadata(x, local_path)
                       })
# extract
categorized_frame <- make_metadata_table(full_list,summarizing = TRUE)
write.csv(categorized_frame,"./metadata_categorized.csv",row.names = FALSE)

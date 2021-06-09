library(tidyverse)
library(data.table)
library(covidHubUtils)
library(lubridate)
library(zoltr)

# build metadata table
source("./functions/metadata_functions.R")
source("./functions/distance_func_script.R")

# make full list
full_list <- list.files("./data/metadata")
full_list_death <- map_dfr(full_list, 
                           function(x) {
                             make_metadata_table(paste0("./data/metadata/",x))
                           })

# extract
categorized_frame <- extract_data(full_list_death)
write.csv(categorized_frame,"./metadata_categorized.csv",row.names = FALSE)

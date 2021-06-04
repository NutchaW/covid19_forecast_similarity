# function to process one file path
get_metadata_table <- function(file_path, sep, fill, header) {
  sep = ":"
  fill = TRUE
  header = FALSE
  
  table <- read.table(file = file_path, sep = sep, fill = fill, header = header)
  
  table <- table %>% 
    mutate_all(list(~na_if(., ""))) %>%
    unite("N", `V2`:`V3`, na.rm = TRUE, remove = FALSE) %>%
    select(`V1`, `N`)
  
  col_names <- tibble(V1 = c("team_name", 
                             "model_name", 
                             "model_abbr", 
                             "model_contributors", 
                             "website_url", 
                             "license", 
                             "team_model_designation", 
                             "methods",
                             "institutional_affil",
                             "team_funding",
                             "repo_url",
                             "twitter_handles",
                             "data_inputs",
                             "citation",
                             "methods_long"))
  
  table <- as_tibble(table)
  table <- full_join(col_names, table, by = "V1")
  table <- t(table)
  table <- as_tibble(table)
  table <- filter(table, `V1` != "team_name")
  
}


# using map_dfr() to take a list of the file paths - IN PROGRESS
  ## currently works using a vector like this 
  
metadata_list <- c("AIpert-pwllnod/metadata-AIpert-pwllnod.txt", 
                   "YYG-ParamSearch/metadata-YYG-ParamSearch.txt")

get_metadata_full <- map_dfr(metadata_list, 
                             function(metadata_list) {
                               get_metadata_table(paste("./data-processed/", metadata_list, sep = ""), 
                                                  sep = ":", 
                                                  fill = TRUE, 
                                                  header = FALSE) 
                             })

## for some reason, this doesn't work using the full list
metadata_list_full <- list.files(path = "./data-processed", pattern = "metadata", 
                                 recursive = TRUE, ignore.case = FALSE, include.dirs = TRUE)

get_metadata_full2 <- map_dfr(metadata_list_full, 
                             function(metadata_list_full) {
                               get_metadata_table(paste("./data-processed/", metadata_list_full, sep = ""), 
                                                  sep = ":", 
                                                  fill = TRUE, 
                                                  header = FALSE) 
                             })

#add col names
colnames(get_metadata_full) <- c("team_name", 
                                 "model_name", 
                                 "model_abbr", 
                                 "model_contributors", 
                                 "website_url", 
                                 "license", 
                                 "team_model_designation", 
                                 "methods",
                                 "institutional_affil",
                                 "team_funding",
                                 "repo_url",
                                 "twitter_handles",
                                 "data_inputs",
                                 "citation",
                                 "methods_long")
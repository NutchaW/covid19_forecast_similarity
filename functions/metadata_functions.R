# function to process one file path
get_metadata_table <- function(file_path, sep, fill, header) {
  sep = ":"
  fill = TRUE
  header = FALSE
  
  table <- read.table(file = file_path, sep = sep, quote="",fill = fill, header = header)
  
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

get_metadata_table2 <- function(file_path, sep, fill, header) {
  sep = ":"
  fill = TRUE
  header = FALSE
  
  table <- read.table(file = file_path, sep = sep, quote="",fill = fill, header = header)
  
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

# function to process one file path
# get_metadata_table <- function(file_path, sep, fill, header) {
#   sep = ":"
#   fill = TRUE
#   header = FALSE
#   
#   table <- read.table(file = file_path, sep = sep, quote="",fill = fill, header = header)
#   
#   table <- table %>% 
#     mutate_all(list(~na_if(., ""))) %>%
#     unite("N", `V2`:`V3`, na.rm = TRUE, remove = FALSE) %>%
#     select(`V1`, `N`)
#   
#   col_names <- tibble(V1 = c("team_name", 
#                              "model_name", 
#                              "model_abbr", 
#                              "model_contributors", 
#                              "website_url", 
#                              "license", 
#                              "team_model_designation", 
#                              "methods",
#                              "institutional_affil",
#                              "team_funding",
#                              "repo_url",
#                              "twitter_handles",
#                              "data_inputs",
#                              "citation",
#                              "methods_long"))
#   
#   table <- as_tibble(table)
#   table <- full_join(col_names, table, by = "V1")
#   table <- t(table)
#   table <- as_tibble(table)
#   table <- filter(table, `V1` != "team_name")
#   
# }

make_metadata_table <- function(path) {
  line_dat <- suppressWarnings(readLines(path)) %>%
    sub("^\\s+", "", .)
  tmp <- data.frame(str_match(line_dat,"((\\w+):|(\\s+))\\s*(.+)")[,c(3,5)]) %>%
    .[rowSums(is.na(.))<ncol(.),]
  for(i in rev(2:nrow(tmp))){
    if(is.na(tmp$X1[i])) {
      tmp$X2[i-1] <- paste0(tmp$X2[i-1],tmp$X2[i]) 
    }}
  frame <- na.omit(tmp) %>% 
    tidyr::pivot_wider(names_from = X1, values_from = X2)
  col_info <- c("team_name","model_name","model_abbr","methods","data_inputs","methods_long")
  cnames <- setdiff(col_info,colnames(frame))
  if(length(cnames)==0){
    final_frame <- frame %>%
      dplyr::select(col_info) %>%
      .[c(col_info)]
    } else {
    na.frame <- data.frame(matrix(rep(NA,length(cnames)),ncol=2))
    names(na.frame) <- cnames
    final_frame <- frame %>%
      dplyr::select(col_info[!col_info %in% grep(paste0(cnames, collapse = "|"), col_info, value = T)])
    final_frame <- cbind(final_frame,na.frame)[col_info]
  }
  return(final_frame)
}

extract_data <- function(frame){
  main <- frame[,1:3]
  # check for Bayesian
  bayes.c <- "bayes|bayesian"
  main$bayesian <- ifelse((grepl(bayes.c,tolower(main$model_name)))|
                            (grepl(bayes.c,tolower(main$methods)))|
                            (grepl(bayes.c,tolower(main$methods_long))),
                          TRUE,FALSE)
  # check for intervention
  int.c <- ""
  main$intervention <- ifelse((grepl(int.c,tolower(main$methods)))|
                                (grepl(int.c,tolower(main$methods_long))),
                              TRUE,FALSE)
  # check for machine learning
  # check for
  return(main)
}

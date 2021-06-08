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
  # method check
  # check for Bayesian
  bayes.c <- "bayes|bayesian"
  # check for intervention
  int.c <- "intervention|compliance|social distanc|policy"
  # check for machine learning
  ml.c <- "neural network|random forecast|learning"
  # check for compartmental 
  compart.c <- "sir|seir|compartmental"
  # check for spatio
  spatio.c <- "spatio"
  # agent based
  agent.c <- "agent"
  # ensemble or not
  ensemble.c <- "ensemble|pool|pooling"
  # human expert or not
  hexpert.c <- "experts|crowdsourc"
  # time series or not
  ts.c <- "time series|arima|sarima|arma"
  # data check
  # JHU cases/deaths
  jhu.c <- "jhu"
  # NYTimes cases/deaths
  nyt.c <- "nytimes|new york times"
  # usa facts for data
  usaf.c <- "usafacts|usa facts"
  # check for mobility data
  mobil.c <- "mobility|gps|gis|tracking"
  # demographic data (for in methods and data)
  demog.c <- "demographic|socioeconomic|age|demography"
  # implement checks
  main <- frame %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bayesian=  ifelse((grepl(bayes.c,tolower(model_name)))|
                                      (grepl(bayes.c,tolower(methods)))|
                                      (grepl(bayes.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  # add NA check
                  na_input= is.na(data_inputs),
                  intervention=  ifelse((grepl(int.c,tolower(model_name)))|
                                      (grepl(int.c,tolower(methods)))|
                                      (grepl(int.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  machine_learning=  ifelse((grepl(ml.c,tolower(model_name)))|
                                      (grepl(ml.c,tolower(methods)))|
                                      (grepl(ml.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  compartmental=  ifelse((grepl(compart.c,tolower(model_name)))|
                                      (grepl(compart.c,tolower(methods)))|
                                      (grepl(compart.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  spatio=  ifelse((grepl(spatio.c,tolower(model_name)))|
                                      (grepl(spatio.c,tolower(methods)))|
                                      (grepl(spatio.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  agent_based=  ifelse((grepl(agent.c,tolower(model_name)))|
                                      (grepl(agent.c,tolower(methods)))|
                                      (grepl(agent.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  ensemble=  ifelse((grepl(ensemble.c,tolower(model_name)))|
                                      (grepl(ensemble.c,tolower(methods)))|
                                      (grepl(ensemble.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  human_expert=  ifelse((grepl(hexpert.c,tolower(model_name)))|
                                      (grepl(hexpert.c,tolower(methods)))|
                                      (grepl(hexpert.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  time_series=  ifelse((grepl(ts.c,tolower(model_name)))|
                                      (grepl(ts.c,tolower(methods)))|
                                      (grepl(ts.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  JHU_data=  ifelse((grepl(jhu.c,tolower(data_inputs))),
                                    TRUE,FALSE),
                  NYTimes_data=  ifelse((grepl(nyt.c,tolower(data_inputs))),
                                    TRUE,FALSE),
                  USAfacts_data=  ifelse((grepl(usaf.c,tolower(data_inputs))),
                                    TRUE,FALSE),
                  mobility_data=  ifelse((grepl(mobil.c,tolower(data_inputs)))|
                                      (grepl(mobil.c,tolower(methods)))|
                                      (grepl(mobil.c,tolower(methods_long))),
                                    TRUE,FALSE),
                  demography=  ifelse((grepl(demog.c,tolower(data_inputs)))|
                                      (grepl(demog.c,tolower(methods)))|
                                      (grepl(demog.c,tolower(methods_long))),
                                    TRUE,FALSE)
                  )
  return(main)
}

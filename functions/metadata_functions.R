get_model_metadata <- function(models, hub_repo_path) {
  if (missing(hub_repo_path)){
    stop ("Error in get_model_designations: Please provide a hub_repo_path")
  } else {
    model_metadata_paths = paste0('data-processed/',models,'/metadata-',models,'.txt')
    
    model_info <- purrr::map_dfr(
      model_metadata_paths,
      function (model_metadata_path){
        commits_command <- paste0("cd ",hub_repo_path,
                                  "; git log --date=unix --follow -- ",
                                  model_metadata_path)
        # invoke command and parse result
        all_commits <- system(commits_command,intern = TRUE) %>%
          stringr::str_split_fixed(" ", 2) %>%
          as.data.frame() %>%
          dplyr::rename(info1 = V1, info2 = V2)
        recent_commit_sha <- all_commits$info2[all_commits$info1=="commit"]

        # construct git command to read metadata file
        read_command_all <- paste0("cd ",hub_repo_path,"; git show ",
                               recent_commit_sha,":./",model_metadata_path)
        # check if file exists in commit
        check_command_list <- paste0("cd ",hub_repo_path,"; git show ",
                                     recent_commit_sha,":",model_metadata_path," > /dev/null 2>&1")
        exit_code <- sapply(check_command_list, function(command_line) system(command_line))
        read_command <- read_command_all[which(exit_code==0)]
        purrr::map_dfr(read_command,
                       function(x) {
                         string_clean <- system(x, intern = TRUE)
                         if(class(try(yaml::yaml.load(string_clean),silent=TRUE))!="try-error" &&
                            grepl("team_name",paste(string_clean,collapse=" ")) &&
                            grepl("model_name",paste(string_clean,collapse=" ")) &&
                            grepl("model_abbr",paste(string_clean,collapse=" "))){
                           metadata_list <- yaml::yaml.load(string_clean)
                           metadata_list[c(which(sapply(metadata_list,function(y) is.null(y))==TRUE))] <- NA
                           as.data.frame(metadata_list,stringsAsFactors = FALSE)  %>%
                              dplyr::mutate(date = as.POSIXct(as.numeric(all_commits$info2[all_commits$info1=="Date:"][
                                which(read_command==x)]), origin="1970-01-01")) %>%
                              tidyr::gather(attribute, value, -c(team_name, model_name,model_abbr,date))
                         }
                       }
          )
      })
  }
  return(model_info)
}


make_metadata_table <- function(frame,summarizing=FALSE) {
  # check for Bayesian
  bayes.c <- "bayes|bayesian"
  # check for intervention
  int.c <- "intervention|compliance|social distanc|policy"
  # check for machine learning
  ml.c <- "neural network|random forecast|learning"
  # check for compartmental 
  compart.c <- "sir|seir|slir|compartment|differential equations"
  # check for spatio
  spatial.c <- "spatio|spatial"
  # agent based
  agent.c <- "agent"
  # ensemble or not
  ensemble.c <- "ensemble|pool|pooling"
  # human expert or not
  hexpert.c <- "experts|crowdsourc"
  # data check
  # JHU cases/deaths
  jhu.c <- "jhu|john hopkins"
  # NYTimes cases/deaths
  nyt.c <- "nytimes|new york times"
  # covid tracking
  covidtracking.com.c <- "covidtracking"
  # usa facts for data
  usaf.c <- "usafacts|usa facts"
  # check for mobility data
  mobil.c <- "mobility|gps|gis|tracking"
  # demographic data (for in methods and data)
  demog.c <- "demographic|socioeconomic|age|demography"
  # statistical
  stats.c <- "gams|regression|arma|arima|splines|time series|time-series|baseline|average"
  final_frame <- frame %>%
    dplyr::filter(attribute %in% 
                    c("data_inputs","methods","methods_long")) %>%
    dplyr::distinct(.,.keep_all = TRUE) %>%
    group_by(model_name,team_name,model_abbr,date)%>%
    tidyr::pivot_wider(names_from=attribute,values_from=value) 
  if(summarizing==TRUE){
    final_frame <- final_frame %>%
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
                  JHU_data=  ifelse((grepl(jhu.c,tolower(data_inputs))),
                                    TRUE,FALSE),
                  NYTimes_data=  ifelse((grepl(nyt.c,tolower(data_inputs))),
                                        TRUE,FALSE),
                  covidtracking.com_data=  ifelse((grepl(covidtracking.com.c,tolower(data_inputs))),
                                                  TRUE,FALSE),
                  USAfacts_data=  ifelse((grepl(usaf.c,tolower(data_inputs))),
                                         TRUE,FALSE),
                  mobility_data=  ifelse((grepl(mobil.c,tolower(methods)))|
                                           (grepl(mobil.c,tolower(methods_long))),
                                         TRUE,FALSE),
                  demography=  ifelse((grepl(demog.c,tolower(data_inputs)))|
                                        (grepl(demog.c,tolower(methods)))|
                                        (grepl(demog.c,tolower(methods_long))),
                                      TRUE,FALSE),
                  stats=  ifelse((grepl(stats.c,tolower(data_inputs)))|
                                   (grepl(stats.c,tolower(methods)))|
                                   (grepl(stats.c,tolower(methods_long))),
                                 TRUE,FALSE),
                  hybrid =  ifelse(sum(stats,ensemble,agent_based,compartmental,machine_learning)>1, TRUE,FALSE))
  }
  return(final_frame)
}


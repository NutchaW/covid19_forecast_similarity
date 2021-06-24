## wrapper and functions for calculating pairwise CvM
## a function to format the dataframe
frame_format <- function(zoltr_frame){
  n_locs <- length(unique(zoltr_frame$location))
  # filter
  formatted_frame <- zoltr_frame %>%
    dplyr::filter(!any(is.na(value)),
                  !any(is.null(value))) %>%
    # filtering on quantile, which is the smallest
    dplyr::group_by(location, horizon,  target_end_date, model) %>%
    mutate(n_q = n_distinct(quantile)) %>%
    ungroup() %>%
    dplyr::filter(n_q==max(n_q)) %>%
    dplyr::select(-"n_q") %>%
    # start filtering date and location and horizon
    group_by(model, horizon,  target_end_date) %>% #Add count of locations
    mutate(n_locations = n_distinct(location)) %>%
    ungroup()  %>%
    dplyr::filter(n_locations==n_locs) %>%
    group_by(model, location, target_end_date) %>% #Add count of weeks
    dplyr::mutate(n_horizons = n_distinct(horizon)) %>%
    ungroup() %>%
    dplyr::filter(n_horizons==max(n_horizons)) %>%
    group_by(model, horizon, location) %>%
    mutate(n_dates = n_distinct(target_end_date)) %>%
    dplyr::filter(n_dates==max(n_dates)) %>%
    ungroup() %>%
    dplyr::select(-c("n_horizons","n_locations","n_dates"))
  # final clean-up
  matrix_frame <- formatted_frame %>%
    dplyr::select("location","target_variable","target_end_date",
                  "type","quantile","model","value","horizon") %>%
    dplyr::arrange(location,horizon,target_variable,target_end_date,model,quantile) %>%
    tidyr::pivot_wider(names_from = model, values_from = value) %>%
    dplyr::select_if(~ !any(is.na(.)))
  return(matrix_frame)
} 
  
## a function that takes a data frame with single target-location and calculates CvM for pairwise combinations
## this returns a matrix of CvM for the models for a single target-location

cd_combination <- function(single_tarloc_frame,approx_rule, tau_F,tau_G){
  # remove any models with NA for values for this target location (assuming all models have the same quantiles)
  single_tarloc<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
  # pairwise column calculation
  tmp <- single_tarloc %>%
    dplyr::select(-c("target_variable","target_end_date","location","type","quantile","horizon"))
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length=nr)
  for (i in 1:nr) {
    cc <- calc_cramers_dist_one_model_pair(as.numeric(unlist(tmp[,eg[i,1]])),
                                           tau_F,
                                           as.numeric(unlist(tmp[,eg[i,2]])),
                                           tau_G,
                                           approx_rule)
    v[i] <- cc
  }
  single_tarloc_cvm <- data.frame(model_1=rep(cnames,nc),
                                  model_2=rep(cnames,each=nc),
                                  approx_cd=v)
  return(single_tarloc_cvm)
}



## A wrapper to create a list of data frame for each target-location combination
### by filter on target and location
build_distance_frame <- function(model_dataframe, horizon_list,target_list, approx_rule,tau_F,tau_G){
  library(tidyverse)
  library(tidyr)
  # remove point estimates and forecast_date
  main_frame <- model_dataframe %>% 
      dplyr::mutate(horizon = as.numeric(as.character(horizon)),
                    target_variable = as.character(target_variable),
                    location = as.character(location), 
                    target_end_date = as.Date(target_end_date)) %>%
      dplyr::filter(target_variable %in% target_list,
                    horizon %in% horizon_list) 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply distance_combination function 
  locations <- unique(main_frame$location)
  dist_frame <- data.frame()
  for(loc in locations){
      for(target in target_list){
        for(horiz in horizon_list){
          tar_loc <- main_frame %>%
            dplyr::filter(location==loc,
                          target_variable==target,
                          horizon==horiz) 
          end_dates <- unique(tar_loc$target_end_date)
          for(end_date in end_dates){
            single_matrix <- tar_loc %>%
              dplyr::filter(target_end_date==end_date) %>%
              dplyr::arrange(quantile) %>%
              cd_combination(., approx_rule, tau_F,tau_G) %>%
              dplyr::mutate(horizon=horiz,
                            location=loc,
                            target_variable=target,
                            target_end_date=end_date)
            rbind(dist_frame,single_matrix) -> dist_frame
          }
        }
      }
  }
  # calculate mean distance
  mean_frame <- dist_frame %>%
    dplyr::select(-"target_end_date") %>%
    dplyr::group_by(horizon,location,target_variable,model_1,model_2) %>%
    dplyr::mutate(mean_dis=mean(approx_cd)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"approx_cd") %>%
    dplyr::arrange(factor(model_2, levels = unique(model_2))) %>%
    distinct()
  # median_frame <- dist_frame %>%
  #   dplyr::select(-"target_end_date") %>%
  #   dplyr::group_by(horizon,location,target_variable,model_1,model_2) %>%
  #   dplyr::mutate(mean_dis=median(approx_cd)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(-"approx_cd") %>%
  #   dplyr::arrange(factor(model_2, levels = unique(model_2))) %>%
  #   distinct()
  mean_locframe <- dist_frame %>%
    dplyr::select(-"target_end_date") %>%
    dplyr::group_by(horizon,target_variable,model_1,model_2) %>%
    dplyr::mutate(mean_dis=mean(approx_cd)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("approx_cd","location")) %>%
    dplyr::arrange(factor(model_2, levels = unique(model_2))) %>%
    distinct()
  return(list(full_dataframe=dist_frame,mean_dataframe=mean_frame,loc_mean = mean_locframe))
}

# create matrix
cd_matrix <- function(mean_cd_frame, h){
# cd_matrix <- function(mean_cd_frame, h, target){
  # pairwise column calculation
  # arrange by largest average of mean dis compared to all forecasts
  tmp <-mean_cd_frame %>% 
    # dplyr::filter(horizon==h,target_variable==target) %>%
    dplyr::filter(horizon==as.numeric(h)) %>%
    # dplyr::group_by(horizon,target_variable,model_1) %>%
    dplyr::group_by(horizon,model_1) %>%
    dplyr::mutate(dist_ens = mean_dis[model_2=="COVIDhub-ensemble"]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(dist_ens)) 
  nc <- length(unique(tmp$model_1))
  if((nc^2 != nrow(tmp))){
    stop(paste0("cannot create a square matrix - check filtering for ",h,"-",target))
  } 
  ord <-  tmp$model_1[(1:nrow(tmp) %% nc)==1]
  mean_cd <- matrix(NA,nc,nc)
  for(i in 1:nc){
    for(j in 1:nc){
      mean_cd[i,j] <- tmp$mean_dis[which(tmp$model_1==ord[i] & tmp$model_2==ord[j])]
    }
  }
  dimnames(mean_cd) <- list(ord,ord)
  return(mean_cd)
}

# a function that takes a matrix and plot a heatmap
# distance_heatmap <- function(sum_dist,h,target,name){
distance_heatmap <- function(sum_dist,name,metadata=NULL){
  tmp <- sum_dist %>%
    dplyr::group_by(model_1,model_2) %>%
    dplyr::mutate(ov_mean=mean(mean_dis)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"mean_dis") %>%
    dplyr::distinct(.,.keep_all = TRUE) %>%
    dplyr::arrange(desc(ov_mean))
  order_list <- unique(tmp$model_1)
  if (is.null(metadata)) {
    ggplot(sum_dist, aes(factor(model_1,levels=order_list),
                         factor(model_2,levels=order_list))) +
      geom_tile(aes(fill= mean_dis)) +
      facet_wrap(~horizon) +
      theme(axis.text.x=element_text(size=rel(0.7),angle=45,hjust=1),
            axis.text.y=element_text(size=rel(0.7)))+
      labs(title=name)+
      xlab("") +
      ylab("") +
      scale_fill_distiller(palette = "YlOrRd",direction=+1,name="distance") +
      theme(plot.title = element_text(size=8),
            legend.title = element_text(size=5),
            legend.key.size = unit(0.3, 'cm'),
            legend.text = element_text(size=4),
            plot.margin=unit(c(0,0,0,0),"cm"))
    
  } else {
    set1<-c("red","blue","purple","pink","orange")
    color_set <- ifelse(metadata$model_type == "ensemble", 
                        set1[1], 
                        ifelse(metadata$model_type ==  "both stats and mech",
                               set1[2],
                               ifelse(metadata$model_type == "statistical",
                                      set1[3],
                                      ifelse(metadata$model_type =="mechanistic",
                                             set1[4],set1[5]))))
    type_color <-  color_set[order(match(metadata$model_abbr,order_list))]
    ggplot(sum_dist, aes(factor(model_1,levels=order_list),
                         factor(model_2,levels=order_list))) +
      geom_tile(aes(fill= mean_dis)) +
      facet_wrap(~horizon) +
      theme(axis.text.x=element_text(size=rel(0.7),angle=45,hjust=1, colour = type_color),
            axis.text.y=element_text(size=rel(0.7), colour = type_color))+
      labs(title=name)+
      xlab("") +
      ylab("") +
      scale_fill_distiller(palette = "YlOrRd",direction=+1,name="distance") +
      theme(plot.title = element_text(size=8),
            legend.title = element_text(size=5),
            legend.key.size = unit(0.3, 'cm'),
            legend.text = element_text(size=4),
            plot.margin=unit(c(0,0,0,0),"cm"))
  }
}

scatter <-  function(data,title_name,metadata=NULL,smooth_tf=FALSE){
  dat <- data %>% 
    dplyr::left_join(metadata,by=c("model_2"="model_abbr")) %>%
    dplyr::mutate(Model=model_2) 
  if (is.null(metadata)) {
    p<-ggplot(dat, aes(x=target_end_date, y=approx_cd,col=Model)) + 
   #   geom_point(alpha=0.6,size=0.8) + 
      geom_line(alpha=0.4) +
      ggtitle(title_name) +
      ylab("Approx. CD") +
      xlab("Forecast End Date") +
      facet_wrap(vars(horizon), nrow = 2,scales = "free") +
      theme(legend.text = element_text(size=5),
            legend.title = element_text(size=7),
            axis.text.x=element_text(size=rel(0.7),angle=45,hjust=1),
            legend.key.size = unit(0.5, 'cm'))+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%m-%y")
  } else if(smooth_tf){
    ggplot(dat, aes(x=target_end_date, y=mean_approx_cd,
                    col=Model,group=interaction(Model, model_type),linetype=model_type)) + 
      geom_point(alpha=0.6,size=0.8) + 
      stat_smooth(alpha=0.4,size=0.5,aes(x = target_end_date, y = mean_approx_cd), method = "loess",
                  formula = y ~ x, se = FALSE) +
    ggtitle(title_name) +
      ylab("Approx. CD") +
      xlab("Forecast End Date") +
      facet_wrap(vars(horizon), nrow = 2,scales = "free") +
      theme(legend.text = element_text(size=5),
            legend.title = element_text(size=9),
            axis.text.x=element_text(size=rel(0.7),angle=45,hjust=1),
            legend.key.size = unit(0.5, 'cm'))+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%m-%y")
  }
  else {
    ggplot(dat, aes(x=target_end_date, y=mean_approx_cd,col=Model,group=interaction(Model, model_type),linetype=model_type)) + 
 #     geom_point(alpha=0.6,size=0.8) + 
      geom_line(alpha=0.4) +
      ggtitle(title_name) +
      ylab("Approx. CD") +
      xlab("Forecast End Date") +
      facet_wrap(vars(horizon), nrow = 2,scales = "free") +
      theme(legend.text = element_text(size=5),
            legend.title = element_text(size=9),
            axis.text.x=element_text(size=rel(0.7),angle=45,hjust=1),
            legend.key.size = unit(0.5, 'cm'))+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%m-%y")
  }
}

sym_mat <- function(X){
  ind1 <- apply(X, 1, function(x) any(is.na(x)))
  ind2 <- apply(X, 2, function(x) any(is.na(x)))
  X_sym <- X[ !ind1, !ind2 ]
  return(as.dist(X_sym))
}

dendro_plot <- function(horizon, frame_name,metadata,type=TRUE){
  hclust_dat <- hclust(as.dist(get(frame_name)[[horizon]]), 
                       method = "ward.D", members = NULL)
  ddata <- label(dendro_data(as.dendrogram(hclust_dat))) %>%
    left_join(metadata,by=c("label"="model_abbr"))
  set1<-c("red","blue","purple","pink","orange")
  if(type){
    color_set <- ifelse(ddata$model_type == "ensemble", 
                        set1[1], 
                        ifelse(ddata$model_type ==  "both stats and mech",
                               set1[2],
                               ifelse(ddata$model_type == "statistical",
                                      set1[3],
                                      ifelse(ddata$model_type =="mechanistic",
                                             set1[4],set1[5]))))
  } else {
    color_set <- ifelse(ddata$data_source == "JHU", set1[1],set1[2])
  }
  dendro_p <- ggdendrogram(
           hclust(as.dist(get(frame_name)[[horizon]]), method = "ward.D",members = NULL)
           ,size = 2, rotate=TRUE) +
           labs(title=paste0("Dendrogram - ",i, " wk ahead inc death"))+
           xlab("") +
           ylab("Mean Cramer's Distance") +
           theme(axis.text.x = element_text(size=5),
                 axis.text.y = element_text(size=7,colour=color_set),
                 plot.margin=unit(c(0,0,0,0),"cm"),
                 plot.title = element_text(size=8)
           )
} 

catbox_plot <- function(dat){
   ggplot(dat, aes(x=mech_type, y=mean_dis,fill=as.factor(horizon))) +
   geom_boxplot() +
   xlab("") +
   ylab("Mean Approc. CD") +
   guides(fill=guide_legend(title="horizon"))
} 
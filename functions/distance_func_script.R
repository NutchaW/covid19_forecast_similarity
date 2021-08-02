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

scatter <-  function(data,title_name,type,xname,yname){
  if(type=="wis"){
    data %>% 
      ggplot(., aes(x=wis, y=mean,color=factor(horizon))) + 
      geom_point()+
      stat_smooth(method = "loess", se = FALSE)+
      ggtitle(title_name) +
      ylab(yname) +
      xlab(xname) +
      theme(legend.text = element_text(size=5),
            legend.title = element_text(size=9),
            legend.key.size = unit(0.5, 'cm'))
  } else if(type=="current"){
    data %>% 
      ggplot(., aes(x=truth, y=sim,color=factor(horizon))) + 
      geom_point()+
      stat_smooth(method = "loess", se = FALSE)+
      ggtitle(title_name) +
      ylab(yname) +
      xlab(xname) +
      theme(legend.text = element_text(size=5),
            legend.title = element_text(size=9),
            legend.key.size = unit(0.5, 'cm'))
  }else if(type=="future"){
    data %>% 
      ggplot(., aes(x=sim, y=truth,color=factor(horizon))) + 
      geom_point()+
      stat_smooth(method = "loess", se = FALSE)+
      ggtitle(title_name) +
      ylab(yname) +
      xlab(xname) +
      theme(legend.text = element_text(size=5),
            legend.title = element_text(size=9),
            legend.key.size = unit(0.5, 'cm'))
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
                 plot.title = element_text(size=7)
           )
} 

catbox_plot <- function(dat,name){
   ggplot(dat, aes(x=target_end_date, y=mean_approx_cd, group=target_end_date)) +
   geom_boxplot(outlier.size = 0.5) +
   facet_wrap(~horizon) +
   xlab("Date") +
   ylab("Mean Approx. CD") +
   guides(fill=guide_legend(title="horizon"))+
   ggtitle(name)+
   scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y")+
   theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
         axis.text.y = element_text(size=7),
         plot.title = element_text(size=10),
         axis.title=element_text(size=8))
} 

catbox_plot2 <- function(dat,name){
  ggplot(dat, aes(x=target_end_date, y=mean_approx_cd,fill=factor(horizon))) +
    geom_boxplot(outlier.size = 0.5) +
    xlab("Forecast Date") +
    ylab("Mean Approx. CD") +
    guides(fill=guide_legend(title="horizon",label.position ="bottom",direction="horizontal"))+
    ggtitle(name)+
    theme(axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          plot.title = element_text(size=10),
          axis.title=element_text(size=8))
} 

plot_step <- function(forecasts, model1, model2,horizon){
  # take filter data and plot step function for a pair of forecasts (1 target)
  p_10 <- sort(unique(forecasts$quantile)) # quantile levels
  mod1 <- unlist(c(forecasts[,grep(model1, colnames(forecasts))]))
  mod2 <- unlist(forecasts[,grep(model2, colnames(forecasts))])
  rge <- c(mod1,mod2)
  cd <- covidHubUtils::calc_cramers_dist_unequal_space(q_F=mod1, tau_F=p_10, q_G=mod2, tau_G=p_10,
                                                       approx_rule="trapezoid_riemann")
  
  # plot(mod1, p_10, type = "s", xlab = paste0(horizon,"-week ahead predictive quantiles"), ylab = "CDF", col = "red",xlim=c(min(rge)+100,max(rge)+100))
  # lines(mod2, p_10, type = "s", col = "blue")
  dat <- data.frame(rbind(cbind(mod1,p_10,rep(model1,length(p_10))),cbind(mod2,p_10,rep(model2,length(p_10)))))
  names(dat)  <- c("q","p","Model")
  dat$q <- as.numeric(dat$q)
  dat$p <- as.numeric(dat$p)
  p <- ggplot(dat,aes(x=q,y=p,group=Model,color=Model))+
    geom_line()
  p +
    theme_bw() +
    theme(legend.position="bottom",
          text = element_text(size=5))+
    xlab(paste0(horizon,"-week ahead predictive quantiles"))+
    ylab("Probability level") +
    annotate(geom="text", x=quantile(rge,0.90), y=0.2, label= paste0("approx. CD = ",round(cd,2)))
}

# plot_cd <- function(forecasts, model1, model2){
#   # take filter data and plot cd for a pair of forecasts (1 target)
#   p_10 <- sort(unique(forecasts$quantile)) # quantile levels
#   mod1 <- unlist(c(forecasts[,grep(model1, colnames(forecasts))]))
#   mod2 <- unlist(forecasts[,grep(model2, colnames(forecasts))])
#   rge <- sort(c(mod1,mod2))
#   cd <- covidHubUtils::calc_cramers_dist_unequal_space(q_F=mod1, tau_F=p_10, q_G=mod2, tau_G=p_10,
#                                                        approx_rule="trapezoid_riemann")
#   q0 <- c(sort(mod1), sort(mod2))
#   # indicator whether the entry is from F or G
#   q <- q0[order(q0)]
#   tf <- unlist(sapply(q, function(x) ifelse(x %in% sort(mod1),p_10[which(x == sort(mod1))],0)))
#   tg <- unlist(sapply(q, function(x) ifelse(x %in% sort(mod2),p_10[which(x == sort(mod1))],0)))
#   #diffs_q <- diff(q)
#   # probability level vectors
#   tau_F_v <- cummax(tf)
#   tau_G_v <- cummax(tg)
#   #plot
#   plot(sort(rge), (tau_F_v - tau_G_v)^2, type = "s", xlab = "x",
#        ylab = "(F*(x) - G*(x))^2")
#   polygon(sort(rge), (tau_F_v - tau_G_v)^2, col = "lightblue")
#   legend("topleft", paste0("approx. CD = ",cd), col = "lightblue", pch = 15)
# }

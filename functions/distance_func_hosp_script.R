## a selection of functions for calculating pairwise CvM customized for hospitalization data
## plus several new functions added

## a function to format the dataframe
frame_format2 <- function(zoltr_frame){
  n_locs <- length(unique(zoltr_frame$location))
  # filter
  formatted_frame <- zoltr_frame %>%
    dplyr::filter(!any(is.na(value)),
                  !any(is.null(value))) %>%
    # filtering on quantile, which is the smallest
    dplyr::group_by(location, horizon_week, target_end_date, model) %>%
    mutate(n_q = n_distinct(quantile)) %>%
    ungroup() %>%
    dplyr::filter(n_q==max(n_q)) %>%
    dplyr::select(-"n_q") %>%
    # start filtering date and location and horizon
    group_by(model, horizon_week,  target_end_date) %>% #Add count of locations
    mutate(n_locations = n_distinct(location)) %>%
    dplyr::filter(n_locations==n_locs) %>%
    ungroup()  %>%
    group_by(model, location, target_end_date) %>% #Add count of weeks
    dplyr::mutate(n_horizons = n_distinct(horizon_week)) %>%
    ungroup() %>%
    dplyr::filter(n_horizons==max(n_horizons)) %>%
    group_by(model, horizon_week, location) %>%
    mutate(n_dates = n_distinct(target_end_date)) %>%
    ungroup() %>%
    dplyr::filter(n_dates==max(n_dates)) %>%
    dplyr::select(-c("n_horizons","n_locations","n_dates"))
  # final clean-up
  matrix_frame <- formatted_frame %>%
    dplyr::select("location","target_variable","target_end_date",
                  "type","quantile","model","value","horizon_week") %>%
    rename(horizon = `horizon_week`) %>% 
    dplyr::arrange(location,horizon,target_variable,target_end_date,model,quantile) %>%
    tidyr::pivot_wider(names_from = model, values_from = value) %>%
    dplyr::select_if(~ !any(is.na(.)))
  return(matrix_frame)
} 


# new functions to plot forecasts to show day of the week effects

## plots single line w/ diff shapes for weekdays vs weekends
plot_day_of_week_effect <- function(point_forecasts, model_name) {
  ggplot(filter(point_forecasts, model == model_name)) + 
    geom_point(aes(x = target_end_date, y = agg_hosp, shape = day_type)) +
    geom_line(aes(x = target_end_date, y = agg_hosp)) +
    labs(title = paste(model_name, "Forecasts"))
}

plot_day_of_week_effect_color <- function(point_forecasts, model_name) {
  ggplot(filter(point_forecasts, model == model_name)) + 
    geom_point(aes(x = target_end_date, y = agg_hosp, color = day_type)) +
    geom_line(aes(x = target_end_date, y = agg_hosp)) +
    labs(title = paste(model_name, "Forecasts"))
}

## plots separate lines w/ diff color dots for weekdays vs weekends
plot_day_of_week_effect_sep <- function(point_forecasts, model_name) {
  ggplot(filter(point_forecasts, model == model_name), aes(group = day_type)) + 
    geom_point(aes(x = target_end_date, y = agg_hosp, color = day_type)) + 
    geom_line(aes(x = target_end_date, y = agg_hosp, linetype = day_type)) +
    labs(title = paste(model_name, "Forecasts"))  + 
    scale_x_date(date_labels = "%m-%y") + 
    theme(legend.position = "none")
}


# a function that takes a matrix and plot a heatmap
distance_heatmap_wk <- function(sum_dist,name,metadata=NULL){
  tmp <- sum_dist %>%
    dplyr::group_by(model_1,model_2) %>%
    dplyr::mutate(ov_mean=mean(mean_dis)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(short_hday, by=c("model_1"="model_abbr")) %>%
    dplyr::select(-"mean_dis") %>%
    dplyr::distinct(.,.keep_all = TRUE) %>%
    dplyr::arrange(desc(day_of_wk_effect))
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
    set1<-c("red","blue","purple")
    color_set <- ifelse(metadata$day_of_wk_effect == "FALSE", 
                        set1[1], 
                        ifelse(metadata$day_of_wk_effect ==  "TRUE",
                               set1[2],
                               set1[3]))
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


# a function that plots scatterplots to show mean CD over time
scatter_wk <-  function(data,title_name,metadata=NULL,smooth_tf=FALSE){
  dat <- data %>% 
    dplyr::left_join(metadata,by=c("model_2"="model_abbr")) %>%
    dplyr::mutate(Model=model_2) 
  if (is.null(metadata)) {
    dat <- data %>% 
      dplyr::mutate(Model=model_2)
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
    dat <- data %>% 
      dplyr::left_join(metadata,by=c("model_2"="model_abbr")) %>%
      dplyr::mutate(Model=model_2)
    
    ggplot(dat, aes(x=target_end_date, y=mean_approx_cd,
                    col=Model,group=interaction(Model, day_of_wk_effect),linetype=day_of_wk_effect)) + 
      geom_point(alpha=0.6,size=0.8) + 
      stat_smooth(alpha=0.4,size=0.5,aes(x = target_end_date, y = mean_approx_cd), method = "loess",
                  formula = y ~ x, se = FALSE) +
      scale_linetype_manual(values=c("solid", "dotted", "dashed", "dotdash")) +
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
    dat <- data %>% 
      dplyr::left_join(metadata,by=c("model_2"="model_abbr")) %>%
      dplyr::mutate(Model=model_2)
    
    ggplot(dat, aes(x=target_end_date, y=mean_approx_cd,
                    col=Model,group=interaction(Model, day_of_wk_effect),linetype=day_of_wk_effect)) + 
      #     geom_point(alpha=0.6,size=0.8) + 
      geom_line(alpha=0.4) +
      scale_linetype_manual(values=c("solid", "dotted", "dashed", "dotdash")) +
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



dendro_plot_wk <- function(horizon, frame_name,metadata){
  hclust_dat <- hclust(as.dist(get(frame_name)[[horizon]]), 
                       method = "ward.D", members = NULL)
  ddata <- label(dendro_data(as.dendrogram(hclust_dat))) %>%
    left_join(metadata,by=c("label"="model_abbr"))
  set1<-c("red","blue","pink","light blue", "purple")
  color_set <- ifelse(ddata$day_of_wk_effect == "FALSE", 
                      set1[1], 
                      ifelse(ddata$day_of_wk_effect ==  "TRUE",
                             set1[2],
                             ifelse(ddata$day_of_wk_effect == "ENS incl",
                                    set1[3], 
                                    ifelse(ddata$day_of_wk_effect == "ENS excl",
                                           set1[4],
                                           set1[5]))))
  # type_color <-  color_set[order(match(ddata$model_abbr,order_list))]
  dendro_p <- ggdendrogram(
    hclust(as.dist(get(frame_name)[[horizon]]), method = "ward.D",members = NULL)
    ,size = 2, rotate=TRUE) +
    labs(title=paste0(i, " wk ahead inc hosp"))+
    xlab("") +
    ylab("Mean Cramer's Distance") +
    theme(axis.text.x = element_text(size=5),
          axis.text.y = element_text(size=7, colour=color_set),
          plot.margin=unit(c(0,0,0,0),"cm"),
          plot.title = element_text(size=7)
    )
} 

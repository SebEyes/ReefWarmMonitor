temporal_autocor_plot = function(time_serie_data,
                                 variable,
                                 station_selected) {
  if (!is.null(time_serie_data)) {
    print(station_selected)
    
    if (station_selected != "Toutes les stations") {
      if (variable == "Température") {
        metric_selected = "mean_temp"
        metric_verbatim = "Autocorrélation temporelle de la température moyenne"
      } else{
        metric_selected = "mean_depth"
        metric_verbatim = "Autocorrélation temporelle de la profondeur moyenne"
      }
      
      TS_data = select(ungroup(time_serie_data), metric_selected)
      if (nrow(TS_data) > 0) {
        res_acf = acf(
          TS_data,
          lag.max = NULL,
          type = c("correlation", "covariance", "partial"),
          plot = FALSE,
          na.action = na.fail,
          demean = TRUE
        )
        
        df_res_acf = data.frame(acf = res_acf$acf, lag = res_acf$lag)
        
        ggplot(
          data = df_res_acf, 
          aes(
            x = lag, 
            y = acf
          )
        ) + 
          geom_bar(
            stat = "identity", 
            fill = "#049DBF"
          ) + 
          labs(
            x = "Lag",
            y = "ACF",
            title = metric_verbatim
          ) + 
          scale_x_discrete(
            limits = c(0:max(df_res_acf$lag))
          )  + theme_stata() +
          theme(
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold")
          )
      }
    }
  } else{
    NULL
  }
}
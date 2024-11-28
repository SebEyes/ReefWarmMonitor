temporal_autocor_plot = function(time_serie_data, variable, station_selected){
  
  if (variable == "Temperature") {
    metric_selected = "mean_temp"
    metric_verbatim = "Temporal auocorrelation mean temperature"
  }else{
    metric_selected = "mean_depth"
    metric_verbatim = "Temporal autocorrelation mean depth"
  }
  
  TS_data = select(ungroup(time_serie_data), metric_selected)
  
  res_acf = acf(TS_data, lag.max = NULL,
      type = c("correlation", "covariance", "partial"),
      plot = FALSE, na.action = na.fail, demean = TRUE)
  
  plot(res_acf, main = metric_verbatim)
  
}

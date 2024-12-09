temporal_selection = function(dataset, start, end, temporal_res){

  dataset_selected = dataset[dataset$timestamp >= start & dataset$timestamp <= end,] %>% na.omit()
  
  dataset_selected$date = date(dataset_selected$timestamp)
  dataset_selected$month_nb = month(dataset_selected$date)
  dataset_selected$year_verbatim = year(dataset_selected$date)
  
  if (temporal_res == "quotidien") {
    temp_data = dataset_selected %>% group_by(date) %>% summarize(
      station_name,
      min_depth = min(depth),
      max_depth = max(depth),
      mean_depth = mean(depth),
      med_depth = median(depth),
      min_temp = min(temperature),
      max_temp = max(temperature),
      mean_temp = mean(temperature),
      med_temp = median(temperature),
      sd_temp = sd(temperature),
      count_hour = n()
    )
  }
  
  if (temporal_res == "mensuel") {
    temp_data = dataset_selected %>% group_by(month_nb) %>% summarize(
      station_name,
      year_verbatim,
      min_depth = min(depth),
      max_depth = max(depth),
      mean_depth = mean(depth),
      med_depth = median(depth),
      min_temp = min(temperature),
      max_temp = max(temperature),
      mean_temp = mean(temperature),
      med_temp = median(temperature),
      sd_temp = sd(temperature),
      count_hour = n(),
    ) %>% unique()
    temp_data$date = paste(
      temp_data$year_verbatim,
      temp_data$month_nb,
      "01",
      sep = "/"
    )
    temp_data$date = as.POSIXct(temp_data$date, tz = "CET")
  }
  
  if (temporal_res == "annuel") {
    temp_data = dataset_selected %>% group_by(year_verbatim) %>% summarize(
      station_name,
      min_depth = min(depth),
      max_depth = max(depth),
      mean_depth = mean(depth),
      med_depth = median(depth),
      min_temp = min(temperature),
      max_temp = max(temperature),
      mean_temp = mean(temperature),
      med_temp = median(temperature),
      sd_temp = sd(temperature),
      count_hour = n()
    )
    temp_data$date = paste(
      temp_data$year_verbatim,
      "01",
      "01",
      sep = "/"
    )
    temp_data$date = as.POSIXct(temp_data$date, tz = "CET")
  }
  
  temp_data %>% unique() %>% ungroup() %>% relocate("date", .before = "station_name")
}

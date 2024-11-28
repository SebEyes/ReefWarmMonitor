## Function to remove the information at the tail of the file
require(stringr)
require(dplyr)

tail_cleaning = function(dataframe, character) {
  dataframe[str_detect(dataframe$Time, pattern = character, negate = T), ]
}

date_detection = function(values) {
  max_value = max(values)
  if (max_value > 31) {
    return("year")
  } else{
    if (max_value <= 12) {
      return("month")
    } else{
      return("day")
    }
  }
}

time_formatting = function(dataframe) {
  # Separate date and time
  new_data = separate(
    dataframe,
    "Time",
    into = c("date", "time"),
    remove = T,
    sep = " "
  )
  
  if (is.na(new_data$time[1])) {
    return(TRUE)
  }
  
  # Find separator for date
  if (str_detect(new_data$date, pattern = "-")[1]) {
    sep = "-"
  }
  if (str_detect(new_data$date, pattern = "/")[1]) {
    sep = "/"
  }
  
  # Separate date elements
  new_data = separate(
    new_data,
    "date",
    into = c("element_date1", "element_date2", "element_date3"),
    remove = F,
    sep = sep,
    convert = T
  )
  
  # Rename
  names(new_data)[2:4] = apply(new_data[, 2:4], MARGIN = 2, FUN = date_detection)
  new_data$date = paste(new_data$year, new_data$month, new_data$day, sep = "-")
  new_data = select(new_data, -c("year", "month", "day"))
  
  # Separate element time
  new_data = separate(
    new_data,
    "time",
    into = c("h", "m", "s"),
    sep = ":",
    remove = T
  )
  new_data$m = "00"
  new_data$s = "00"
  
  new_data$time = paste(new_data$h, new_data$m, new_data$s, sep = ":")
  new_data$Time = paste(new_data$date, new_data$time)
  
  new_data$Time = as.POSIXct(new_data$Time,
                             format = c("%Y-%m-%d %H:%M:%S"),
                             tz = "GMT")
  
  new_data %>% select("Time", "Temperature", "Depth")
}

temperature_outliers_detection = function(dataframe, lim_inf, lim_sup){
  lim_inf = as.numeric(lim_inf)
  lim_sup = as.numeric(lim_sup)
  output = dataframe %>% filter(!between(Temperature, lim_inf, lim_sup))
  output$Time = as.POSIXct(output$Time)
  output
}

depth_outliers_detection = function(dataframe, lim_inf, lim_sup){
  lim_inf = as.numeric(lim_inf)
  lim_sup = as.numeric(lim_sup)
  output = dataframe %>% filter(!between(Depth, lim_inf, lim_sup))
  output$Time = as.POSIXct(output$Time)
  output
}
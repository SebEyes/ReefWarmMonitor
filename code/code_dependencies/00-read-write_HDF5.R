# install.packages("BiocManager")
# BiocManager::install("rhdf5")

#Packages
require(rhdf5)

# HDF5 file overview
h5ls("data/data_station.h5")

## Read data
station_read = function(station_name){
  H5F_data = h5read(
    "data/data_station.h5",
    name = station_name
    )
  
  # Object data frame
  station_df = data.frame(
    depth = H5F_data$depth,
    temperature = H5F_data$temperature_c,
    timestamp = H5F_data$timestamp,
    station_name = station_name
  )
  
  # formatting data
  station_df$temperature = round(station_df$temperature, digits = 2)
  station_df$depth = round(station_df$depth, digits = 2)
  station_df$timestamp = as.POSIXlt(station_df$timestamp,format="%Y/%m/%d %H:%M:%S", tz = "CET")
  
  station_df
}

## Write data
station_write = function(station_name, dataset){
  print(h5ls("data/data_station.h5"))
  #Remove dataset from HDF5 file
  h5delete("data/data_station.h5", paste(station_name, "/depth", sep =""))
  h5delete("data/data_station.h5", paste(station_name, "/temperature_c", sep =""))
  h5delete("data/data_station.h5", paste(station_name, "/timestamp", sep =""))
  
  
  # Write new dataset to HDF5
  h5write(dataset$depth, "data/data_station.h5", paste(station_name, "/depth", sep =""))
  h5write(dataset$temperature, "data/data_station.h5", paste(station_name, "/temperature_c", sep =""))
  h5write(dataset$timestamp, "data/data_station.h5", paste(station_name, "/timestamp", sep =""))
  print(h5ls("data/data_station.h5"))
}

## Remove data
station_remove = function(){
  
  historic <- read.csv(
    "data/historic.csv",
    sep = ";",
    check.names = F
  )
  
  data_station = station_read(head(historic$`Station ajoutée`,1))
  
  start_time = as.POSIXlt(head(historic$"Début série temporelle",1),format="%Y/%m/%d %H:%M:%S", tz = "CET")
  end_time = as.POSIXlt(head(historic$"Fin série temporelle",1),format="%Y/%m/%d %H:%M:%S", tz = "CET")
  
  data_station_early = data_station %>% filter(timestamp < start_time)
  data_station_late = data_station %>% filter(timestamp > end_time)
  
  data_station = rbind(data_station_early, data_station_late)
  
  data_station$timestamp = format(data_station$timestamp,"%Y/%m/%d %H:%M:%S")
  
  station_write(
    head(historic$`Station ajoutée`,1),
    data_station
  )
  

  #update history
  historic = historic[-1,]
  
  write.table(
    historic,
    "data/historic.csv",
    sep = ";",
    fileEncoding = "UTF8",
    row.names = F
  )
}
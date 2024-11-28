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
  station_df$timestamp = as.POSIXlt(station_df$timestamp,format="%Y/%m/%d %H:%M:%S")
  
  station_df
}

## Write data
station_write = function(station_name, dataset){
  
  #Remove dataset from HDF5 file
  h5delete("data/data_station.h5", paste(station_name, "/depth", sep =""))
  h5delete("data/data_station.h5", paste(station_name, "/temperature_c", sep =""))
  h5delete("data/data_station.h5", paste(station_name, "/timestamp", sep =""))
  
  # Write new dataset to HDF5
  h5write(dataset$depth, "data/data_station.h5", paste(station_name, "/depth", sep =""))
  h5write(dataset$temperature, "data/data_station.h5", paste(station_name, "/temperature_c", sep =""))
  h5write(dataset$timestamp, "data/data_station.h5", paste(station_name, "/timestamp", sep =""))
}
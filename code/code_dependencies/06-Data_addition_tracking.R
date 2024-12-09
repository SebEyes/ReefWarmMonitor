data_history = function(file_data_added, flag_data){
  
  old_historic <- read.csv(
    "data/historic.csv",
    sep = ";",
    check.names = F
  )
  
  new_history = rbind(
    data.frame(
      "Timestamp ajout" = format(Sys.time(), "%a %b %d %X %Y"),
      "Station ajoutée" = unique(file_data_added$station_name),
      "Début série temporelle" = format(head(file_data_added$timestamp,1), "%Y/%m/%d %H:%M:%S"),
      "Fin série temporelle" = format(tail(file_data_added$timestamp,1), "%Y/%m/%d %H:%M:%S"),
      "Nombre de données ajoutée" = as.integer(nrow(file_data_added)),
      "Présence de données abérrentes" = flag_data,
      check.names = F
    ),
    old_historic
  )
  
  write.table(
    new_history,
    "data/historic.csv",
    sep = ";",
    fileEncoding = "UTF8",
    row.names = F
  )
  print("history tracked")
}
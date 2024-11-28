require(ggplot2)
require(ggthemes)
require(reshape2)

# options(repr.plot.width = 2, repr.plot.height =3) 

graph_TS = function(data_temp, variable) {
  # select data
  if (variable == "Température") {
    data_temp = select(data_temp,
                       c(
                         "date",
                         "station_name",
                         "min_temp",
                         "max_temp",
                         "mean_temp"
                       ))
    data_temp = data_temp %>% rename("min" = "min_temp",
                                     "max" = "max_temp",
                                     "mean" = "mean_temp")
    
    y_label = "Température moyenne (°C)"
    
  } else{
    data_temp = select(data_temp,
                       c(
                         "date",
                         "station_name",
                         "min_depth",
                         "max_depth",
                         "mean_depth"
                       ))
    data_temp = data_temp %>% rename("min" = "min_depth",
                                     "max" = "max_depth",
                                     "mean" = "mean_depth")
    
    y_label = "Profondeur moyenne (m)"
  }
  
  TS_plot = ggplot(data = data_temp,
         aes(
           x = date,
           y = mean,
           ymin = min,
           ymax = max,
           color = station_name
         )) + labs(x = "Temps", y = y_label, color = "Station: ") + geom_rect(
           ymin = 29,
           ymax = Inf,
           xmin = -Inf,
           xmax = Inf,
           fill = 'pink'
         )  + geom_linerange() + geom_pointrange() + geom_line() + theme_stata() +
    theme(plot.caption = element_text(size = 10),
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold")
      )
  
  
  if (variable == "Température") {
    TS_plot + labs(caption = "INFO : La zone rouge représente une température critique supérieure à 29°C")
  }else{
    TS_plot
  }
  
}

graph_preview = function(dataframe) {
  data_melt = melt(dataframe, id.vars = "Time")
  data_melt$date = date(data_melt$Time)
  data_melt$value = as.numeric(data_melt$value)
  data_melt = data_melt %>%
    group_by(date, variable) %>%
    summarise(mean = mean(value),
              max = max(value),
              min = min(value))
  data_melt[, 3:5] = apply(data_melt[, 3:5], MARGIN = 2, FUN = as.numeric)
  
  #outliers
  data_melt$outlier = FALSE
  data_melt$outlier[data_melt$variable == "Temperature" &
                      data_melt$min <= 25] = TRUE
  data_melt$outlier[data_melt$variable == "Temperature" &
                      data_melt$max >= 35] = TRUE
  
  
  data_melt$outlier[data_melt$variable == "Depth" &
                      data_melt$min <= 2] = TRUE
  data_melt$outlier[data_melt$variable == "Depth" &
                      data_melt$max >= 10] = TRUE
  
  
  ggplot(data = data_melt, aes(
    x = date,
    y = mean,
    ymin = min,
    ymax = max,
    color = outlier
  )) + geom_line(mapping = aes(group = 1)) +
    labs(x = "Temps", y = "Valeur journalière moyenne") + guides(color = FALSE) +
    scale_color_manual(values = c("black", "red")) +
    geom_rect(
      ymin = 29,
      ymax = Inf,
      xmin = -Inf,
      xmax = Inf,
      fill = 'pink'
    )  + geom_linerange() + geom_pointrange()  + theme_stata() +
    facet_grid(scales = "free_y", rows = vars(variable)) +
    ggtitle(label = "Séries temporelles")+ labs(caption = "Les valeurs aberrantes sont indiquées en rouge, le cas échéant.") + theme(plot.subtitle = element_text(color = "black", face = "bold"))
  
}
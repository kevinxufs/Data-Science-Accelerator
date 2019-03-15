site_info <- read.csv('defra_site_info.csv')
colnames(site_info)[4] <- "Location"

x <- c('Camden Kerbside', 'London Bloomsbury', 'Ealing Horn Lane', 'London Harlington', 'London Hillingdon')

site_info<-filter(site_info, Location %in% x )


weather_data <- read.csv('weather_data.csv')

bus_stops <- read.csv('london_bus_stops.csv')

# weather_data <- merge(weather_data, site_info, by = 'Location')


i =1

while (i <= nrow(pollution_set)) {
  
  lat = pollution_set$latitude[i]
  long = pollution_set$longitude[i]
  
  dist_results = list()
  j=1
 
 

  while (j <= nrow(site_info)){
    
    loc <- as.character(site_info$Location[j] )
    lat2 = site_info$Latitude[j]
    long2 = site_info$Longitude[j]
    dist = distm(c(as.numeric(long), as.numeric(lat)), c(as.numeric(long2), as.numeric(lat2)), fun = distHaversine)
    dist_results[[loc]] = dist
    j<-j+1
    
  }
  val = dist_results[which.min(dist_results)][[1]][1]
  name = names(dist_results)[which.min(dist_results)]
  pollution_set[i, 'closest_defra_site'] = name
  pollution_set[i, 'dist_to_closest_defra_site'] = floor(val)
  pollution_set[i, 'Location'] = name

  i <- i+1
  
}



i =1

while (i <= nrow(pollution_set)) {
  
  lat = pollution_set$latitude[i]
  long = pollution_set$longitude[i]
  
  dist_bus_results = list()
  j=1
  print(i)
  
  
  while (j <= nrow(bus_stops)){
    
    loc2 <- as.character(bus_stops$Bus_Stop_Code[j] )
    lat2 = bus_stops$Latitude[j]
    long2 = bus_stops$Longitude[j]
    dist = distm(c(as.numeric(long), as.numeric(lat)), c(as.numeric(long2), as.numeric(lat2)), fun = distHaversine)
    dist_bus_results[[loc2]] = dist
    j<-j+1
    
  }
  val = dist_bus_results[which.min(dist_bus_results)][[1]][1]
  name = names(dist_bus_results)[which.min(dist_bus_results)]
  pollution_set[i, 'closest_bus_stop'] = name
  pollution_set[i, 'dist_to_closest_bus_stop'] = floor(val)

  i <- i+1
  
}











str(pollution_set)
colnames(site_info)



pollution_data <- select (pollution_data,-c(total_stations, total_traffic))

pollution_data2 <- merge(pollution_data, pollution_set[, c('monitor_ref', 'Location',
                                                           'closest_train_station', 'dist_to_closest_train_station', 
                                                           "total_train_stations","total_train_traffic" , "closest_defra_site" ,
                                                          "closest_bus_stop",   "dist_to_closest_bus_stop" 
                                                           )], by = 'monitor_ref')


colnames(weather_data)[1] = 'date'
colnames(weather_data)[2] = 'time'

pollution_data3 <- merge(pollution_data2, weather_data, by = c('Location', 'date', 'time'))  

colnames(pollution_data4)

colnames(pollution_data3)[5] = 'monitor_latitude'
colnames(pollution_data3)[6] = 'monitor_longitude'


pollution_data3 <- merge(pollution_data3, site_info, by = 'Location')

colnames(pollution_data3)[37] = 'defra_site_latitude'
colnames(pollution_data3)[38] = 'defra_site_longitude'

pollution_data3 <- pollution_data3 %>%
  mutate(Bus_Stop_Code = closest_bus_stop)


pollution_data3 <- merge(pollution_data3, bus_stops, by = 'Bus_Stop_Code')

colnames(pollution_data3)[53] = 'closest_bus_latitude'
colnames(pollution_data3)[54] = 'closest_bus_longitude'

pollution_data4 <- pollution_data3

saveRDS(pollution_data4, 'combined_pollution_data.rds')

pollution_data3air <- pollution_data3%>%
  filter(pollution_type == 'air')



colnames(pollution_data3air)

weather_mlset <- pollution_data3air %>%
  dplyr::select('pollution_level', 'hour_time', "Modelled.Temperature", "Environment.Type", "Modelled.Wind.Speed", 'monitor_ref' )

weather_mlset2 <- pollution_data3air %>%
  dplyr::select('pollution_level', 'hour_time', "Modelled.Temperature", "Modelled.Wind.Speed")

saveRDS(pollution_set, 'pollution_set.rds')


pollution_set <- readRDS('pollution_set.rds')



combined_pollution_data <- readRDS('combined_pollution_data.rds')



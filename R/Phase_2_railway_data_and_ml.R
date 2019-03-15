# STEP 2: adding railway data 


# Functions and data prep libraries ---------------------------------------------------------------

library(tidyr)
library(geosphere)

#Will be used to convert latitude and longitude columns into the correct format for calculations
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# Producing a railway dataset with lat longs and traffic -------------------

train_traffic <- read.csv('train_traffic.csv')
colnames(train_traffic)[3] <- "station_name"
colnames(train_traffic)[2] <- 'station_code'

london_railway <- read.csv('London_railway_wlatlong.csv', col.names = c("station_name", 'local_authority', 'managed_by', 'station_code', 'fare_zone', 'year_opened', 'category', 'coordinates'))

#Combining the datasets by station_code
railway <- merge(train_traffic, london_railway, by = 'station_code')

#Fixing up the coordinates data to form seperate latitude and longitude columns

railway <- separate(data = railway, col = coordinates, into = c('latitude', 'longitude'), sep = "\\ ")

railway$latitude <- substr(railway$latitude, 1, nchar(railway$latitude)-2)

railway$longitude <- as.character(railway$longitude)


railway <- railway %>%
  mutate(longitude = ifelse(grepl('W', longitude),
                            0 - as.numeric(substr(longitude, 1, nchar(longitude)-2)), 
                            as.numeric(substr(longitude, 1, nchar(longitude)-2))))

railway_matrix <- railway %>%
  dplyr::select('latitude', 'longitude', 'station_code', 'Station.Facility.Owner', 'X1718.Entries...Exits' )%>%
  dplyr::rename(traffic = X1718.Entries...Exits)%>%
  mutate(traffic = as.numeric(gsub(',', '', traffic)))

pollution_set <- pollution_data %>%
  dplyr::select('latitude', 'longitude', 'pollution_type', 'monitor_ref')%>%
  unique()


# Calculating nearby stations and traffic ---------------------------------


# 1 mile
distance = 1609
i =1

while (i <= nrow(pollution_set)) {
  
  lat = pollution_set$latitude[i]
  long = pollution_set$longitude[i]
  n_stations = 0
  tot_traffic = 0
  dist_train_results = list()
  j =1
  while (j <= nrow(railway_matrix)){
    
    loc1 <- as.character(railway_matrix$station_code[j])
    lat2 = railway_matrix$latitude[j]
    long2 = railway_matrix$longitude[j]
    dist = distm(c(as.numeric(long), as.numeric(lat)), c(as.numeric(long2), as.numeric(lat2)), fun = distHaversine)
    dist_train_results[[loc1]] = dist
    
    if (dist < distance){
      n_stations <- n_stations+1
      tot_traffic = tot_traffic + railway_matrix$traffic[j]
      
      
    }
    j=j+1
    
    
    
  }
  val = dist_train_results[which.min(dist_train_results)][[1]][1]
  name = names(dist_train_results)[which.min(dist_train_results)]
  pollution_set[i, 'closest_train_station'] = name
  pollution_set[i, 'dist_to_closest_train_station'] = floor(val)
  pollution_set[i, 'total_train_stations'] = n_stations
  pollution_set[i, 'total_train_traffic'] = tot_traffic
  i <- i+1
  
}


# Refined datasets --------------------------------------------



pollution_data <- merge(pollution_data, pollution_set[,c('monitor_ref', 'total_stations', 'total_traffic')], by = 'monitor_ref', all.x = TRUE, all.y = FALSE)

pollution_data_air <- pollution_data[pollution_data$pollution_type == 'air', ]

mlset <- pollution_data_air%>%
  dplyr::select('pollution_level', 'total_stations', 'total_traffic', 'is_weekend', 'is_working_hour')

saveRDS(mlset, 'mlset.rds')
# Data to be used for ML

mlset <- readRDS('mlset.rds')





pollution_set <- pollution_data %>%
  dplyr::select('latitude', 'longitude', 'pollution_type', 'monitor_ref')%>%
  unique()














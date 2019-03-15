combined_pollution_data_noise <- filter(combined_pollution_data, pollution_type == 'noise')

combined_pollution_data_noise <- combined_pollution_data_noise%>%
  mutate(borough = as.factor(strsplit(as.character(lsoa11nm), ' ')[[1]][1]))

summary(combined_pollution_data_noise)

column_to_use <- c('hour_time', 
                   'pollution_level', 
                   'day_of_week',
                   'dist_to_closest_train_station', 
                   'total_train_stations',
                   'total_train_traffic',
                   'monitor_ref',
                   'lsoa11nm',
                   'dist_to_closest_bus_stop',
                   'Modelled.Wind.Direction',
                   'Modelled.Wind.Speed',
                   'Modelled.Temperature',
                   'Environment.Type',
                   'Altitude..m.',
                   'month',
                   'season',
                   'bank_holiday',
                   'is_working_hour',
                   'date')

a <- strsplit('Camden 023B', ' ')

a[[1]][1]


  
pure_combined_data_noise <- combined_pollution_data_noise[, column_to_use]

summary(pure_combined_data_noise)

pure_combined_data_noise1 <- mutate_if(pure_combined_data_noise, is.character, as.factor)

summary(pure_combined_data_noise)


pure_combined_data_noise1 <- pure_combined_data_noise1 %>%
  mutate(is_working_hour = ifelse(is_working_hour == 'Yes', 1, 0) )%>%
  mutate(total_train_stations = as.factor(total_train_stations))%>%
  mutate(day_of_week = as.factor(day_of_week))%>%
  mutate(month = as.factor(month))%>%
  mutate(season = as.factor(season))%>%
  mutate(hour_time = as.factor(hour_time))%>%
  mutate(dist_to_closest_train_station = as.factor(case_when(dist_to_closest_train_station <= quantile(dist_to_closest_train_station, c(.25), na.rm = TRUE) ~ '_low',
                                                             pollution_level<= quantile(dist_to_closest_train_station, c(.5), na.rm = TRUE) ~ '_medium',
                                                             pollution_level<= quantile(dist_to_closest_train_station, c(.75), na.rm = TRUE) ~ '_high',
                                                             TRUE ~ '_very_high')))%>%
  mutate(dist_to_closest_bus_stop = as.factor(case_when(dist_to_closest_bus_stop <= quantile(dist_to_closest_bus_stop, c(.25), na.rm = TRUE) ~ '_low',
                                                        dist_to_closest_bus_stop<= quantile(dist_to_closest_bus_stop, c(.5), na.rm = TRUE) ~ '_medium',
                                                        dist_to_closest_bus_stop<= quantile(dist_to_closest_bus_stop, c(.75), na.rm = TRUE) ~ '_high',
                                                        TRUE ~ '_very_high')))%>%
  mutate(Modelled.Wind.Direction = as.factor(case_when(between(Modelled.Wind.Direction, 0, 90) ~ 'NE',
                                                       between(Modelled.Wind.Direction, 90, 180) ~ 'SE',
                                                       between(Modelled.Wind.Direction,  180, 270) ~ 'SW',
                                                       between(Modelled.Wind.Direction, 270,360) ~ 'NW')))%>%
  mutate(Modelled.Wind.Speed = as.factor(case_when(Modelled.Wind.Speed <= quantile(Modelled.Wind.Speed, c(.25), na.rm = TRUE) ~ '_low',
                                                   Modelled.Wind.Speed<= quantile(Modelled.Wind.Speed, c(.5), na.rm = TRUE) ~ '_medium',
                                                   Modelled.Wind.Speed<= quantile(Modelled.Wind.Speed, c(.75), na.rm = TRUE) ~ '_high',
                                                   TRUE ~ '_very_high')))%>%
  mutate(Modelled.Temperature = as.factor(case_when(Modelled.Temperature <= quantile(Modelled.Temperature, c(.25), na.rm = TRUE) ~ '_low',
                                                    Modelled.Temperature<= quantile(Modelled.Temperature, c(.5), na.rm = TRUE) ~ '_medium',
                                                    Modelled.Temperature<= quantile(Modelled.Temperature, c(.75), na.rm = TRUE) ~ '_high',
                                                    TRUE ~ '_very_high')))%>%
  mutate(Altitude..m. = as.factor(case_when(Altitude..m. <= quantile(Altitude..m., c(.25), na.rm = TRUE) ~ '_low',
                                            Altitude..m.<= quantile(Altitude..m., c(.5), na.rm = TRUE) ~ '_medium',
                                            Altitude..m.<= quantile(Altitude..m., c(.75), na.rm = TRUE) ~ '_high',
                                            TRUE ~ '_very_high')))%>%
  mutate(total_train_traffic = as.factor(case_when(total_train_traffic <= quantile(total_train_traffic, c(.25), na.rm = TRUE) ~ '_low',
                                                   total_train_traffic<= quantile(total_train_traffic, c(.5), na.rm = TRUE) ~ '_medium',
                                                   total_train_traffic<= quantile(total_train_traffic, c(.75), na.rm = TRUE) ~ '_high',
                                                   TRUE ~ '_very_high')))

results_noise <- fastDummies::dummy_cols(pure_combined_data_noise1, remove_first_dummy =  TRUE)  

summary(pure_combined_data_noise1)

colnames(results_noise)

pure_combined_data_noise1 <- pure_combined_data_noise1%>%
  mutate(is_exceed = as.factor(is_exceed))

colnames(results_noise)

remove_list <- c('season', 
                 'month', 
                 'Altitude..m.',
                 'Modelled.Temperature',
                 'Modelled.Wind.Speed',
                 'Modelled.Temperature',
                 'Modelled.Wind.Direction',
                 'dist_to_closest_bus_stop',
                 'total_train_traffic',
                 'total_train_stations',
                 'dist_to_closest_train_station',
                 'day_of_week',
                 'hour_time',
                 'date',
                 'is_exceed_exceed',
                 'pollution_level',
                 'Environment.Type',
                 'lsoa11nm',
                 'monitor_ref'
)

pure_dummy_noise <- results_noise%>%
  dplyr::select(-one_of(remove_list))

colnames(pure_dummy_noise)


weekday_list <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
weekend_list <- c('Saturday', 'Sunday')
any <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

day_list <- list(weekday_list, 
                 weekday_list,
                 weekday_list, 
                 weekday_list, 
                 list('Saturday'),
                 list('Saturday'),
                 list('Saturday'),
                 list('Saturday'),
                 list('Sunday'),
                 any)
                
                 
time_list <- list(list(7), 
                  list(8:17), 
                  list(18), 
                  list(19:21), 
                  list(7),
                  list(8:12), 
                  list(13),
                  list(14:21), 
                  list(7:21), 
                  list(0:6, 22, 23 ) )

averaging_period <- list(1,10, 1, 1, 1, 5, 1, 1, 1, 1)

insulation_limit <- list(70, 75, 70, 65, 70, 75, 70, 65, 65, 55)

rehousing_limit <- list(80, 85, 80, 75, 80, 85, 80, 75, 75, 65)



noise_lims <- read.csv('noiselims.csv')
colnames(noise_lims)

summary(combined_pollution_data_noise$hour_time)

x <- noise_lims %>%
  filter(day == 'Monday' & hour == 2)

x$insu_lim

y <- noise_limit(55, 'Monday', 4, 1)

y

summary(pure_combined_data_noise1$pollution_level)

rows_to_watch <- c()

i <- 1

noise_limit <- function(pollution_level, day_of_week, hours, bankholiday){
  
  # pollution_level = 61.2
  # day_of_week = 'Saturday'
  # hours = 18
  # bankholiday = 0
  # 

  if (bankholiday == 1){
    day_of_week = 'Sunday'
  }
  x <- noise_lims %>%
    filter(day == day_of_week & hour == hours)
  
 
  
  if (pollution_level > x$insu_lim){
    return('exceed')
  } else {
    return('not_exceed')
  }
  
}
colnames(pure_combined_data_noise1)

summary(pure_combined_data_noise1$is_exceed)

summary(noise_lims)

group_by(noise_lims, day, hour)%>%
  dplyr::summarise(count=n())%>%
  dplyr::arrange(desc(count))


i = 1


pure_combined_data_noise1 <- pure_combined_data_noise1%>%
  mutate(is_exceed = as.factor(is_exceed))


pure_combined_data_noise1 <- filter(pure_combined_data_noise1, !is.na(pollution_level))

i <- 1

summary(pure_combined_data_noise1$is_exceed)

pure_combined_data_noise1%>%
  group_by(is_exceed)%>%
  dplyr::summarise(count=n()) %>% 
  mutate(percent = count / sum(count))

pure_combined_data_noise1$is_exceed <- apply(pure_combined_data_noise1, 1, function(row){
  
  pollution_level = as.numeric(row['pollution_level'])
  day_of_week = as.character(row['day_of_week'])
  hours = as.numeric(row['hour_time'])
  bankholiday = as.numeric(row['bank_holiday'])
  
  noise_limit(pollution_level, day_of_week, hours, bankholiday)
  
  
  
  
 
  
  
})


pure_combined_data_noise1[sample(1:nrow(pure_combined_data_noise1), 10),]

while (i <= nrow(pure_combined_data_noise1)){
  pure_combined_data_noise1[i, 'is_exceed'] = noise_limit(pure_combined_data_noise1[i, "pollution_level"],
                                                          pure_combined_data_noise1[i, "day_of_week"],
                                                          pure_combined_data_noise1[i, "hour_time"],
                                                          pure_combined_data_noise1[i, "bank_holiday"])
  i= i+1
  if (i%%1000 == 0){
    print(i)}
  
}

colnames(pure_combined_data_noise1)










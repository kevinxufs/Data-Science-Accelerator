combined_pollution_data <- readRDS('combined_pollution_data.rds')

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Autumn Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Autumn")))
}



combined_pollution_data <- combined_pollution_data %>%
  mutate(month = format(date,"%m"))%>%
  mutate(season = getSeason(date))%>%
  mutate(bank_holiday = 0)

combined_pollution_data <- subset(combined_pollution_data, select =-bank_holiday)

colnames(combined_pollution_data)


ws <- as.Date("2012-12-15", format = "%Y-%m-%d")
ax <- as.Date("2012-12-15", format = "%Y-%m-%d")


# Will need to update if we ever get more data
bank_holidays <- c("2017-11-05", "2017-11-12", "2017-12-25", "2017-12-26", "2018-01-01", "2018-03-30", "2018-04-02", "2018-05-07", "2018-05-28",
               "2018-08-27", "2018-12-25", "2018-12-26")

bank_holidays_list <- list()


for (i in bank_holidays) {
  
  bank_holidays[i] <- as.Date(i, format = "%Y-%m-%d")
}

x <- as.Date("2017-11-05", format = "%Y-%m-%d")

bank_holiday_check <- function(date) {
  if (date %in% bank_holidays) {
    return(1)
    
  } else
    {
    return(0)}
  }
  




i = 1

while (i <= nrow(combined_pollution_data)){
  combined_pollution_data[i, 'bank_holiday'] = bank_holiday_check(combined_pollution_data[i, 'date'])
  i= i+1
  if (i%%1000 == 0){
    print(i)}
  
}



combined_pollution_data%>%
  group_by(bank_holiday)%>%
  dplyr::summarise(count = n())

colnames(combined_pollution_data)

saveRDS(combined_pollution_data, 'combined_pollution_data.rds')

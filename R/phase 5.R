# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
library(knitr)
library(fastDummies)
library(plotly)
install.packages('fastDummies')


combined_pollution_data_air <- filter(combined_pollution_data, pollution_type == 'air')

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(pollution_level~., data=results, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)

summary(combined_pollution_data_air)
colnames(combined_pollution_data_air)


combined_pollution_data_air <- mutate_if(combined_pollution_data_air, is.character, as.factor)

summary(combined_pollution_data_air)

knitr::kable(combined_pollution_data_air)

results <- fastDummies::dummy_cols(combined_pollution_data_air, remove_first_dummy =  TRUE)

knitr::kable(results)


colnames(results)

colnames(combined_pollution_data_air)

x <- combined_pollution_data_air[, c('')]

y <- combined_pollution_data_air

z <- y[sapply(y, function(y) is.factor(y))]
z <- y[sapply(y, function(y) is.character(y))]
colnames(z)  

colnames(combined_pollution_data_air)

column_to_use <- c('hour_time', 
                   'pollution_level', 
                   'day_of_week',
                   'dist_to_closest_train_station', 
                   'total_train_stations',
                   'total_train_traffic',
                  
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
                   
                   

pure_combined_data_air <- combined_pollution_data_air[, column_to_use]

pure_combined_data_air <- pure_combined_data_air %>%
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
# 
x <- quantile(pure_combined_data_air$pollution_level, c(.25, .5, .75), na.rm = TRUE)
x[1]

summary(pure_combined_data_air)

pure_combined_data_air <- pure_combined_data_air%>%
  filter(!is.na(pollution_level))%>%
  
  dplyr::group_by(date, hour_time)%>%
  # mutate(pollution_level = ifelse(pollution_level > 50, 'exceed', 'not exceed'))%>%
  mutate(pollution_level = as.factor(case_when(pollution_level > 50 ~ '_exceed',
                                               pollution_level< x[1] ~ '_low',
                                               pollution_level< x[2] ~ '_medium',
                                               pollution_level< x[3] ~ '_high',
                                            
                                               TRUE ~ '_very_high')))%>%
  ungroup()
  
table(pure_combined_data_air$pollution_level)


pure_combined_data_air3 <- combined_pollution_data_air%>%
  filter(!is.na(pollution_level))%>%
  dplyr::group_by(date, monitor_ref)%>%
  dplyr::summarise(pollution_level = mean(pollution_level, na.rm = TRUE))%>%
  filter(pollution_level > 50)


pure_combined_data_air4 <- combined_pollution_data_air%>%
  filter(!is.na(pollution_level))%>%
  dplyr::group_by(date, hour_time, monitor_ref)%>%
  mutate(pollution_level = case_when())





summary(pure_combined_data_air3)

summary(combined_pollution_data_air)

group_by(pure_combined_data_air, pollution_level)%>%
  summarise(count=n())


pure_combined_data_air%>%
  group_by(pollution_level)%>%
  dplyr::summarise(count=n()) %>% 
  mutate(percent = count/sum(count))

pure_combined_data_air <- pure_combined_data_air%>%
  dplyr::select(pollution_level, everything())
  
           
    

  
  

colnames(pure_combined_data_air)  
summary(pure_combined_data_air)  

colnames(results)


results <- fastDummies::dummy_cols(pure_combined_data_air, remove_first_dummy =  TRUE)  
  

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
                 'pollution_level__low',
                 'pollution_level__high',
                 'pollution_level__medium',
                 'pollution_level__exceed'
                )

pure_dummy_air <- results%>%
  dplyr::select(-one_of(remove_list))

colnames(pure_dummy_air)  

pure_dummy_air <- pure_dummy_air %>%
  mutate(Environment.Type = ifelse(Environment.Type == 'Background Urban', 1, 0))%>%
  dplyr::rename(Background_urban = Environment.Type)


# load the data
data(combined_pollution_data)
# calculate correlation matrix
correlationMatrix <- cor(pure_dummy_air[,2:ncol(pure_dummy_air)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

remove_again <- colnames(pure_dummy_air)[highlyCorrelated+1]

pure_dummy_air2 <- pure_dummy_air%>%
  dplyr::select(-one_of(remove_again))%>%
  mutate(pollution_level = gsub(' ', '_', pollution_level))

  
correlationMatrix <- cor(pure_dummy_air2[,2:ncol(pure_dummy_air2)])



# training ----------------------------------------------------------------

index <- createDataPartition(pure_dummy_air2$pollution_level, p=0.75, list=FALSE)
trainSet <- pure_dummy_air2[ index,]
testSet <- pure_dummy_air2[-index,]  

trainSet%>%
  group_by(pollution_level)%>%
  dplyr::summarise(count=n())

fit_control <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 2,
  # classProbs = TRUE,
  verboseIter = TRUE)



levels(trainSet$pollution_level) <- c("exceed", "not exceed")

reg_fit <- train(pollution_level ~ ., 
                 data = trainSet, 
                 method = "LogitBoost",
                 # tuneLength = 5,
                 trControl = fit_control)

reg_fit

prediction = predict(reg_fit, newdata = testSet, type = 'prob')

prediction <- prediction%>%
  mutate(mod_guess = ifelse(not_exceed < 0.9895, 'exceed', 'not_exceed') )



pure_dummy_air2 <- pure_dummy_air%>%
  dplyr::select(-one_of(remove_again))%>%
  mutate(pollution_level = gsub(' ', '_', pollution_level))

cm <- confusionMatrix(as.factor(prediction2), as.factor(testSet$pollution_level))

unique(testSet$pollution_level)
unique(prediction2)

testSet <- testSet %>%
  mutate(pollution_level = as.factor(pollution_level))





cm
cm$byClass

table(prediction2)


naive_bayes_fit <- train(pollution_level ~ ., 
                 data = trainSet, 
                 method = "naive_bayes",
                 # tuneLength = 5,
                 trControl = fit_control)

svm_fit <- train(pollution_level ~ ., 
                 data = trainSet, 
                 method = "svmLinear",
                 # tuneLength = 5,
                 trControl = fit_control)



knn_fit <- train(pollution_level ~ ., 
                         data = trainSet, 
                         method = "knn",
                         # tuneLength = 5,
                         trControl = fit_control)


svm_fit

knn_fit

prediction = predict(svm_fit, newdata = testSet, type = 'prob')

prediction2 <- predict(knn_fit, newdata = testSet, type = 'prob')





prediction %>%
  mutate(mod_guess = ifelse(not_exceed < 0.9895, 'exceed', 'not_exceed') )%>%
  group_by(mod_guess)%>%
  dplyr::summarise(count=n())

p <- plot_ly(data = combined_pollution_data_air, x = ~pollution_level, type = 'histogram')
p
plot_ly(data= combined_pollution_data_air,
        x = ~date, 
        y = ~pollution_level, 
        type = 'scatter', 
        mode = 'markers', 
        marker = list(color = 'rgba(128, 128, 128, 0.5)'))%>%
  layout(shapes = list(
    type = 'line',
    y0 = 50,
    y1 = 50,
    x0 = 0,
    x1 = 1,
    xref = 'paper'
    
  ))

summary(combined_pollution_data_air)







# roc curve, 0.7 rf or gradient boosting
# same location same weekday, averages. Area - borough maybe, over sampling and undersampling, 
# Stronger models, binary prediction.
# practical machine learning in R John Hopkins

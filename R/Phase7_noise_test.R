setwd("~/Desktop/Accelerator")
library(caret)

saveRDS(pure_dummy_noise, 'pure_dummy_noise.rds')
pure_dummy_noise <- readRDS('pure_dummy_noise.rds')

index <- createDataPartition(pure_dummy_noise$is_exceed, p=0.75, list=FALSE)
trainSet <- pure_dummy_noise[ index,]
testSet <- pure_dummy_noise[-index,]  



trainSet%>%
  group_by(is_exceed)%>%
  dplyr::summarise(count=n())
colnames(trainSet)

fit_control <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 2,
  classProbs = TRUE,
  verboseIter = TRUE)


svm_fit <- train(is_exceed ~ ., 
                 data = trainSet, 
                 method = "svmLinear",
                 # tuneLength = 5,
                 trControl = fit_control)


log_fit <- train(is_exceed ~ ., 
                 data = trainSet[sample(1:nrow(trainSet),10000), !grepl('monitor_ref|lsoa', colnames(trainSet)) ], 
                 method = "LogitBoost",
                 # tuneLength = 5,
                 trControl = fit_control,
                 importance = TRUE)


logit_model <- train(is_exceed ~ ., 
                 data = trainSet, 
                 method = "LogitBoost",
                 # tuneLength = 5,
                 trControl = fit_control
                 )

knn_noise_fit <- train(is_exceed ~ ., 
                       data = trainSet[sample(1:nrow(trainSet), 500),], 
                       method = "knn",
                       # tuneLength = 5,
                       trControl = fit_control,
                       importance = 'permutation')

knn_noise_fit_big <- train(is_exceed ~ ., 
                       data = trainSet, 
                       method = "knn",
                       # tuneLength = 5,
                       trControl = fit_control)



rf_fit  <- train(is_exceed ~ ., 
                           data = trainSet, 
                           method = "ranger",
                           # tuneLength = 5,
                           trControl = fit_control,
                 importance = 'permutation'
                           )

rf_model_unbias <- train(is_exceed ~ ., 
                            data = trainSet[, !grepl('monitor_ref|lsoa', colnames(trainSet)) ], 
                            method = "rf",
                            # tuneLength = 5,
                            trControl = fit_control)



fda_model <- train(is_exceed ~ ., 
                           data = trainSet, 
                           method = "fda",
                           # tuneLength = 5,
                           trControl = fit_control
                          )

earth_model <- train(is_exceed ~ ., 
                   data = trainSet, 
                   method = "earth",
                   # tuneLength = 5,
                   trControl = fit_control
)


earth_model_unbias <- train(is_exceed ~ ., 
                     data = trainSet[, !grepl('monitor_ref|lsoa', colnames(trainSet)) ], 
                     method = "earth",
                     # tuneLength = 5,
                     trControl = fit_control
)

nnet_model <- train(is_exceed ~ ., 
                  data = trainSet, 
                  method = "nnet",
                  # tuneLength = 5,
                  trControl = fit_control,
                  importance = 'permutation')

nnet_model

varImp(nnet_model)

earth_model_unbias
earth_model

bag_model

earth_model

varImp(bag_model)

varImp(bayesglm_model)

varImp(pda_model)

logit_model
fda_model

varImp(fda_fit)

varImp(logit_model)

rf_fit

knn_noise_fit_big

log_fit_bias



colnames(pure_dummy_noise)

prediction = predict(log_fit, newdata = testSet, type = 'prob')
prediction3 = predict(log_fit_bias, newdata = testSet, type = 'prob')
predictionknn = predict(knn_model, newdata = testSet, type = 'prob')
predictionrf = predict(rf_model, newdata = testSet, type = 'prob')%>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionfda = predict(fda_model, newdata = testSet, type = 'prob')%>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionlogit = predict(logit_model, newdata = testSet, type = 'prob')%>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionearth = predict(earth_model, newdata = testSet, type = 'prob')%>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionnnet = predict(nnet_model, newdata = testSet, type = 'prob')%>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionearth_unbias = predict(earth_model_unbias, newdata = testSet, type = 'prob')%>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))




colnames(prediction)
unique(testSet$is_exceed)

table(prediction$pred)

table(testSet$is_exceed)

prediction3 <- prediction3 %>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionknn <- predictionknn %>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))
predictionrf <- predictionrf %>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))

prediction <- prediction %>%
  mutate(pred = ifelse(exceed > 0.5, 'exceed', 'not_exceed'))

cm <- confusionMatrix(as.factor(prediction$pred), as.factor(testSet$is_exceed))
cm3 <- confusionMatrix(as.factor(prediction3$pred), as.factor(testSet$is_exceed))
cmknn <- confusionMatrix(as.factor(predictionknn$pred), as.factor(testSet$is_exceed))
cmrf <- confusionMatrix(as.factor(predictionrf$pred), as.factor(testSet$is_exceed))
cmfda <- confusionMatrix(as.factor(predictionfda$pred), as.factor(testSet$is_exceed))
cmlogit <- confusionMatrix(as.factor(predictionlogit$pred), as.factor(testSet$is_exceed))
cmearth <- confusionMatrix(as.factor(predictionearth$pred), as.factor(testSet$is_exceed))
cmnnet <- confusionMatrix(as.factor(predictionnnet$pred), as.factor(testSet$is_exceed))
cmearth_unbias <- confusionMatrix(as.factor(predictionearth_unbias$pred), as.factor(testSet$is_exceed))




modelfiles <- list(rf_model, knn_model, nnet_model, earth_model, logit_model, earth_model_unbias, fda_model)

cm_set <- list(cmrf, cmknn, cmnnet, cmearth, cmlogit, cmearth_unbias, cmfda)


varimp_set <- list(varImp(rf_model),
                   varImp(knn_model),
                   varImp(nnet_model), 
                   varImp(earth_model), 
                   varImp(logit_model), 
                   varImp(earth_model_unbias), 
                   varImp(fda_model))




cmearth$byClass
varImp(earth_model)
varImp(earth_model_unbias)
varImp(rf_model)

rf_model
cmrf$byClass

17015/39075



cmlogit
cmearth_unbias



cmearth
cmfda
cmfda$byClass
cmknn
cmrf
cmknn$byClass
cmrf$byClass
cm3$byClass
cmnnet$byClass

varImp(knn_model)
varImp(rf_model)


plot(varImp(fda_model), top = 10)


saveRDS(knn_noise_fit_big, 'knn_model.rds')

saveRDS(earth_model, 'earth_model.rds')

saveRDS(rf_fit, 'rf_model.rds')


rf_fit <- readRDS('rf_model.rds')
rf_fit

rf_model <- readRDS('rf_model.rds')

rf_model
saveRDS(fda_model, 'fda_model.rds')
fda_model <- readRDS('fda_model.rds')

knn_model <- readRDS('knn_model.rds')

earth_model_unbias <- readRDS('earth_model_unbias.rds')
earth_model <- readRDS('earth_model.rds')  
  
saveRDS(earth_model_unbias, 'earth_model_unbias.rds')

saveRDS(logit_model, 'logit_model.rds')

logit_model <- readRDS('logit_model.rds')
nnet_model <- readRDS('nnet_model.rds')


saveRDS(nnet_model, 'nnet_model.rds')


varimp_set <- list(varImp(rf_model),
                   
                   varImp(nnet_model), 
                   varImp(earth_model), 
                   
                   varImp(earth_model_unbias), 
                   varImp(fda_model))
cm_set <- list(cmrf,  cmnnet, cmearth, cmearth_unbias, cmfda, cmknn, cmlogit )

datafiles <- list(predictionrf, predictionnnet, predictionearth, 
                  predictionearth_unbias, predictionfda, predictionknn,  predictionlogit)

setwd('~/Desktop/ml_app')

saveRDS(varimp_set, 'varimp_set.rds')
saveRDS(cm_set, 'cm_set.rds')
saveRDS(datafiles, 'datafiles.rds')


varimp_set <- readRDS('varimp_set.rds')
cm_set <- readRDS('cm_set.rds')
datafiles <- readRDS('datafiles.rds')

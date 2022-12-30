library("tidyverse")
library("caret")
library("randomForest")
path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)

#set.seed(123)
#test_model<-randomForest(formula= intentional_cause~.,data = fire_Train_Data)
#test_model

#confusionMatrix(predict(fire_Train_Data, fire_Test_Data), fire_Test_Data$intentional_cause)

###########################
##########################
##########################
#library(caret)
# In R, the intentional_cause~. means use intentional_cause as the model response
# and use all other variables as predictors
#lm1 <- train(intentional_cause~., data = fire_Train_Data, method = "lm")
#class(lm1)
#attributes(lm1)
#lm1$finalModel
#rf1 <- train(intentional_cause~., data = fire_Train_Data, method = "rf")
#rf1$finalModel

###########################
##########################
##########################
###########################
##########################
##########################
library(caret)

dim(fire_Train_Data)
dim(fire_Test_Data)

# list types for each attribute
sapply(fire_Train_Data, class)
sapply(fire_Test_Data, class)

# split input and output
x <- fire_Train_Data[,1:19]
y <- fire_Train_Data[,20]

# a) linear algorithms
set.seed(7)
fit.lda <- train(intentional_cause~., data=fire_Train_Data, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(spam$intentional_cause~., data=fire_Train_Data, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(intentional_cause~., data=fire_Train_Data, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(intentional_cause~., data=fire_Train_Data, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(intentional_cause~., data=fire_Train_Data, method="rf", metric=metric, trControl=control)













library(rpart)
library(rpart.plot)
library(MASS)
library(caret)
library(glmnet)
library(sjPlot)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)

lrmDingling <- lm(intentional_cause~., fire_Train_Data) # build the model on training data
# Make predictions
lrmDingling <- levels(droplevels(lrmDingling$municipality))
AQI_Pred_lr <- predict(lrmDingling, fire_Test_Data[, -20]) # predict intentional_cause on test data without AQI_value column

#Calculate prediction accuracy and error rates
#A simple correlation between the actuals and predicted values can be used as a form of accuracy measure. A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, i.e. when the actuals values increase the predicteds also increase and vice-versa.

actuals_preds <- data.frame(cbind(actuals=fire_Test_Data$intentional_cause, predicteds=AQI_Pred_lr ))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
#head(actuals_preds)







#################3
############
#################3
############
#################3
############
#################3
############
#################3
############
library(tidyverse)
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)
#fire_Test_Data$intentional_cause = NA


#pima_rec <- recipe(intentional_cause ~.,fire_Train_Data) 
fire_Train_Data$intentional_cause <- as.factor(fire_Train_Data$intentional_cause)
pima_rec <- recipe(intentional_cause ~.,fire_Train_Data)
pima_rec
pima_rec <- pima_rec %>% step_normalize(all_numeric_predictors()) %>% prep() 
pima_train <- pima_rec %>% bake(new_data=NULL) #não vai nenhum null no data set
pima_test <- pima_rec %>% bake(new_data=fire_Test_Data)
str(pima_train)
str(pima_test)


library(kknn)
model_knn <- nearest_neighbor(mode="classification")

#Fit the k-nn algorithm to the train data and inspect the obtained model.
knn_fit <- model_knn %>%
  fit(intentional_cause ~., data = pima_train)#prever intentional_cause em relação a todas as variaveis
knn_fit


#Make predictions on the test set.
knn_preds <- predict(knn_fit,new_data = pima_test)
knn_preds

#como saber a mat de confusão se não tenho output para comparar??±?±?±?±
#knn_preds <-
#  pima_test %>% dplyr::select() %>% 
#  bind_cols(predict(knn_fit, pima_test))

#knn_preds %>% conf_mat(intentional_cause,.pred_class) %>% autoplot(type="heatmap") 
#knn_preds %>% accuracy(truth=intentional_cause,estimate=.pred_class)

fire_Test_Data








#Using the same experimental setting, run the Naive Bayes algorithm. Be critical regarding the results
library(discrim) 
library(klaR)
path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)
fire_Test_Data$intentional_cause = NA


model_nb <- naive_Bayes(mode="classification")

#pima_rec <- recipe(intentional_cause ~.,fire_Train_Data) 
fire_Train_Data$intentional_cause <- as.factor(fire_Train_Data$intentional_cause)
pima_rec <- recipe(intentional_cause ~.,fire_Train_Data)
pima_rec
pima_rec <- pima_rec %>% step_normalize(all_numeric_predictors()) %>% prep() 
pima_train <- pima_rec %>% bake(new_data=NULL) #não vai nenhum null no data set
pima_test <- pima_rec %>% bake(new_data=fire_Test_Data)
str(pima_train)
str(pima_test)


nb_fit <- model_nb %>% fit(intentional_cause ~., data =pima_train)

nb_preds <- 
  pima_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(knn_fit, pima_test)) %>% 
  bind_cols(predict(knn_fit, pima_test,type="prob"))

nb_preds

nb_preds %>% accuracy(truth=intentional_cause,estimate=.pred_class)

nb_preds %>% roc_auc(truth=relevel(intentional_cause,"pos"),estimate=.pred_pos)
#Plot the ROC Curve.
roc_curve(nb_preds,relevel(intentional_cause,"pos"),.pred_pos) %>% autoplot()






##################################
library(tidyverse)
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)
#fire_Test_Data$intentional_cause = NA

#str(fire_Test_Data)

####após verificar algumas correlações e ler a importancia das features com CART
#vamos testar com district, TemperatureCMax, WindkmhInt, TemperatureCAvg, TemperatureCMin, village_area, extinction_hour, farming_area, village_veget_area

model_lm <- 
  linear_reg(engine="lm")

set.seed(1)
linreg_reg_fit <- model_lm %>%   fit(intentional_cause ~ district, TemperatureCMax, data = fire_Train_Data) 
linreg_reg_fit

predict(linreg_reg_fit, fire_Test_Data)

lm_fit1 <- model_lm %>%
  fit(intentional_cause ~ district, TemperatureCMax, WindkmhInt, TemperatureCAvg, TemperatureCMin, village_area, extinction_hour, farming_area, village_veget_area, data = fire_Train_Data) 
tidy(lm_fit1)

lm_preds1 <-
  fire_Test_Data %>% dplyr::select("intentional_cause") %>% bind_cols(predict(lm_fit1,fire_Test_Data))
lm_preds1 %>% metrics(truth=intentional_cause,estimate=.pred)




#############################
####CART TREES#########
#############################
library(tidyverse)
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)
fire_Test_Data$intentional_cause = NA

model_rt <- decision_tree(mode="regression", engine="rpart")
rt_fit <- model_rt %>% fit(intentional_cause ~ ., data = fire_Train_Data)
library(rpart.plot)
# to extract it from the parsnip
rt_fit %>% extract_fit_engine() %>% rpart.plot(roundint=FALSE)

library(vip) 
vip(rt_fit)

fire_tree <- rt_fit %>% extract_fit_engine() 
fire_tree$variable.importance
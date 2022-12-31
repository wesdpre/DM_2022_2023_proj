library(tidyverse) 
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)
fire_Train_Data$intentional_cause <- as.factor(fire_Train_Data$intentional_cause)

str(fire_Train_Data) 
summary(fire_Train_Data)

set.seed(1234)
#pima_split <- PimaIndiansDiabetes %>% initial_split(prop=.7)
pima_split <- fire_Train_Data %>% initial_split(prop=.7,strata=intentional_cause) 
pima_split

train <- training(pima_split)
test <- testing(pima_split)
summary(train$intentional_cause)
summary(test$intentional_cause)

pima_rec <- recipe(intentional_cause ~.,train) 
pima_rec

pima_rec <- pima_rec %>% step_normalize(all_numeric_predictors()) %>% prep() 
pima_train <- pima_rec %>% bake(new_data=NULL)
pima_test <- pima_rec %>% bake(new_data=test)

library(kknn)
model_knn <- nearest_neighbor(mode="classification")


knn_fit <- model_knn %>%
  fit(intentional_cause ~ ., data = pima_train)
knn_fit

knn_preds <- predict(knn_fit,new_data = pima_test)

knn_preds <-
  pima_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(knn_fit, pima_test))

knn_preds %>% conf_mat(intentional_cause,.pred_class) %>% autoplot(type="heatmap") 
knn_preds %>% accuracy(truth=intentional_cause,estimate=.pred_class)


knn_preds <-
  pima_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(knn_fit, pima_test)) %>% 
  bind_cols(predict(knn_fit, pima_test,type="prob"))

knn_preds %>% roc_auc(truth=relevel(intentional_cause,"pos"),estimate=.pred_pos)


roc_curve(knn_preds,relevel(intentional_cause,"pos"),.pred_pos) %>% autoplot()
#################################################################################################################
##################################################################################################################
##################################################################################################################
#####################################NAIVE BAYES##################################################################
##################################################################################################################
##################################################################################################################
###################################################################################################################
###############################################################################################################
##################################################################################################################

library(tidyverse) 
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)
fire_Train_Data$intentional_cause <- as.factor(fire_Train_Data$intentional_cause)

str(fire_Train_Data) 
summary(fire_Train_Data)

set.seed(1234)
#pima_split <- PimaIndiansDiabetes %>% initial_split(prop=.7)
pima_split <- fire_Train_Data %>% initial_split(prop=.7,strata=intentional_cause) 
pima_split

train <- training(pima_split)
test <- testing(pima_split)
summary(train$intentional_cause)
summary(test$intentional_cause)

pima_rec <- recipe(intentional_cause ~.,train) 
pima_rec

pima_rec <- pima_rec %>% step_normalize(all_numeric_predictors()) %>% prep() 
pima_train <- pima_rec %>% bake(new_data=NULL)
pima_test <- pima_rec %>% bake(new_data=test)


library(discrim) 
library(klaR)

model_nb <- naive_Bayes(mode="classification")

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

#################################################################################################################
##################################################################################################################
##################################################################################################################
##################################Multiple Linear Regression##################################################################
##################################################################################################################
##################################################################################################################
###################################################################################################################
###############################################################################################################
##################################################################################################################

library(tidyverse) 
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

str(fire_Train_Data) 
summary(fire_Train_Data)

set.seed(123)
fire_split <- fire_Train_Data %>% initial_split(prop=.7,strata=intentional_cause) #rsample
fire_split
## <Training/Testing/Total> 
fire_train <- training(fire_split) 
fire_test <- testing(fire_split)

model_lm <- linear_reg(engine="lm") 
lm_fit1 <- model_lm %>%
  fit(intentional_cause ~ district, data = fire_train) 

tidy(lm_fit1)

lm_preds1 <-
  fire_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(lm_fit1,fire_test))

lm_preds1 %>% metrics(truth=intentional_cause,estimate=.pred)

#model_lm <- linear_reg(engine="lm")

lm_fit2 <- model_lm %>%
  fit(intentional_cause ~ district + TemperatureCMax + WindkmhInt + TemperatureCAvg + TemperatureCMin + village_area + extinction_hour + farming_area + village_veget_area, data = fire_train) 
tidy(lm_fit2)

lm_preds2 <-
  fire_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(lm_fit2,fire_test))

##################################Ridge Regressio##################################################################
# model_glm_ridge <- linear_reg(engine="glmnet",penalty = 10^2,mixture=0)
model_glm_ridge <- linear_reg(engine="glmnet",penalty = 10^-2,mixture=0) 

glm_ridge_fit <- model_glm_ridge %>%
  fit(intentional_cause ~ district + TemperatureCMax, data = fire_train) 
tidy(glm_ridge_fit)

glm_ridge_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(glm_ridge_fit,fire_test))
glm_ridge_preds %>% metrics(truth=intentional_cause,estimate=.pred)

# model_glm_ridge <- linear_reg(engine="glmnet",penalty = 10^2,mixture=0)
model_glm_ridge <- linear_reg(engine="glmnet",penalty = 10^-2,mixture=0) 

glm_ridge_fit <- model_glm_ridge %>%
  fit(intentional_cause ~ district, data = fire_train) 
tidy(glm_ridge_fit)

glm_ridge_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(glm_ridge_fit,fire_test))
glm_ridge_preds %>% metrics(truth=intentional_cause,estimate=.pred)


##################################Lasso Regression##################################################################
# model_glm_lasso <- linear_reg(engine="glmnet",penalty = 10^2,mixture=1)
model_glm_lasso <- linear_reg(engine="glmnet",penalty = 10^-2,mixture=1) 
glm_lasso_fit <- model_glm_lasso %>%
  fit(intentional_cause ~ district, data = fire_train)  
tidy(glm_lasso_fit)

glm_lasso_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(glm_ridge_fit,fire_test))
glm_lasso_preds %>% metrics(truth=intentional_cause,estimate=.pred)

# model_glm_lasso <- linear_reg(engine="glmnet",penalty = 10^2,mixture=1)
model_glm_lasso <- linear_reg(engine="glmnet",penalty = 10^-2,mixture=1) 
glm_lasso_fit <- model_glm_lasso %>%
  fit(intentional_cause ~ district + TemperatureCMax  + WindkmhInt, data = fire_train)  
tidy(glm_lasso_fit)

glm_lasso_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(glm_ridge_fit,fire_test))
glm_lasso_preds %>% metrics(truth=intentional_cause,estimate=.pred)



#############################CART TREES#############################################################################
model_rt <- decision_tree(mode="regression", engine="rpart")
rt_fit <- model_rt %>% 
  fit(intentional_cause ~ district, data = fire_train)

rt_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(rt_fit,fire_test))
rt_preds %>% metrics(truth=intentional_cause,estimate=.pred)

model_rt <- decision_tree(mode="regression", engine="rpart")
rt_fit <- model_rt %>% 
  fit(intentional_cause ~ district + TemperatureCMax, data = fire_train)

rt_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(rt_fit,fire_test))
rt_preds %>% metrics(truth=intentional_cause,estimate=.pred)

model_rt <- decision_tree(mode="regression", engine="rpart")
rt_fit <- model_rt %>% 
  fit(intentional_cause ~ district + TemperatureCMax + WindkmhInt, data = fire_train)

rt_preds <-
  fire_test %>% dplyr::select(intentional_cause) %>% bind_cols(predict(rt_fit,fire_test))
rt_preds %>% metrics(truth=intentional_cause,estimate=.pred)




#####################################KNN##############################################
library(tidyverse) 
library(tidymodels)
library(modelr)
library(lubridate)
library(broom)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)
fire_Train_Data <- fire_Train_Data %>% fill(TemperatureCAvg)
fire_Train_Data <- fire_Train_Data %>% fill(TemperatureCMax)
fire_Train_Data <- fire_Train_Data %>% fill(TemperatureCMin)
fire_Train_Data <- fire_Train_Data %>% fill(HrAvg)
fire_Train_Data <- fire_Train_Data %>% fill(WindkmhInt)

fire_Train_Data$intentional_cause <- as.factor(fire_Train_Data$intentional_cause)
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)

str(fire_Train_Data) 
summary(fire_Train_Data)

set.seed(1234)
#f_split <- fIndiansDiabetes %>% initial_split(prop=.7)
f_split <- fire_Train_Data %>% initial_split(prop=.7,strata=intentional_cause) 
f_split

train <- training(f_split)
test <- testing(f_split)
summary(train$intentional_cause)
summary(test$intentional_cause)

f_rec <- recipe(intentional_cause ~.,train) 
f_rec

f_rec <- f_rec %>% step_normalize(all_numeric_predictors()) %>% prep() 
f_train <- f_rec %>% bake(new_data=NULL)
f_test <- f_rec %>% bake(new_data=test)



library(kknn)
model_knn <- nearest_neighbor(mode="classification")

#Fit the k-nn algorithm to the train data and inspect the obtained model.
knn_fit <- model_knn %>%
  fit(intentional_cause ~ district + TemperatureCMax + WindkmhInt + TemperatureCAvg + TemperatureCMin + village_area + extinction_hour + farming_area + village_veget_area, data = f_train, na.action = na.exclude)#prever intentional_cause em relação a todas as variaveis
knn_fit


#Make predictions on the test set.
knn_preds <- predict(knn_fit,new_data = f_test)
knn_preds

#como saber a mat de confusão se não tenho output para comparar??±?±?±?±
knn_preds <-
  f_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(knn_fit, f_test))

knn_preds %>% conf_mat(intentional_cause,.pred_class) %>% autoplot(type="heatmap") 
knn_preds %>% metrics(truth=intentional_cause,estimate=.pred_class)

#para o kaggle

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")
fire_Test_Data <- readRDS(path)

prev <- predict(knn_fit, fire_Test_Data, , type = "class")
prev = cbind("id"=rownames(prev),prev)
names(prev)[length(names(prev))]<-"intentional_cause" 
write.csv(prev, "grupo13_DMI.csv", row.names=FALSE)

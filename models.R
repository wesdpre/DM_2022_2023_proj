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
  fit(intentional_cause ~ district + TemperatureCMax, data = fire_train) 
tidy(lm_fit2)

lm_preds2 <-
  fire_test %>% dplyr::select(intentional_cause) %>% 
  bind_cols(predict(lm_fit2,fire_test))
lm_preds2 %>% metrics(truth=intentional_cause,estimate=.pred)


load(file="Rdata/Test_Data_noNa.Rdata")
test_data_noNa$fire_duration <- as.numeric(difftime(as_datetime(paste(date(train_data_NA$extinction_date), train_data_NA$extinction_hour)), as_datetime(paste(date(train_data_NA$alert_date), train_data_NA$alert_hour)), units = "mins"))
train_data_NA$fire_duration <- ifelse(train_data_NA$fire_duration < 0, 0, train_data_NA$fire_duration) 

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

library(tidyverse)
library(lubridate)
library(DMwR2)
library(xts)
library(zoo)
library(quantmod)
library(TTR)
library(caret)
library(lattice)
library(ModelMetrics)
library(pROC)
library(RANN)

#fire_Train_Data <- read_csv("fires_train.csv")
#apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)

fire_Test_Data <- read_csv("fires_test.csv")
apply(X = is.na(fire_Test_Data), MARGIN = 2, FUN = sum)

#fire_Train_Data <-fire_Train_Data %>%  
#  select(-c(alert_source, parish, district, id))

fire_Test_Data <-fire_Test_Data %>%  
  select(-c(alert_source, parish, district))

#REGION Só tem um pu dois na pode ser preenchido
#is.na(fire_Train_Data)#saber onde está na
#which(is.na(fire_Train_Data$region))#saber a linha do na
#fire_Train_Data[5275,]
#fire_Train_Data[fire_Train_Data$municipality == "Almada",] #Ribatejo e Oeste
#fire_Train_Data <- fire_Train_Data %>% mutate(region = ifelse(is.na(region), "Ribatejo e Oeste", region))
is.na(fire_Test_Data)#saber onde está na
which(is.na(fire_Test_Data$region))#saber a linha do na
fire_Test_Data[518,]
fire_Test_Data[3987,]
fire_Test_Data <- fire_Test_Data %>% mutate(region = ifelse(is.na(region), "Alentejo", region))

#spec(fire_Train_Data)
#str(fire_Train_Data)
#summary(fire_Train_Data)
#fire_Train_Data

#apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)

#alert_date_pull <- fire_Train_Data %>%  pull(alert_date)
#alert_hour_pull <- fire_Train_Data %>%  pull(alert_hour)

#mutate a coluna fire_Train_alert_date
#fire_Train_Data <- fire_Train_Data %>% mutate(alert_date = alert_date_pull %>% ymd_hms())
#summary(fire_Train_Data)

#Fill com o de cima ou debaixo
#fire_Train_Data <- fire_Train_Data %>% fill(extinction_hour)
#knn imputation
#fire_Train_Data <- kNN(fire_Train_Data, variable = c("extinction_hour", "extinction_date"), k=6 )
#apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
#y = c("extinction_hour", "firstInterv_date", "firstInterv_hour")
#vars <- "y"
#fire_Train_Data <- drop_na(fire_Train_Data, any_of(y))
#apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
#summary(fire_Train_Data)

#Fill com o de cima ou debaixo
#fire_Test_Data <- fire_Test_Data %>% fill(extinction_hour)
#knn imputation
#fire_Test_Data <- kNN(fire_Test_Data, variable = c("extinction_hour", "extinction_date"), k=6 )
apply(X = is.na(fire_Test_Data), MARGIN = 2, FUN = sum)
y = c("extinction_hour", "firstInterv_date", "firstInterv_hour")
vars <- "y"
fire_Test_Data <- drop_na(fire_Test_Data, any_of(y))
apply(X = is.na(fire_Test_Data), MARGIN = 2, FUN = sum)
summary(fire_Test_Data)



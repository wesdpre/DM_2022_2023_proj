library(tidyverse)
library(lubridate)
library(DMwR2)
library(dplyr)

fire_Train_Data <- read_csv("fires_train.csv")
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)

fire_Train_Data <- fire_Train_Data %>% select(-c(alert_source, parish))

#REGION Só tem um ou dois na pode ser preenchido
is.na(fire_Train_Data)#saber onde está na
which(is.na(fire_Train_Data$region))#saber a linha do na
fire_Train_Data[5275,]
fire_Train_Data[fire_Train_Data$municipality == "Almada",] #Ribatejo e Oeste
fire_Train_Data <- fire_Train_Data %>% mutate(region = ifelse(is.na(region), "Ribatejo e Oeste", region))

#fazer drop de linhas com valores nulos nas colunas "extinction_hour", "firstInterv_date", "firstInterv_hour"
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
y = c("extinction_hour", "firstInterv_date", "firstInterv_hour")
vars <- "y"
fire_Train_Data <- drop_na(fire_Train_Data, any_of(y))
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
summary(fire_Train_Data)
#str(fire_Train_Data)




fire_Test_Data <- read_csv("fires_test.csv")
apply(X = is.na(fire_Test_Data), MARGIN = 2, FUN = sum)

fire_Test_Data <- fire_Test_Data %>% select(-c(alert_source, parish))

#REGION Só tem um ou dois na pode ser preenchido
is.na(fire_Test_Data)#saber onde está na
which(is.na(fire_Test_Data$region))#saber a linha do na
fire_Test_Data[518,]
fire_Test_Data[3987,]
fire_Test_Data <- fire_Test_Data %>% mutate(region = ifelse(is.na(region), "Alentejo", region))

fire_Test_Data <- fire_Test_Data %>% fill(extinction_date)
fire_Test_Data <- fire_Test_Data %>% fill(extinction_hour)
fire_Test_Data <- fire_Test_Data %>% fill(firstInterv_date)
fire_Test_Data <- fire_Test_Data %>% fill(firstInterv_hour)
apply(X = is.na(fire_Test_Data), MARGIN = 2, FUN = sum)




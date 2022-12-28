library(tidyverse)
library(lubridate)

fire_Train_Data <- read_csv("fires_train.csv")
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
fire_Train_Data <-fire_Train_Data %>%
  select(-c(alert_source, parish, district))
is.na(fire_Train_Data)#saber onde está na
which(is.na(fire_Train_Data$region))#saber a linha do na
fire_Train_Data[5275,]
fire_Train_Data[fire_Train_Data$municipality == "Almada",] #Ribatejo e Oeste
fire_Train_Data <- fire_Train_Data %>% mutate(region = ifelse(is.na(region), "Ribatejo e Oeste", region))
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
y = c("extinction_hour", "firstInterv_date", "firstInterv_hour")
vars <- "y"
fire_Train_Data <- drop_na(fire_Train_Data, any_of(y))
apply(X = is.na(fire_Train_Data), MARGIN = 2, FUN = sum)
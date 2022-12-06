
library(tidyverse)

fire_Train_Data <- read_csv("~/up201905966/DM1/proj/DM_2022_2023_proj/fires_train.csv")

#eleminar firstInterv_hour, alert_source, parish, region, distinct e a id
fire_Train_Data <-fire_Train_Data %>%  select(-c(firstInterv_hour, alert_source, parish, region, district ,id))

fire_Train_Data <- fire_Train_Data  %>% drop_na(alert_hour)

spec(fire_Train_Data)
str(fire_Train_Data)
summary(fire_Train_Data)
fire_Train_Data


fire_Train_Data <- fire_Train_Data %>% mutate(intentional_cause=as.factor(intentional_cause),
                                    region = as.factor(region),
                                    municipality=as.factor(municipality),
                                    parish=as.factor(parish),
                                    district=as.factor(district))

fire_Train_Data <- fire_Train_Data %>% mutate(alert_hour       =as.(alert_hour       ),
                                            extinction_date    =as.Date(extinction_date),
                                            extinction_hour    =as.Date(extinction_hour    ),
                                            firstInterv_date      =as.Date(firstInterv_date      ),
                                            firstInterv_hour       =as.Date(firstInterv_hour       ))

fire_Train_Data <-fire_Train_Data %>%  select(-firstInterv_hour)


fire_Train_Data %>% group_by(alert_hour) %>% count()


fire_Train_Data


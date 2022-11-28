
library(tidyverse)
fireData_Train <- read_csv("~/up201905966/DM1/proj/DM_2022_proj/fires_train.csv",na = "?")

spec(fireData_Train)

fireData_Train

summary(fireData_Train)

fireData_Train <- fireData_Train %>% mutate(intentional_cause=as.factor(intentional_cause),
                                    region = as.factor(region),
                                    municipality=as.factor(municipality),
                                    parish=as.factor(parish),
                                    district=as.factor(district))

fireData_Train <- fireData_Train %>% mutate(alert_hour       =asDateBuilt(alert_hour       ),
                                            extinction_date    =as.Date(extinction_date),
                                            extinction_hour    =as.Date(extinction_hour    ),
                                            firstInterv_date      =as.Date(firstInterv_date      ),
                                            firstInterv_hour       =as.Date(firstInterv_hour       ))

fireData_Train %>% group_by(alert_hour) %>% count()

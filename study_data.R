
library(tidyverse)
library(lubridate)


#PC do Pedro A. 
#fire_Train_Data <- read_csv("~/up201905966/UC_M1_1s_DM1/proj/DM_2022_2023_proj/fires_train.csv")

# star of data preparation 
#delete firstInterv_hour, alert_source, parish, region, distinct e a id

fire_Train_Data <-fire_Train_Data %>%  
  select(-c(firstInterv_hour, alert_source, parish, region, district ,id))

fire_Train_Data <- fire_Train_Data  %>% drop_na(alert_hour)

spec(fire_Train_Data)
str(fire_Train_Data)
summary(fire_Train_Data)
fire_Train_Data

####################################  date mutate  #######################################

alert_date_pull <- fire_Train_Data %>%  pull(alert_date)
alert_hour_pull <- fire_Train_Data %>%  pull(alert_hour)

#mutate a coluna fire_Train_alert_date
fire_Train_Data <- fire_Train_Data %>% mutate(alert_date = alert_date_pull %>% ymd_hms())

#criar a coluna fire_Train_alert_week
fire_Train_alert_week <- alert_date_pull%>% week()
fire_Train_Data <- fire_Train_Data %>% mutate(alert_week = fire_Train_alert_week)

#criar a coluna fire_Train_alert_month
fire_Train_alert_month <- alert_date_pull%>% month()
fire_Train_Data <- fire_Train_Data %>% mutate(alert_month = fire_Train_alert_month) 

######
#criar a coluna fire_Train_alert_hour
fire_Train_alert_hour
alert_hour_pull
fire_Train_alert_hour <- alert_hour_pull%>% hour()
#fire_Train_alert_hour <- fire_Train_alert_hour %>% as.character()
                                              
time_of_day <- function(hours){
  hours <- hours%>% hour()
  for (i in 1:length(hours)) {
    if(hours[i] >= 0 && hours[i]< 5){
      hours[i] <- "Night"
    }else if(hours[i] > 4 && hours[i]< 12){
      hours[i] <- "Morning"
    }else if(hours[i] > 11 && hours[i]< 17){
      hours[i] = "Afternoon"
    }else if(hours[i] > 16 && hours[i]< 21){
      hours[i] <- "Evening"
    }else{
      hours[i] <- "a"
    }
  }
  hours
}

fire_Train_alert_hour

fire_Train_Data <-fire_Train_Data %>% mutate(timeOfDaay = time_of_day(alert_hour))
fire_Train_Data <-fire_Train_Data %>% mutate(timeOfDaay = as.vector(timeOfDaay))

str(fire_Train_Data)
summary(fire_Train_Data)


alert_date_pull%>% ymd_hms()
fire_Train_alert_week
fire_Train_alert_month

#################################### end of D mutate#####################################



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

fire_Train_Data %>% group_by(alert_hour) %>% count()

fire_Train_Data


library(remotes)
install_github("bczernecki/climate")
library(climate)
library(lubridate)
library(measurements)
library(tidyverse)

coordinates <- function(coordinate_string) {
  flag = 0
  if(grepl(':',coordinate_string)) {
    splited <- unlist(strsplit(coordinate_string, split=':'))
  } else {
    flag = 1
    splited <- unlist(strsplit(coordinate_string, split='º'))
    splited <- unlist(strsplit(splited, split="'"))
  }
  
  if(c(splited)[1] == "00") {
    flag = 2
  }
  
  print(flag)
  if(flag == 2) {
    splited2 <- unlist(strsplit(c(splited)[3], split="\\."))
    dms <- paste(c((splited)[2], (splited2)[1], (splited2)[2]), collapse = " ")
    coord <- conv_unit(dms, from = "deg_min_sec", to = "dec_deg")
  }else if(flag == 1) {
    dms <- paste(c((splited)[1], (splited)[2], (splited)[3]), collapse = " ")
    coord <- conv_unit(dms, from = "deg_min_sec", to = "dec_deg") 
  } else if(flag == 0) {
    splited3 <- unlist(strsplit(c(splited)[3], split="'"))
    dms <- paste(c((splited)[1], (splited)[2], (splited3)[1], collapse = " "))
    coord <- conv_unit(dms, from = "deg_min_sec", to = "dec_deg") 
  }
  print(c(coord[1]))
  return (c(coord[1]))
}

getData_Ogi <-function(lati, long, date) {
  # get the nearest station
  # be aware that the nearest station can change over time as new stations can appear
  nearest_station <- nearest_stations_ogimet(country = "Portugal", 
                                             date= ymd(date(date)),
                                             point = c(long, lati),
                                             add_map = FALSE, 
                                             no_of_stations = 1) 
  
  # scrap meteorological data from Ogimet regarding a period and a specific station
  meteo_data <- meteo_ogimet(date=date,interval="daily", station=nearest_station$wmo_id)
  return(meteo_data)
}

coord_values <- select(fire_Train_Data, lat, lon, alert_date)
train_data <- getData_Ogi(coord_values[[1]][1], coord_values[[2]][1], coord_values[[3]][1])
train_data$id <- c(fire_Train_Data[[1]][1])
count = 0
for(i in 2:nrow(fire_Train_Data)) {
  cat(i)
  latitude <- coordinates(coord_values[[1]][i])
  longitude <- paste("-", coordinates(coord_values[[2]][i]), sep="")
  
  ogimet_dados <- getData_Ogi(latitude, longitude, coord_values[[3]][i])
  if(length(nrow(ogimet_dados)) == 0 || nrow(ogimet_dados) != 0) {
    ogimet_dados$id = c(fire_Train_Data[[1]][i])
    #ogimet_dados <- ogimet_dados %>% add_column(id = c(fire_Train_Data[[1]][i]))
    train_data <- rbind(train_data[colnames(train_data)], ogimet_dados[colnames(train_data)])
  } else {
    count = count + 1
  }
}
save(train_data, file="Rdata/ogimetData_train.Rdata")

#para dar load e não correr sempre ogimet
load(file="Rdata/ogimetData_train.Rdata")

#remover colunas que têm demasiados NA's (> 10% do conjunto de dados)
train_data <- train_data %>% select(-c(station_ID,Date,WindkmhGust,Precmm,TotClOct,lowClOct,VisKm,PreselevHp,SnowDepcm,TdAvgC,WindkmhDir,PresslevHp,SunD1h))
train_data_NA <- merge(fire_Train_Data, train_data, by = c("id"))

# calcular duração do incêndio
train_data_NA$fire_duration <- as.numeric(difftime(as_datetime(paste(date(train_data_NA$extinction_date), train_data_NA$extinction_hour)), as_datetime(paste(date(train_data_NA$alert_date), train_data_NA$alert_hour)), units = "mins"))
train_data_NA$fire_duration <- ifelse(train_data_NA$fire_duration < 0, 0, train_data_NA$fire_duration) 

# Alterar os valores da variável origin de categóricos para numéricos
train_data_NA$origin <- c('fire'=1,'firepit'=2,'agriculture'=3,'agric_burn' =4,'false_alarm'=5)[train_data_NA$origin]

# Dividir o dia em 4 partes cada uma com 6 horas (1h - 6:59 -> 1 parte, 7h - 12:59 -> 2 parte, 13h - 18:59 -> 3 parte, 19h - 00h59 -> 4 parte)
# Alert hour
train_data_NA$alert_hour <- hour(train_data_NA$alert_hour)
train_data_NA$alert_hour <- ifelse(train_data_NA$alert_hour <= 6 & train_data_NA$alert_hour > 0, 1, ifelse(train_data_NA$alert_hour <= 12 & train_data_NA$alert_hour > 6, 2, ifelse(train_data_NA$alert_hour <= 18 & train_data_NA$alert_hour > 12, 3, 4)))

# First Intervation hour
train_data_NA$firstInterv_hour <- hour(train_data_NA$firstInterv_hour)
train_data_NA$firstInterv_hour <- ifelse(train_data_NA$firstInterv_hour <= 6 & train_data_NA$firstInterv_hour > 0, 1, ifelse(train_data_NA$firstInterv_hour <= 12 & train_data_NA$firstInterv_hour > 6, 2, ifelse(train_data_NA$firstInterv_hour <= 18 & train_data_NA$firstInterv_hour > 12, 3, 4)))

# Extinction hour
train_data_NA$extinction_hour <- hour(train_data_NA$extinction_hour)
train_data_NA$extinction_hour <- ifelse(train_data_NA$extinction_hour <= 6 & train_data_NA$extinction_hour > 0, 1, ifelse(train_data_NA$extinction_hour <= 12 & train_data_NA$extinction_hour > 6, 2, ifelse(train_data_NA$extinction_hour <= 18 & train_data_NA$extinction_hour > 12, 3, 4)))

# Alterar data para o formato [year].[quarter]
train_data_NA$alert_date <- quarter(date(train_data_NA$alert_date), with_year = TRUE)
train_data_NA$firstInterv_date <- quarter(date(train_data_NA$firstInterv_date), with_year = TRUE)
train_data_NA$extinction_date <- quarter(date(train_data_NA$extinction_date), with_year = TRUE)

#remover outras colunas
dataset_train <- train_data_NA %>% select(-c(id,lon,lat,region))

# conjunto de dados com NAs 
save(dataset_train, file="Rdata/Train_Data_na.Rdata")

train_data_noNAs <- drop_na(dataset_train, any_of(c(colnames(train_data)[colnames(train_data) != "id"])))

#recolocou-se a coluna intentional_cause no fim uma vez que é o output
train_data_noNAs <- train_data_noNAs %>% relocate(intentional_cause, .after=fire_duration)

save(train_data_noNAs, file="Rdata/Train_Data_noNa.Rdata")
saveRDS(train_data_noNAs, "Rdata/Train_Data_noNa.rds")
cat('Percentagem da perda de valores obtidos inicialmente do conjunto de dados original', c((nrow(fire_Train_Data) - nrow(train_data_noNAs)) / nrow(fire_Train_Data) * 100), '%')



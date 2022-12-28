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

# verificar quantos NAs os atributos não numéricos têm
for(j in 1:19) {
  counter = 0
  for(i in 1:nrow(train_data)) {
    if(is.na(train_data[[j]][i])) {
      counter = counter + 1
    } 
  }
  print(c("NAs de ", j, counter))
}

#remover colunas que têm demasiados NA's (> 10% do conjunto de dados)
ogi_data <- train_data[, -c(1,2,10,12,13,14,16,17,18)]

train_data_NA <- merge(fire_Train_Data, ogi_data, by = c("id"))

summary(train_data_NA)

dataset_train <- train_data_NA[, -c(27,28)]

# conjunto de dados com NAs 
save(dataset_train, file="Rdata/Train_Data_na.Rdata")

y1 = c("TemperatureCAvg", "TemperatureCMax", "TemperatureCMin","TdAvgC","HrAvg","WindkmhDir","WindkmhInt","PresslevHp")
train_data_noNAs <- drop_na(dataset_train, any_of(y1))

save(train_data_noNAs, file="Rdata/Train_Data_noNa.Rdata")
# das 9997 observações iniciais obtemos 9288 observações resultando numa perda de 7% das observações
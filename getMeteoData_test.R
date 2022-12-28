library(remotes)
install_github("bczernecki/climate")
library(climate)
library(lubridate)
library(measurements)
library(tidyverse)

coordinates <- function(coordinate_string) {
  
  if(grepl(':',coordinate_string)) {
    splited <- unlist(strsplit(coordinate_string, split=':'))
  } else {
    splited <- unlist(strsplit(coordinate_string, split='º'))
    splited <- unlist(strsplit(splited, split="'"))
  }
  
  dms <- paste(c((splited)[1], (splited)[2], (splited)[3]), collapse = " ")
  coord <- conv_unit(dms, from = "deg_min_sec", to = "dec_deg")
  return (c(coord))
}

getData_Ogi <-function(lati, long, date) {
  
  lat <- coordinates(lati)
  longi <- paste("-", coordinates(long), sep="")
  
  print(c(lat,longi))
  
  # get the nearest station
  # be aware that the nearest station can change over time as new stations can appear
  nearest_station <- nearest_stations_ogimet(country = "Portugal", 
                                             date= ymd(date(date)),
                                             point = c(longi, lat),
                                             add_map = FALSE, 
                                             no_of_stations = 1) 
  
  # scrap meteorological data from Ogimet regarding a period and a specific station
  meteo_data <- meteo_ogimet(date=date,interval="daily", station=nearest_station$wmo_id)
  return(meteo_data)
}

coord_values <- select(fire_Test_Data, lat, lon, alert_date)
test_data <- getData_Ogi(coord_values[[1]][1], coord_values[[2]][1], coord_values[[3]][1])
test_data$id <- c(103010)

for(i in 2:nrow(fire_Test_Data)) {
  ogimet_dados <- getData_Ogi(coord_values[[1]][i], coord_values[[2]][i], coord_values[[3]][i])
  ogimet_dados$id <- c(fire_Test_Data[[1]][i])
  common <- intersect(colnames(test_data), colnames(test_data))
  test_data <- rbind(test_data[common], ogimet_dados[common])
  #save(dados, file="/home/santos/Desktop/DataMining/DataMiningProject/ogimetData/ogimetData.Rdata")
}
save(test_data, file="Rdata/ogimetData_test.Rdata")

#remover colunas WindkmhGust, Precmm, TotClOct, lowClOct, VisKm pois têm demasiados NA's (> 10% do conjunto de dados)
ogi_data <- test_data[, -c(1,2,10,12,16,17,18)]

data_set_with_na <- merge(fire_Test_Data, ogi_data, by = c("id", "id"))
save(ogi_data, file="Rdata/Test_Data_na.Rdata")

summary(data_set_with_na)
# temos 584 NAs em TotClOct, 724 NAs em lowClOct, 558 em VisKm e 667 NAs em WindkmhGust
# 724 representa aproximadamente 17% do data set
# a variável PreselevHp foi removida devido ao elevado número de NAs também

dataset_test_rmcolls <- data_set_with_na[, -c(22,23,24,25,28)]

y1 = c("TemperatureCAvg","HrAvg","WindkmhDir","WindkmhInt","TemperatureCMin","TdAvgC")
ogi_data_no_nas <- drop_na(dataset_test_rmcolls, any_of(y1))

save(ogi_data_no_nas, file="/home/santos/Desktop/DataMining/DM_2022_2023_proj/Rdata/Test_Data_noNa.Rdata")
# das 4283 observações iniciais obtemos 4063 observações resultando numa perda de 5.13% das observações
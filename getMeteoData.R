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

fireData_Train <- read.csv("/home/santos/Desktop/DataMining/DM_2022_2023_proj/fires_train.csv",na = "?")
coord_values <- select(fireData_Train, lat, lon, alert_date)

dados <- getData_Ogi(coord_values[[1]][1], coord_values[[2]][1], coord_values[[3]][1])

# NÃO SEI SE ESTÁ 100% BEM
for(i in 2:10309) {
  ogimet_dados <- getData_Ogi(coord_values[[1]][i], coord_values[[2]][i], coord_values[[3]][i])
  common <- intersect(colnames(dados), colnames(dados))
  dados <- rbind(dados[common], ogimet_dados[common])
  #save(dados, file="/home/santos/Desktop/DataMining/DataMiningProject/ogimetData/ogimetData.Rdata")
}

save(dados, file = "data.RData")


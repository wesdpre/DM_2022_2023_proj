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
  return (c(coord[1]))
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
test_data$id <- c(fire_Test_Data[[1]][1])

for(i in 2:nrow(fire_Test_Data)) {
  cat(i)
  ogimet_dados <- getData_Ogi(coord_values[[1]][i], coord_values[[2]][i], coord_values[[3]][i])
  ogimet_dados$id <- c(fire_Test_Data[[1]][i])
  test_data <- rbind(test_data[colnames(test_data)], ogimet_dados[colnames(test_data)])
}
save(test_data, file="Rdata/ogimetData_test.Rdata")

# verificar quantos NAs os atributos não numéricos têm
for(j in c(8,15,17,18)) {
  counter = 0
  for(i in 1:nrow(test_data)) {
    if(is.na(test_data[[j]][i])) {
      counter = counter + 1
    } 
  }
  print(c("NAs de ", j, counter))
}

#remover colunas que têm demasiados NA's (> 10% do conjunto de dados)
ogi_data <- test_data[, -c(1,2,8,10,12,13,14,15,16,17,18)]

test_data_NA <- merge(fire_Test_Data, ogi_data, by = c("id"))

summary(test_data_NA)

dataset_test <- test_data_NA[, -c(22,23,24)]

# conjunto de dados com NAs 
save(dataset_test, file="Rdata/Test_Data_na.Rdata")

y1 = c("TemperatureCAvg","HrAvg","WindkmhDir","WindkmhInt","TemperatureCMin","TdAvgC")
test_data_noNAs <- drop_na(dataset_test, any_of(y1))

save(test_data_noNAs, file="Rdata/Test_Data_noNa.Rdata")
# das 4283 observações iniciais obtemos 3901 observações resultando numa perda de 8.91% das observações
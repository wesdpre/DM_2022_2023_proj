library(remotes)
install_github("bczernecki/climate", force = TRUE)
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
test_data <- getData_Ogi(coord_values[[1]][1], coord_values[[2]][1], date(coord_values[[3]][1]))
#names(test_data) <- c("station_ID","Date","TemperatureCAvg","TemperatureCMax","TemperatureCMin","TdAvgC","HrAvg","WindkmhDir","WindkmhInt","WindkmhGust","PresslevHp","Precmm","TotClOct","lowClOct","SunD1h","VisKm","PreselevHp","SnowDepcm","id")
test_data$id <- c(fire_Test_Data[[1]][1])

count = 0
for(i in 2:nrow(fire_Test_Data)) {
  cat(i)
  ogimet_dados <- getData_Ogi(coord_values[[1]][i], coord_values[[2]][i], date(coord_values[[3]][i]))
  if(nrow(ogimet_dados) != 0 || length(nrow(ogimet_dados)) == 0) {
    ogimet_dados$id <- c(fire_Test_Data[[1]][i])
    test_data <- rbind(test_data[colnames(test_data)], ogimet_dados[colnames(test_data)])
  }else {
    count = count +1
  }
}

cat(count)
save(test_data, file="Rdata/ogimetData_test.Rdata")

load(file="Rdata/ogimetData_test.Rdata")

#remover colunas que têm demasiados NA's (> 10% do conjunto de dados)
test_data <- test_data %>% select(-c(station_ID,Date,TotClOct,SunD1h,WindkmhGust,Precmm,SnowDepcm,WindkmhDir,PreselevHp,lowClOct,VisKm,TdAvgC,PresslevHp))

test_data_NA <- merge(fire_Test_Data, test_data, by = c("id"), all = TRUE)[-1]

# calcular duração do incêndio
test_data_NA$fire_duration <- as.numeric(difftime(as_datetime(paste(date(test_data_NA$extinction_date), test_data_NA$extinction_hour)), as_datetime(paste(date(test_data_NA$alert_date), test_data_NA$alert_hour)), units = "mins"))
test_data_NA$fire_duration <- ifelse(test_data_NA$fire_duration < 0, 0, test_data_NA$fire_duration) 

# Alterar os valores da variável origin de categóricos para numéricos
test_data_NA$origin <- c('fire'=1,'firepit'=2,'agriculture'=3,'agric_burn' =4,'false_alarm'=5)[test_data_NA$origin]

# Dividir o dia em 4 partes cada uma com 6 horas (1h - 6:59 -> 1 parte, 7h - 12:59 -> 2 parte, 13h - 18:59 -> 3 parte, 19h - 00h59 -> 4 parte)
# Alert hour
test_data_NA$alert_hour <- hour(test_data_NA$alert_hour)
test_data_NA$alert_hour <- ifelse(test_data_NA$alert_hour <= 6 & test_data_NA$alert_hour > 0, 1, ifelse(test_data_NA$alert_hour <= 12 & test_data_NA$alert_hour > 6, 2, ifelse(test_data_NA$alert_hour <= 18 & test_data_NA$alert_hour > 12, 3, 4)))

# First Intervation hour
test_data_NA$firstInterv_hour <- hour(test_data_NA$firstInterv_hour)
test_data_NA$firstInterv_hour <- ifelse(test_data_NA$firstInterv_hour <= 6 & test_data_NA$firstInterv_hour > 0, 1, ifelse(test_data_NA$firstInterv_hour <= 12 & test_data_NA$firstInterv_hour > 6, 2, ifelse(test_data_NA$firstInterv_hour <= 18 & test_data_NA$firstInterv_hour > 12, 3, 4)))

# Extinction hour
test_data_NA$extinction_hour <- hour(test_data_NA$extinction_hour)
test_data_NA$extinction_hour <- ifelse(test_data_NA$extinction_hour <= 6 & test_data_NA$extinction_hour > 0, 1, ifelse(test_data_NA$extinction_hour <= 12 & test_data_NA$extinction_hour > 6, 2, ifelse(test_data_NA$extinction_hour <= 18 & test_data_NA$extinction_hour > 12, 3, 4)))

# Alterar data para o formato [year].[quarter]
test_data_NA$alert_date <- quarter(date(test_data_NA$alert_date), with_year = TRUE)
test_data_NA$firstInterv_date <- quarter(date(test_data_NA$firstInterv_date), with_year = TRUE)
test_data_NA$extinction_date <- quarter(date(test_data_NA$extinction_date), with_year = TRUE)

#remover outras colunas 
dataset_test <- test_data_NA %>% select(-c(region,lon,lat))

# conjunto de dados com NAs 
save(dataset_test, file="Rdata/Test_Data_na.Rdata")

#test_data_noNAs <- drop_na(dataset_test, any_of(c(colnames(test_data)[colnames(test_data) != "id"])))
# não remover NAs
test_data_noNAs <- test_data_NA
test_data_noNAs <- test_data_noNAs %>% select(-c(region,lon,lat))
save(test_data_noNAs, file="Rdata/Test_Data_noNa.Rdata")
saveRDS(test_data_noNAs, "Rdata/Test_Data_noNa.rds")
cat('Percentagem da perda de valores obtidos inicialmente do conjunto de dados original', c((nrow(fire_Test_Data) - nrow(test_data_noNAs)) / nrow(fire_Test_Data) * 100), '%')
# library(remotes)
# install_github("bczernecki/climate")
library(climate)

# get the nearest station
# be aware that the nearest station can change over time as new stations can appear
nearest_station <- nearest_stations_ogimet(country = "Portugal", 
                                        date=ymd("2014-06-01"),
                                        point = c(-8.62865, 41.16072), #Porto
                                        add_map = FALSE, 
                                        no_of_stations = 1) 



# scrap meteorological data from Ogimet regarding a period and a specific station
meteo_data <- meteo_ogimet(date="2014-06-01",
             interval="daily",station=nearest_station$wmo_id)

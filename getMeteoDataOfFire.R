
#Get data of ogimet os fires 

# install_github("bczernecki/climate")

library(climate)
library(lubridate)

for (variable in vector) {
  
}

nearest_station <- nearest_stations_ogimet(country = "Portugal", 
                                           date=ymd("2014-06-01"),
                                           point = c(-8.62865, 41.16072), #Porto
                                           add_map = FALSE, 
                                           no_of_stations = 1) 
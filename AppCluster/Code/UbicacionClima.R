library(dplyr)
library(readr)
getwd()
dir = "Data/Clima Estaciones/Clima txt"
estaciones = list.files(dir)

dirx = "Data/Clima Estaciones/Clima csv"
estacionex = list.files(dirx)

i = 1

ubicacion = read_delim(paste0(dirx,"/",estacionex[i]), 
                  ";", escape_double = FALSE, trim_ws = TRUE, 
                  n_max = 3,skip = 2)

iest = unlist(gregexpr(pattern =':',ubicacion[1,]))
ilat = unlist(gregexpr(pattern =':',ubicacion[2,]))
ilon = unlist(gregexpr(pattern =':',ubicacion[3,]))

estacion = substring(as.character(ubicacion[1,]), iest+2)
latitud = as.numeric(substring(as.character(ubicacion[2,]), ilat+2))
longitud = as.numeric(substring(as.character(ubicacion[3,]), ilon+2))




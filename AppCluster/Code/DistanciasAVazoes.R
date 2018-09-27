#Matriz de Distancias entre estaciones (Vazoes - Clima)

getwd()
load("Data/EstacionesPosicion.RData")

dist_vaz = function(vazoes_dat,clima_dat){
  #Distancia entre dos estaciones
  dist_pos = function(vaz_pos, clim_pos){
    d = sqrt(sum((vaz_pos-clim_pos)^2))
    return(d)
  }
  #Matriz de distancias hacia Vazoes
  D=matrix(0, dim(vazoes_dat)[1], dim(clima_dat)[1])
  for (i in 1:dim(vazoes_dat)[1]) {
    for(j in 1:dim(clima_dat)[1]){
      vaz_pos = as.numeric(vazoes_dat[i,c(4,5)])
      clim_pos = as.numeric(clima_dat[j,c(3,4)])
      D[i,j] = dist_pos(vaz_pos,clim_pos)
    }
  }
  colnames(D) = clima_dat$Estacion
  rownames(D) = vazoes_dat$Estacion
  return(D)
}

#Quitar vazoes con NA's !!!!
vazoes_dat2=vazoes_dat[!is.na(vazoes_dat$Latitud),]

#Distancias mínimas hacia Vazoes
D =dist_vaz(vazoes_dat2,clima_dat)

#Hallamos Nombres de estaciones de Clima Asociadas
Clima_min = apply(D,1,function(x) which.min(x))
Clima_min = colnames(D)[as.integer(Clima_min)]


BDD_min = data.frame(Estacion = vazoes_dat2$Estacion, Clima_min)

remove(D,vazoes_dat,vazoes_dat2,clima_dat,Clima_min,resum_NAs,dist_vaz)














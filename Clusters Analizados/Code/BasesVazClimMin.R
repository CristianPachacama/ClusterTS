#Recoleccion de Datos en una sola base Clima-Vazoe ---------------

dir = "Data/Clima Estaciones/Clima csv2"
estacionesExist = list.files(dir)

# Juntamos Data de Vazoes, Clima mas cercano y Cluster
VazClimClust = BDD_min %>% inner_join(Clusters) %>% arrange(Cluster)

VariablesClima = VazClimClust %>% filter(Cluster==nclu)

estacionesAux = unique(as.character(VariablesClima$Clima_min))

BDDRegresion = MedVazPca

for(k in  1:length(estacionesAux)){
  
  # k=1 #Estacion
  indEst = estacionesExist == paste0(estacionesAux[k],".csv")
  estacionK = estacionesExist[indEst]
  print(paste("**************************",estacionK,"**************************"))
  if(length(estacionK)>0){
    
    #Cargar Data de Clima asociadas al Cluster 
    source("Code/DataClima.R")
    #Limpiamos Data Clima
    source("Code/LimpiezaSTL-Loess.R")
    
    #Juntamos en una sola Base (PCAVazoe, Clim1Esta1,Clima2Esta1,....Clima6Esta1)
    LimBDDc = data.frame(LimBDDtsc)
    estacion_k = substr(estacionK,1,nchar(estacionK)-4)
    names(LimBDDc) = paste0(estacion_k,"_",names(LimBDDc))
    LimBDDc$Fecha = as.Date(LimBDDtsc)
    
    BDDRegresion = BDDRegresion %>% inner_join(LimBDDc,by="Fecha")
    
  }
  
}

#Vazoe obtenido del ACP Funcional 
fecha0r = unlist(strsplit(x = as.character(BDDRegresion$Fecha[1]) , split = "-"))[-3]
fecha0r = gsub("(?<![0-9])0+", "", fecha0r, perl = TRUE)
fecha0r = as.numeric(fecha0r)


VazoePCAts = ts(BDDRegresion$VazoePCA,start = fecha0r , frequency = 12)
# library(TSstudio)
# ts_plot(VazoePCAts)
# ts_seasonal(VazoePCAts,type="all")
# ts_lags(VazoePCAts, lags = c(12, 24, 36, 48))
# ts_heatmap(VazoePCAts)

#Removemos Variables ------------------------------------------------------
remove(VazClimClust,VariablesClima,LimBDDc,LimBDDtsc,
       estacion_k,estacionesAux,estacionK,indEst,k,BDDc)



#Grafico de Series que componen la Base de Datos  -------------------------

graf_series = function(BDDRegresion,ind = NULL,fecha = fecha0r){
  
  if(is.null(ind)){
    aux = BDDRegresion[,-1]
  }else{
    aux = BDDRegresion[,ind]
  }
  
  aux = ts(aux,start = fecha , frequency = 12)
  
  graf=dygraph(data = aux, main = "Datos Estacion Vazoes vs Clima")%>%
    # dyAxis("x", label=names(metric)) %>%
    # dyAxis('y',label='')%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(show= "follow",width = 400)
  
  return(graf)
  
}

graf_series(BDDRegresion, fecha = fecha0r)

# library(TSstudio)
# ts_seasonal(BDDtsc[,3],type="all")
# ts_lags(BDDtsc[,3], lags = c(12, 24, 36, 48))
# ts_heatmap(BDDtsc[,3])





# Indices Globales    -----------------------------------------------------------
library(readxl)
MACROclima = read_excel("Data/MACRO Clima/MACROclima.xlsx",na="NA")


MACROclima = as.data.frame(MACROclima)
FechaMacro = seq(from = as.Date("1931/1/1") ,to=as.Date("2015/12/1"),by= "month")
#Corregir para cada Cluster!!!!  -----------
MACROclimaAux = MACROclima
MACROclimaAux$Fecha = FechaMacro

BDDRegMacro = BDDRegresion %>% inner_join(MACROclimaAux[,-c(1,2)],by="Fecha")

# dim(MACROclimaAux)
# dim(BDDRegresion)


# BDDRegMacro = cbind(BDDRegresion,MACROclimaAux[,-c(1,2)])

#Grafico de Indices  -------------
graf_series(BDDRegMacro, ind= c(37:40) ,fecha = fecha0r)


#Grafico doble eje
graf_series2 = function(BDDRegresion,ind = NULL,fecha = fecha0r){
  
  if(is.null(ind)){
    aux = BDDRegresion[,-1]
  }else{
    aux = BDDRegresion[,c(2,ind)]
  }
  
  aux = ts(aux,start = fecha , frequency = 12)
  
  graf=dygraph(data = aux, main = "Datos Estacion Vazoes vs Clima")%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3) %>% 
    dySeries("VazoePCA",axis = "y2")
  
  
  return(graf)
  
}


graf_series2(BDDRegMacro, ind= c(37:40) ,fecha = fecha0r)

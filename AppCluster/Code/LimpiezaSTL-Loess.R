# devtools::install_github("hafen/stlplus")
library(stlplus)


#Carga de Datos de la Estacion  ---------------------------------------------------

# k=10 # Estacion de Clima
tendWindow = 24 #Retardos Tendencia
estacioWindow = 12   #Retardos Estacionalidad

# source("Code/DataClima.R")

# Quitar Estaciones con mas de 10% deobservaciones perdidas (2000-2015) -----------
# considerando el max de las 6 variables climaticas??? 
# O considerando por variable Climatica la quitamos del modelo.    <--


#Cuales son variables va'lidas
IndVarValid = apply(BDDtsc, MARGIN = 2, FUN = function(serie){

  PrcPerdidos = sum(is.na(serie))/192
  
  return(PrcPerdidos<=8)
})

#Quitar las No Validas
BDDtsc = BDDtsc[,IndVarValid]


#Simulacion de Distribucion Empirica de Ruido -------------------------------------
{
  #Funcion de Distribucion Empirica de los Datos (Xi)   
  DistribEmpirica = function(Lista_Xi,x=Lista_Xi){
    Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
    Distrib = c()
    
    for(i in 1:length(x)){
      Indicatriz = as.numeric(Lista_Xi <= x[i])
      Distrib[i] = mean(Indicatriz, na.rm = TRUE)
    }
    
    return(Distrib)
  }
  #Simulacion a Partir de Distribucion Empirica
  rEmpirica = function(Lista_Xi,n=1){
    Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
    if(n>0){
      u =runif(n)
      xsim=c()
      for(k in 1:n){
        xsim[k] = min(Lista_Xi[u[k]<=DistribEmpirica(Lista_Xi)],na.rm = TRUE)
      }
      
    }else{
      xsim = NA
      print("Ingrese un n adecuado")
    }
    
    return(xsim)
  }
  
}


LimpiezaSTL = function(BDDtsc,fecha0){
  
  BDDLimpia = matrix(NA,nrow = dim(BDDtsc)[1], ncol = dim(BDDtsc)[2])
  colnames(BDDLimpia) = colnames(BDDtsc)
  BDDLimpia = ts( BDDLimpia , start = fecha0[c(2,1)] , frequency = 12 )
  
  for(j in 1:dim(BDDtsc)[2]){
    
    # Extaer Residuos de las Series Clima    -------------------------------------------
    # Descomposicion STL-Plus
    SerieDescomp = stlplus(BDDtsc[,j],t.window = tendWindow,s.window = estacioWindow)
    SerieOrig = SerieDescomp$data$raw
    Tendencia = SerieDescomp$data$trend
    Estacional = SerieDescomp$data$seasonal
    Ruido = SerieDescomp$data$remainder
    print(paste("Datos Perdidos:",sum(is.na(Ruido))))
    # p0 = plot(SerieDescomp)
    
    # LLenar Residuos con Simulaciones   ---------------------------------------------
    if(sum(is.na(Ruido))>0){
      Ruido[is.na(Ruido)] = rEmpirica(Lista_Xi = Ruido , n = sum(is.na(Ruido)))
    }
    
    print(paste("Datos Perdidos (Luego de Correccion):",sum(is.na(Ruido))))
    
    #Reagregar Series Sumando Componentes    -----------------------------------------
    SerieLimp = Tendencia + Estacional + Ruido
    remove(Tendencia,Estacional,Ruido,SerieOrig)
    #Transformamos a objeto ts
    BDDLimpia[,j] = ts(SerieLimp, start = fecha0[c(2,1)] ,frequency = 12)
    
  }
  return(BDDLimpia)
}


#Ejecutar Limpieza-STL --------------------
LimBDDtsc= LimpiezaSTL(BDDtsc,fecha0)

# Graficas
print(plot(stlplus(BDDtsc[,4], t.window = tendWindow , s.window = estacioWindow)))
print(plot(stlplus(LimBDDtsc[,4], t.window = tendWindow , s.window = estacioWindow)))




#Removemos Variables  ----------------------
remove(IndVarValid)






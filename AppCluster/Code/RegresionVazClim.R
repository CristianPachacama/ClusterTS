source("Code/ACP Funcional.R")
dir = "Data/Clima Estaciones/Clima csv2"
estacionesExist = list.files(dir)
# Juntamos Data de Vazoes, Clima mas cercano y Cluster
VazClimClust = BDD_min %>% inner_join(Clusters) %>% arrange(Cluster)
nclu
VariablesClima = VazClimClust %>% filter(Cluster==nclu)

estacionesAux = unique(as.character(VariablesClima$Clima_min))
 

k=1 #Estacion
indEst = estacionesExist == paste0(estacionesAux[k],".csv")
estacionK = estacionesExist[indEst]

#Cargar Data de Clima asociadas al Cluster 
source("Code/DataClima.R")
#Limpiamos Data Clima
source("Code/LimpiezaSTL-Loess.R")

#Juntamos en una sola Base (PCAVazoe, Clim1Esta1,Clima2Esta1,....Clima6Esta1)
LimBDDc = data.frame(LimBDDtsc)
LimBDDc$Fecha = as.Date(LimBDDtsc)


BDDRegresion = MedVazPca %>% inner_join(LimBDDc,by="Fecha")

regFit = lm(data=BDDRegresion, VazoePCA ~ PrecipitacaoTotal+TempMaximaMedia+TempMinimaMedia+UmidadeRelativaMedia) 
summary(regFit)

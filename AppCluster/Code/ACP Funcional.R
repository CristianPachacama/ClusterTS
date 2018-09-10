library(fpca)
library(fdapace)
library(readr)
library(reshape2)
source("Code/DistanciasAVazoes.R")

#Cargamos Datos Analizados en Plataforma Cluster Mensual
load("Data/Actual/DataVazoes.RData")
Clusters = read_csv("Analizado/Clusters.csv",locale = locale(encoding = "UTF-8"))
NClusters = unique(Clusters$Cluster)
Clusters = Clusters %>% select(Estacion,Cluster)

# Filtramos Series por Cluster   --------------------------------------------

nclu = NClusters[1]

EstacionesClus = Clusters %>% filter(Cluster == nclu) %>% select(Estacion,Cluster)

#Acortar Series (Considerando solo 120 es decir 10 anios)
BDDv_corta = BDDv[(dim(BDDv)[1]-240):(dim(BDDv)[1]),]
fechasV = as.Date(BDDtsv)
BDDv_corta$Fecha = fechasV[(dim(BDDv)[1]-240):(dim(BDDv)[1])]

SeriesClus = melt(BDDv_corta[,EstacionesClus$Estacion])
SeriesClus$Tiempo = 1:(dim(BDDv_corta)[1])
names(SeriesClus) = c("Estacion","Vazoe","Tiempo")

#Pasamos a un objeto FPCA
VazPca = MakeFPCAInputs(IDs = SeriesClus$Estacion,tVec = SeriesClus$Tiempo,yVec = SeriesClus$Vazoe)
FitVazPca =  FPCA(VazPca$Ly, VazPca$Lt, list(plot = TRUE, methodMuCovEst = 'smooth', userBwCov = 20 , useBinnedData="OFF"))

MedVazPca = data.frame(Fecha=BDDv_corta$Fecha , VazoePCA = FitVazPca$mu)













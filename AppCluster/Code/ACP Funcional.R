library(fpca)
library(fdapace)
library(readr)
library(reshape2)
# source("Code/DistanciasAVazoes.R")

#Cargamos Datos Analizados en Plataforma Cluster Mensual
load("Data/Actual/DataVazoes.RData")
Clusters = read_csv("Analizado/Clusters.csv",locale = locale(encoding = "UTF-8"))
# NClusters = unique(Clusters$Cluster)
Clusters = Clusters %>% select(Estacion,Cluster)

# Filtramos Series por Cluster   --------------------------------------------

# nclu =1

EstacionesClus = Clusters %>% filter(Cluster == nclu) %>% select(Estacion,Cluster)

#Acortar Series (Considerando solo 240 es decir 20 anios) !!!!  --------------

lagsACP = 240  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

BDDv_corta = BDDv[(dim(BDDv)[1]-lagsACP):(dim(BDDv)[1]),]
fechasV = as.Date(BDDtsv)
BDDv_corta$Fecha = fechasV[(dim(BDDv)[1]-lagsACP):(dim(BDDv)[1])]

SeriesClus = melt(BDDv_corta[,EstacionesClus$Estacion])
SeriesClus$Tiempo = 1:(dim(BDDv_corta)[1])
names(SeriesClus) = c("Estacion","Vazoe","Tiempo")

#Pasamos a un objeto FPCA
VazPca = MakeFPCAInputs(IDs = SeriesClus$Estacion,tVec = SeriesClus$Tiempo,yVec = SeriesClus$Vazoe)

# ACP Funcional  -----------------------
FitVazPca =  FPCA(VazPca$Ly, VazPca$Lt, list(plot = TRUE, methodMuCovEst = 'smooth', userBwCov = 20 , useBinnedData="OFF"))
plot(FitVazPca)
MedVazPca = data.frame(Fecha=BDDv_corta$Fecha , VazoePCA = FitVazPca$mu)


#Graficos de Presicion del modelo
par(mfrow=c(1,2))
CreatePathPlot(FitVazPca, subset = c(4,5), main = 'K = 11', pch = 4); grid()
CreatePathPlot(FitVazPca, subset = c(3,5,10), K = 3, main = 'K = 3', pch = 4) ; grid()


#Removemos variables -------------------
remove(EstacionesClus,SeriesClus,VazPca,BDDv,BDDv_corta,fechasV)
# remove(EstacionesClus,SeriesClus,VazPca,BDDv,BDDv_corta,BDDtsv,fechasV)







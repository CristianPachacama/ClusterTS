#------------------------     Cluster 2: Modelo Final     --------------------------
library(dplyr)

library(xts)
library(TSA)
library(forecast)

library(ggplot2)
library(dygraphs)
library(TSstudio)

library(lubridate)
library(lmtest)

#Se elije Cluster y se compila codigo extra  ------------------------------
nclu = 1
source("Code/DistanciasAVazoes.R")
source("Code/ACP Funcional.R")
source("Code/BasesVazClimMin.R")
source("Code/BddMacro.R")

#SARIMAX con MACRO CLima --------------------------------------
particion = 0.20 #Particion 20% - 80%

ntr = round(particion*dim(BDDRegMacro)[1])
indTrain = -c((dim(BDDRegMacro)[1]-ntr):(dim(BDDRegMacro)[1]))
BDDRegMacroTS = ts(BDDRegMacro,start=fecha0r,frequency = 12)
# BDDRegMacroTS =BDDRegMacro

BDDtrain = ts(BDDRegMacroTS[indTrain ,],start=fecha0r,frequency = 12)
BDDtest = BDDRegMacroTS[-indTrain ,]

#SELECCION DE VARIABLES  -------------------------------------
(VarXsMacro = names(BDDRegMacro)[c(4,5,6,8,16,17,18,36)])

(VarXsMacro = names(BDDRegMacro)[c(6)])

(VarXsMacro = names(BDDRegMacro)[c(37,38)])

X  = BDDtrain[,c(VarXsMacro)]
VazTrain = ts(VazoePCAts[indTrain],start=fecha0r,frequency = 12)
# VazTrain = ts(BDDtsv[(12*60):(12*60+126),1],start=fecha0r,frequency = 12)
# VazActual = ts(BDDtsv[(12*60):(12*60+159),1],start=fecha0r,frequency = 12)

#Modelo usando funcion Arima
# ArimaF = Arima
# arimaf = arimax
modeloClust = Arima(y= log(VazTrain),
                    order = c(p=6,d=0,q=2),
                    seasonal = list(order = c(1,1,0), period = 12),
                    xreg = X
)

summary(modeloClust)
coeftest(modeloClust)
checkresiduals(modeloClust)
prediccion = forecast(modeloClust,xreg = BDDtest[,c(VarXsMacro)])
# accuracy(prediccion)
autoplot(prediccion)
test_forecast(actual = log(VazoePCAts), forecast.obj = prediccion, test = BDDtest[,"VazoePCA"])

# test_forecast(actual = log(VazActual), forecast.obj = prediccion, test = BDDtest[,c(VarXsMacro)])








#Variante del modelo (arima) Admite Estacionalidad (No Vale AUTOPLOT ni FORECAST)
# (VarXsMacro = names(BDDRegMacro)[c(6,7,11)])
# X  = BDDtrain[,c(VarXsMacro)]
# modeloClust = TSA::arima(x= VazoePCAts[indTrain],
#                               order = c(p=4,d=0,q=1),
#                               seasonal = list(order= c(P=1,D=1,Q=0),period=12),
#                               xreg = X ,
#                               include.mean = T
# )
# summary(modeloClust)
# coeftest(modeloClust, xreg = )
# checkresiduals(modeloClust)
# prediccion = forecast(modeloClust,xreg = BDDtest[,c(VarXsMacro)])
# autoplot(prediccion)

#Grafico de Predicciones





















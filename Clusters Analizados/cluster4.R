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
(VarXsMacro = names(BDDRegMacro)[c(4,5,6,8,16,17,18)])
X  = BDDtrain[,c(VarXsMacro)]
VazTrain = ts(VazoePCAts[indTrain],start=fecha0r,frequency = 12)
#Modelo usando funcion Arima
modeloClust = Arima(y= VazTrain,
                    order = c(p=4,d=0,q=1),
                    seasonal = list(order = c(1,1,0), period = 12),
                    xreg = X
)

summary(modeloClust)
coeftest(modeloClust)
checkresiduals(modeloClust)
prediccion = forecast(modeloClust,xreg = BDDtest[,c(VarXsMacro)])
# accuracy(prediccion)
autoplot(prediccion)

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

test_forecast(actual = VazoePCAts, forecast.obj = prediccion, test = BDDtest[,c("VazoePCA")])




















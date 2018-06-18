library(tidyverse)
library(dygraphs)
library(lubridate)
library(xts)
library(TSclust)
library(TSdist)
library(cluster)
library(reshape2)

load("Data/Base_Consolidada.RData")

BDD_vaz = BDD_full[,1:6]
names(BDD_vaz)[1] = "Anio"

BDD = dcast(data= BDD_vaz, formula= Anio + Mes ~ Usina,value.var = "Vazoes" )









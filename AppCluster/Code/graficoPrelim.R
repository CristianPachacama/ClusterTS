library(tidyverse)
library(lubridate)
library(xts)
library(TSclust)
library(TSdist)
library(cluster)

library(reshape2)
library(dygraphs)
load("Data/Base_Consolidada.RData")

BDD_vaz = BDD_full[,1:6]
names(BDD_vaz)[1] = "Anio"

BDDv = reshape2::dcast(data= BDD_vaz, formula= Anio + Mes ~ Usina,value.var = "Vazoes" )
remove(BDD_vaz,BDD_full)
BDDtsv = ts(BDDv[,-c(1,2)],start = c(1931,1),frequency = 12)

dygraphs::dygraph(BDDtsv[,1:3]) %>% 
  dyRangeSelector(dateWindow = c("2000-01-01", "2015-12-01"))%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6,
              hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth = 1.5))


getwd()
save(BDDv,BDDtsv,file = "Data/DataVazoes.RData")

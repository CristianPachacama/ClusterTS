library(dplyr)
library(readr)
library(dygraphs)
# dir = "Data/Clima Estaciones/Clima csv2"
# estacionesExist = list.files(dir)

# k=1
#Clima de la Estacion K-esima
BDDc = read_delim(paste0(dir,"/",estacionK), #,".csv"), 
                  ";", escape_double = FALSE, trim_ws = TRUE, 
                  skip = 16)

fecha0 = unlist(strsplit(BDDc$Data[1],"/"))[-1]
fecha0 = gsub("(?<![0-9])0+", "", fecha0, perl = TRUE)
fecha0 = as.numeric(fecha0)
# fecha0 = c(2000,1)
BDDtsc = ts(BDDc[,-c(1:3,8)],start = fecha0[c(2,1)] , frequency = 12)

# dygraphs::dygraph(BDDtsc[,1:4]) %>% 
#   dyRangeSelector(dateWindow = c("1980-01-01", "2015-12-01"))%>%
#   dyHighlight(highlightCircleSize = 5, 
#               highlightSeriesBackgroundAlpha = 0.6,
#               hideOnMouseOut = FALSE,
#               highlightSeriesOpts = list(strokeWidth = 1.5))
 


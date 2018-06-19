#Grafico Conjunto de Vazoe y Clima (Percip, Temp)

BDDcons = cbind(BDDv[829:1020,4],BDDc[,4:7])
BDDcons = ts(BDDcons,start = c(2000,1),frequency = 12)
dygraphs::dygraph(BDDcons) %>% 
  dyRangeSelector(dateWindow = c("2000-01-01", "2015-12-01"))%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6,
              hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth = 1.5))
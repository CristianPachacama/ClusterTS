library(dplyr)
library(readr)
library(dygraphs)

dir = "Data/Clima Estaciones/Clima csv"
estaciones = list.files(dir)

BDDc = read_delim(paste0(dir,"/",estaciones[10]), 
                  ";", escape_double = FALSE, trim_ws = TRUE, 
                  skip = 16)

BDDtsc = ts(BDDc[,-(1:3)],start = c(2000,1),frequency = 12)

dygraphs::dygraph(BDDtsc[,1:3]) %>% 
  dyRangeSelector(dateWindow = c("2000-01-01", "2015-12-01"))%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6,
              hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth = 1.5))
 


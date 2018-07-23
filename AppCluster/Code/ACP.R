library(reshape2)
library(dygraphs)
library(TSdist)
library(TSclust)
library(smacof)
library(cluster)
library(factoextra)

load("Data/Base_Consolidada.RData")

BDD_vaz = BDD_full[,1:6]
names(BDD_vaz)[1] = "Anio"

# Series de Tiempo Vazoes
BDDv = reshape2::dcast(data= BDD_vaz, formula= Anio + Mes ~ Usina,value.var = "Vazoes" )
remove(BDD_vaz,BDD_full)
BDDtsv = ts(BDDv[,-c(1,2)],start = c(1931,1),frequency = 12)

# Matriz de Distancias (Autocorrelación)

MatrDist = function(BDDv){
  N = dim(BDDv)[2] - 2
  D = matrix(0,nrow = N, ncol = N)
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      D[i,j]=ACFDistance(BDDv[,2+i],BDDv[,2+j])
    }
  }
  D=D+t(D)
  return(D)
}

Dvaz = MatrDist(BDDv)

# Escalonamiento Multidimensional 
mds = smacofSym(Dvaz)

#Cluster
k = 3
clus = clara(x = mds$conf,k)

#Grafico Cluster
fviz_nbclust(mds$conf, clara, method = "silhouette")+  theme_classic()

fviz_cluster(clus, 
             palette = c("#00AFBB", "#FC4E07","#09D843","#9820A0","#2C68F6","#F67B00"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)

#Grafico Series por cluster
clus$clustering

j = 3 #Cluster 1 al 6

ind= as.numeric(names(clus$clustering)[clus$clustering == j])

dygraphs::dygraph(BDDtsv[,ind]) %>% 
  dyRangeSelector(dateWindow = c("2000-01-01", "2015-12-01"))%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6,
              hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth = 1.5))

#ACP del cluster
acp = prcomp(BDDtsv[,ind])

componentes = acp$x

plot(componentes[,1],type= "l")
compTs = ts(3500-componentes[,1],start = c(1931,1),frequency = 12)

dygraphs::dygraph(compTs) %>% 
  dyRangeSelector(dateWindow = c("2000-01-01", "2015-12-01"))%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6,
              hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth = 1.5))




# ANALISIS CLUSTERIZACION (MAPA) ======================

# TSDist
# Distance measure to be used. It must be one of: "euclidean", 
# "manhattan", "minkowski", "infnorm", "ccor", "sts", "dtw", 
# "keogh.lb", "edr", "erp", "lcss", "fourier", "tquest", 
# "dissim", "acf", "pacf", "ar.lpc.ceps", "ar.mah", 
# "ar.mah.statistic", "ar.mah.pvalue", "ar.pic", "cdm", 
# "cid", "cor", "cort", "wav", "int.per", "per", "mindist.sax", 
# "ncd", "pred", "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet"

DistMatrix = function(BDD=BDDv,Metrica= "acf"){
  N = dim(BDD)[2] - 2
  D = matrix(0,nrow = N, ncol = N)
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      D[i,j]=TSDistances(x=BDD[,2+i],y=BDD[,2+j],distance = Metrica)
    }
  }
  D=D+t(D)
  return(D)
  
}
  
#Funcion Extra ------
cluster_geografico = function(D,k=4,tipo='kmedias',vazoes_code){
  k_aux<-k
  map <- smacofSym(D)
  fit <- switch(tipo,
                'kmedias' = {kmeans(map$conf, centers = k_aux)},
                'clara' = {clara(map$conf, k=k_aux, samples = 50, stand=TRUE, pamLike = TRUE)},
                'gerarquico' = {hclust(D)}
  )
  grupo <- switch (tipo,
                   'kmedias' = {fit$cluster},
                   'clara' = {fit$cluster},
                   'gerarquico' = {cutree(fit, k = k_aux)}
  )
  D1<-as.numeric(map$conf[,'D1']);
  D2<-as.numeric(map$conf[,'D2']);
  
  BDD_cluster<-data.frame(Nombre_ST = paste0('VAZOES(',vazoes_code$Estacion,')'),
                          Estacion=vazoes_code$Estacion,
                          Codigo_ONS=vazoes_code$Codigo_ONS,
                          Latitud=vazoes_code$Latitud,
                          Longitud=vazoes_code$Longitud,
                          D1, D2, Cluster=as.factor(as.character(grupo))
  )
  
  BDD_cluster<-data.frame(Nombre_ST = paste0('VAZOES(',vazoes_code$Estacion,')'),
                          Estacion=vazoes_code$Estacion,
                          Codigo_ONS=vazoes_code$Codigo_ONS,
                          Latitud=vazoes_code$Latitud,
                          Longitud=vazoes_code$Longitud,
                          D1, D2, Cluster=as.factor(as.character(grupo))
  )
  if(k_aux >= 1){
    est_cluster1<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==1])
  }
  if(k_aux >=2){
    est_cluster2<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==2])
  }
  if(k_aux >=3){
    est_cluster3<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==3])
  }
  if(k_aux >=4){
    est_cluster4<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==4])
  }
  if(k_aux >=5){
    est_cluster5<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==5])
  }
  if(k_aux >=6){
    est_cluster6<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==6])
  }
  if(k_aux >=7){
    est_cluster7<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==7])
  }
  if(k_aux>=8){
    est_cluster8<-as.character(BDD_cluster$Nombre_ST[BDD_cluster$Cluster==8])
  }
  if(k_aux > 8){
    print('Valor de k demasiado grande, elige un menor número de clusters')
  }
  
  resultado <- switch (k_aux,
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5,'est_cluster6' = est_cluster6)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5,'est_cluster6' = est_cluster6,
                             'est_cluster7' = est_cluster7)},
                       {list('map'=map,'grupo'=grupo,'BDD_cluster'=BDD_cluster,
                             'est_cluster1' = est_cluster1,'est_cluster2' = est_cluster2,
                             'est_cluster3' = est_cluster3,'est_cluster4' = est_cluster4,
                             'est_cluster5' = est_cluster5,'est_cluster6' = est_cluster6,
                             'est_cluster7' = est_cluster7,'est_cluster8' = est_cluster8)}
  )
  
  return(resultado)
  
}

#Genera Data para Mapa ---------
clus_dat<-eventReactive(input$vaz_clus_boton,{
  D_aux <- switch(input$vaz_clus_metric,
                  'D_ccor'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'ccor'),
                  'D_cor'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'cor'),
                  'D_cort'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'cort'),
                  'D_acf'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'acf'),
                  'D_euc'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'euc'),
                  'D_fourier'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'fourier'),
                  'D_ifnrm'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'infnorm'),
                  'D_manh'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'manhattan'),
                  'D_mink'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'minkowski'),
                  'D_pacf'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'pacf'),
                  'D_per'=DistMatrix(BDDv[c(-1,-2),],Metrica = 'per')
  )
  
  aux_cluster <- cluster_geografico(D=D_aux,k=as.numeric(input$vaz_clus_k),
                                    tipo=input$vaz_clus_metod,
                                    vazoes_code)
  
  return(aux_cluster$BDD_cluster)
})

# Mapa Cluster ----------
output$mapa_cluster <- renderLeaflet({
  
  BDD_cluster <- clus_dat()
  # Etiquetas de Estaciones
  burbuja_clust <- paste(sep = "<br/>",
                         "<b><a href='-'>Tipo: </a></b>Vazoes",
                         paste0("<b><a href='-'>Estación: </a></b>", BDD_cluster$Estacion),
                         paste0("<b><a href='-'>Longitud: </a></b>", BDD_cluster$Longitud),
                         paste0("<b><a href='-'>Latitud: </a></b>" ,BDD_cluster$Latitud),
                         paste0("<b><a href='-'>Cluster: </a></b>" , BDD_cluster$Cluster)
  )
  
  #Modificar colorde Marcadores para estaciones
  getColor_clus <- function(BDD_cluster) {  
    #La funcion crea un vector con colores basado en una condicion
    sapply(BDD_cluster$Cluster, function(Cluster) {
      if(Cluster == 1) {
        "blue"
      } else if(Cluster == 2) {
        "orange"
      } else if(Cluster == 3) {
        "green"
      } else if(Cluster == 4) {
        'purple'
      } else if(Cluster == 5) {
        "gray"
      } else if(Cluster == 6) {
        "red"
      } else if(Cluster == 7) {
        "lightgray"
      } else if(Cluster == 8) {
        "darkblue"
      } else{
        NA
      }
      
    })
  }
  
  icons_clus <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor_clus(BDD_cluster) 
  )
  
  #Generacion de Mapa (Leaflet)
  
  leaflet() %>% addTiles() %>%
    
    #Marcadores: 
    addAwesomeMarkers(lng=BDD_cluster$Longitud,lat=BDD_cluster$Latitud,#group ='Clima',
                      icon=icons_clus,popup=burbuja_clust)%>%
    #Añadir Minimapa
    addMiniMap(position='bottomright',toggleDisplay = TRUE)
  
})

#Tabla de Estaciones por Cluster -----
output$tabla_cluster <- renderDataTable(server = FALSE,
                                        datatable(filter = 'top',
                                          extensions = c('Buttons'), #c('Responsive','Buttons'),
                                          options = list(pageLength=7,searchHighlight = TRUE,
                                            dom = 'Bfrtip',
                                            buttons = list('copy','print', list(
                                              extend = 'collection',
                                              buttons = c('csv', 'excel', 'pdf'),
                                              text = 'Descargar'
                                            ))
                                          ),
                                          {
                                            aux<-clus_dat() %>% arrange(Cluster)
                                            aux[,c(-1,-3)]
                                          })
)
#Grafico de Series (CLUSTER) ------
serie_cluster <- eventReactive(input$vaz_clu_grf_boton,{
  est_selec<-input$tabla_cluster_rows_selected
  # aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
  aux<-BDDtsv[,est_selec]
  return(aux)
  
})

output$vaz_clu_grf <- renderDygraph({
  aux<-serie_cluster()
  dygraph(aux)%>% 
    dyRangeSelector(dateWindow = c('2000-01-01','2015-12-31'))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})


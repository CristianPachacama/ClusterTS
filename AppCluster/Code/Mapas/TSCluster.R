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
  
  
clus_dat<-eventReactive(input$vaz_clus_boton,{
  D_aux <- switch(input$vaz_clus_metric,
                  'D_ccor'=DistMatrix(BDDv,Metrica = 'ccor'),
                  'D_cor'=DistMatrix(BDDv,Metrica = 'cor'),
                  'D_cort'=DistMatrix(BDDv,Metrica = 'cort'),
                  'D_acf'=DistMatrix(BDDv,Metrica = 'acf'),
                  'D_euc'=DistMatrix(BDDv,Metrica = 'euc'),
                  'D_fourier'=DistMatrix(BDDv,Metrica = 'fourier'),
                  'D_ifnrm'=DistMatrix(BDDv,Metrica = 'infnorm'),
                  'D_manh'=DistMatrix(BDDv,Metrica = 'manhattan'),
                  'D_mink'=DistMatrix(BDDv,Metrica = 'minkowski'),
                  'D_pacf'=DistMatrix(BDDv,Metrica = 'pacf'),
                  'D_per'=DistMatrix(BDDv,Metrica = 'per')
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
                                                  options = list(pageLength=7,searchHighlight = TRUE),{
                                                    aux<-clus_dat()
                                                    aux[,c(-1,-3)]
                                                  })
)
#Grafico de Series (CLUSTER) ------
serie_cluster <- eventReactive(input$vaz_clu_grf_boton,{
  est_selec<-input$tabla_cluster_rows_selected
  # aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
  aux<-vazoes_cluster_ts[,est_selec]
  return(aux)
  
})

output$vaz_clu_grf <- renderDygraph({
  aux<-serie_cluster()
  dygraph(aux)%>% 
    dyRangeSelector(dateWindow = c('2010-01-01','2015-12-31'))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
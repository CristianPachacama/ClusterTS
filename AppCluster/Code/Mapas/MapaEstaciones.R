# --------------- MAPA ESTACIONES VARIAS ------------------

output$mapa1 <- renderLeaflet({
  
  leaflet() %>% addTiles() %>%
    
    #Marcadores: Clima=Azul,Vazoes=Naranja
    # addAwesomeMarkers(lng=clima_dat$Longitud,lat=clima_dat$Latitud,group ='Clima',icon=icons_clim,label=clima_dat$Estacion,popup=htmlEscape(clima_dat$Estacion))%>%
    # addAwesomeMarkers(lng=vazoes_dat$Longitud,lat=vazoes_dat$Latitud,group ='Vazoes',icon=icons_vaz,label=vazoes_dat$Estacion,popup=htmlEscape(vazoes_dat$Estacion))%>%
    
    addAwesomeMarkers(lng=clima_dat$Longitud,lat=clima_dat$Latitud,group ='Clima',icon=icons_clim,popup=burbuja_clima)%>%
    addAwesomeMarkers(lng=vazoes_dat$Longitud,lat=vazoes_dat$Latitud,group ='Vazoes',icon=icons_vaz,popup=burbuja_vazoe)%>%
    
    #Circulos (Usinas): Rojo
    addCircles(lng = usinas_dat$Longitud, lat =usinas_dat$Latitud,group ='Usinas Principales',weight = 1,color=pal2(usinas_dat$Capacidad),
               radius =5*(usinas_dat$Capacidad)^1.2 , popup =burbuja_usina 
    ) %>%
    #Agregar Controles
    addLayersControl(
      overlayGroups = c("Clima", "Vazoes",'Usinas Principales'),
      options = layersControlOptions(collapsed = FALSE)
    )%>%
    addLegend(position='bottomleft',values=usinas_dat$Capacidad,
              pal=pal2,title='Capacidad Usina:')%>%
    addMiniMap(position='bottomright',toggleDisplay = TRUE)
  
})
#Descarga de Paquetes ================================
# (Solo compilar esta sección la primera vez)
# install.packages('shinythemes',dependencies = T)
# install.packages('shinydashboard',dependencies = T)
# install.packages('markdown',dependencies = T)
# install.packages('leaflet',dependencies = T)
# install.packages('htmltools',dependencies = T)
# install.packages('rgdal',dependencies = T)
# install.packages('DT',dependencies = T)
# install.packages('plotly',dependencies = T)
# install.packages('ggplot2',dependencies = T)
# install.packages('dygraphs',dependencies = T)
# install.packages('seasonal',dependencies = T)
# install.packages('stlplus',dependencies = T)

#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# devtools::install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------   ANALISIS CLUSTER Mensual (Jul 2018)   ------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
#library(shinythemes)
#library(shinydashboard)
#library(markdown)
#Mapas
library(leaflet)
library(htmltools)
library(rgdal)
#Tablas
library(DT)
library(dplyr)
#Series de Tiempo
library(TSdist)
library(dygraphs)
library(xts)
#Mutivariante
library(smacof)
library(cluster)
#>> Carga de Datos
#Data Antigua
# load('Data/Antigua Data/datos_interfaz.RData')
# load('Data/Antigua Data//Vazoes_Cluster.RData')
#Nueva Data
load('Data/Actual/InterfazMes.RData')
load('Data/Actual/DataVazoes.RData')
load("Data/Actual/VazoesCode.RData")
# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================

ui <- navbarPage(title = "Tesis",
                 header = tags$h2("Header-Plataforma",tags$head(tags$link(rel='shortcut icon', 
                                                                          href='epn.ico', 
                                                                          type='image/x-icon'))),
                 position = "fixed-top",#theme=shinytheme('flatly'),#theme = 'estilo.css',
                 footer = fluidRow(column(12,img(src='epn_logo.png',width='30px',align='center'),
                                          tags$b('Proyecto: '),' "Extreme low Levels of setreamflow in Hydropower Plants".' ,
                                          '-',tags$a('Departamento de Matemática - EPN (2018)',href='http://www.epn.edu.ec'),
                                          tags$b('  ||  '),tags$b('Desarrollado por: '),
                                          tags$a('Cristian Pachacama',href='http://www.linkedin.com/in/cristian-david-pachacama')
                 )
                 ),
                 
                 #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
                 tabPanel('Introducción',icon=icon('home'),
                          
                          fluidRow(
                            
                            sidebarPanel(img(src='epn_logo2.png',width='90%',align='center' ),
                                         fluidRow(' '),
                                         hr(),
                                         fluidRow(
                                           column(3,tags$b('Proyecto:')),column(1),
                                           column(8,'Forecast and Impact of extreme low levels of streamflow in hydropower plants.')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Código:')),column(1),
                                           column(8,'PIS-16-14')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Linea de Investigación:')),column(1),
                                           column(8,'Modeloamiento Estadístico')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Departamento:')),column(1),
                                           column(8,'Matemática')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Directora:')),column(1),
                                           column(8,'PhD. Adriana Uquillas')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Researcher:')),column(1),
                                           column(8,'PhD. Meitner Cadena')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Assistant:')),column(1),
                                           column(8,'Cristian Pachacama')
                                         )
                                         
                                         
                            ),
                            
                            mainPanel(
                              h3('Forecast and Impact of extreme low levels of streamflow in hydropower plants.'),
                              hr(),h4('Resume:'),
                              fluidRow(' '),
                              p('The development of a methodology to valuate hydrologic critical situations 
                                is critical given that the availability of water, profoundly affects the service 
                                conditions of the consumer energy market, mainly in the countries where the 
                                predominance energy sector is hydraulic.'),
                              p('In this project, we will propose a new approach for modeling extreme low levels 
                                of streamflow in hydropower plants. The Brazilian electricity system has the 
                                peculiarity that most of its energy is generated using renewable sources, 
                                mainly by hydroelectric plants (as also in Ecuador). 
                                Moreover, due to the continental dimensions of Brazil and the influences of many 
                                different climatological patterns, there is the possibility to develop a deep 
                                search in operation planning of hydroelectric plants.For this reason, it provides a representative sample of the difficulties for 
                                operating a predominantly renewable basis energy matrix. To deal with that kind 
                                of problems of Brazilian case we have the support of PhD Ildo Sauer, research 
                                professor of the Sao Paulo University, who is an expert in energy planning, demand 
                                models, resources and energy supply, regulation, control and energy policies. 
                                In this project, we will benefit of him experience in order to replicate these 
                                studies to the Ecuadorian case in next researches.'),
                              p('In the theoretical aspect, the modeling of future inflows will be made via Extreme 
                                Value Theory, because in the situation of climate changes the deviation of the 
                                mean-variance models increase significantly, resulting in periods with risk of deeper 
                                droughts. This, may, eventually, lead to situations in which the supply capacity is 
                                less than the demand, leading even rationing (Brazil had a rationing in 2002). 
                                It is then clear that the ability to accurately predict extreme low levels of flow 
                                rates and to make early warnings of these events is an important tool for the 
                                operation of the electric sectors with   predominance of hydraulic energy.'),
                              p('There are evidences that exist a causal relationship between the streamflow and the 
                                large-scale phenomena such as El Niño where the climate information is incorporated 
                                in a systematic way for decision making in water resources. However, this evidences 
                                and techniques used in its analysis do not consider extreme events caused by critical 
                                climate changes that are the objective of this study, where we propose the study of 
                                extreme low water levels in hydropower plants incorporating temporal, spatial and 
                                weather couplings that directly affect the operational planning.')
                              
                              
                            )
                            
                            
                            
                          ),hr()
                          
                          
                 ),
                 
                 #INFORMACIÓN DE LA BASE DE DATOS ------------------------------
                 tabPanel("Datos",
                            leafletOutput("mapa1",width = "100%",height = "450px"),hr()
                            ),
                 
                 # ANALISIS MULTIVARIANTE DE SERIES ============================
                 tabPanel('Multivariante',
                          
                          fluidRow(
                            
                            sidebarPanel(
                              h4('Cluster de Series de Tiempo'),
                              p('Primero selecciona una de las Métricas definidas para series de tiempo.'),
                              selectInput('vaz_clus_metric', label= 'Selecciona Métrica',selected = 'D_ccor',
                                          list('Correlación Cruzada'='D_ccor',
                                               'Autocorrelación'='D_acf',
                                               'Correlación de Pearson'='D_cor',
                                               'Correlación Temporal'='D_cort',
                                               'Métrica Euclidea'='D_euc',
                                               'Métrica de Fourier'='D_fourier',
                                               'Métrica Infinito'='D_ifnrm',
                                               'Métrica Manhatan'='D_manh',
                                               'Métrica de Minkwoski'='D_mink',
                                               'Autocorrelación Parcial'='D_pacf',
                                               'Periodograma'='D_per')),
                              p('Luego elige un método de clusterización (agrupamiento).'),
                              selectInput('vaz_clus_metod', label= 'Selecciona Método',selected = 'clara',
                                          list('K-Medias'='kmedias','K-Medoid (CLARA)'='clara','Cluster Gerárquico'='gerarquico')),
                              p('Finalmente elige el número de clusters que quieres que se formen.'),
                              selectInput('vaz_clus_k', label= 'Número de Clusters',c(2:8),selected = 4),
                              actionButton('vaz_clus_boton',label='Clusterizar',icon = icon('braille')),hr(),
                              h4('Gráfico de Series'),
                              p('Para graficar una o varias series, primero clusteriza las estaciones, luego
                                  seleccione los nombres de las estaciones correspondientes en la Tabla que 
                                  se encuentra en la parte inferior derecha'),
                              actionButton('vaz_clu_grf_boton',label='Graficar',icon = icon('line-chart'))
                            ),
                            mainPanel(
                              h3('Mapa de Estaciones Clusterizadas: Vazoes '),hr(),
                              leafletOutput("mapa_cluster",width = "100%",height = "450px"),hr(),
                              h4('Tabla de Estaciones por Cluster'),
                              fluidRow(dataTableOutput("tabla_cluster", width = "50%")),hr(),
                              dygraphOutput('vaz_clu_grf')
                            )
                          ),hr()
                 )
                          

)


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  
  source("Code/Mapas/MapaEstaciones.R",local = TRUE)
  source("Code/Mapas/TSCluster.R",local = TRUE)
  
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)


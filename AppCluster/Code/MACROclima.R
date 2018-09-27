#Carga de Datos ----
{
  library(readxl)
  
  #<-read_xlsx("C:/Users/TOSHIBA/Desktop/SharePoint/ADRIANA UQUILLAS ANDRADE/Proyecto Semilla Energia Streamflow/Datos/BDD_descargadas/BDD_xsl/.xlsx")
  
  AA<-read_xlsx("Data/MACRO Clima/AA.xlsx")
  
  
  AAO<-read_xlsx("Data/MACRO Clima/AAO.xlsx")
  
  
  AMM_sst<-read_xlsx("Data/MACRO Clima/AMM_sst.xlsx")
  
  
  AMM_wind<-read_xlsx("Data/MACRO Clima/AMM_wind.xlsx")
  
  
  AMO<-read_xlsx("Data/MACRO Clima/AMO.xlsx")
  
  
  AO<-read_xlsx("Data/MACRO Clima/AO.xlsx")
  
  
  Enas_Bacias<-read_xlsx("Data/MACRO Clima/Enas_Bacias.xlsx")
  
  
  Enas_Subsistemas<-read_xlsx("Data/MACRO Clima/Enas_Subsistemas.xlsx")
  
  
  MEI<-read_xlsx("Data/MACRO Clima/MEI.xlsx")
  
  
  MEI_Ext<-read_xlsx("Data/MACRO Clima/MEI_Ext.xlsx")
  
  
  MEI_Rank<-read_xlsx("Data/MACRO Clima/MEI_Rank.xlsx")
  
  
  MJO<-read_xlsx("Data/MACRO Clima/MJO.xlsx")
  
  
  MODOKI<-read_xlsx("Data/MACRO Clima/MODOKI.xlsx")
  
  
  NAO<-read_xlsx("Data/MACRO Clima/NAO.xlsx")
  
  
  Nino3<-read_xlsx("Data/MACRO Clima/Nino3.xlsx")
  
  
  Nino4<-read_xlsx("Data/MACRO Clima/Nino4.xlsx")
  
  
  Nino12<-read_xlsx("Data/MACRO Clima/Nino12.xlsx")
  
  
  Nino34<-read_xlsx("Data/MACRO Clima/Nino34.xlsx")
  
  
  ONI<-read_xlsx("Data/MACRO Clima/ONI.xlsx")
  
  
  PDO<-read_xlsx("Data/MACRO Clima/PDO.xlsx")
  
  
  QBO<-read_xlsx("Data/MACRO Clima/QBO.xlsx")
  
  
  SOI<-read_xlsx("Data/MACRO Clima/SOI.xlsx")
  
  
  SOI_DAR<-read_xlsx("Data/MACRO Clima/SOI_DAR.xlsx")
  
  
  SOI_TAH<-read_xlsx("Data/MACRO Clima/SOI_TAH.xlsx")
  
  
  SSA<-read_xlsx("Data/MACRO Clima/SSA.xlsx")
  
  
  SSAN<-read_xlsx("Data/MACRO Clima/SSAN.xlsx")
  
  
  SSAS<-read_xlsx("Data/MACRO Clima/SSAS.xlsx")
  
  
  SSN<-read_xlsx("Data/MACRO Clima/SSN.xlsx")
  
  
  TELE_INDEX<-read_xlsx("Data/MACRO Clima/TELE-INDEX-Completo.xlsx")
  
  
  TNA<-read_xlsx("Data/MACRO Clima/TNA.xlsx")
  
  
  TSA<-read_xlsx("Data/MACRO Clima/TSA.xlsx")
  
  SAM<-read_xlsx("Data/MACRO Clima/SAM.xlsx")
  
  SAM2<-read_xlsx("Data/MACRO Clima/SAM2.xlsx")
  
  #Flujos (Vazoes)
  # Vazoes<-read_xlsx("Data/MACRO Clima/Vazoes/Vazoes_mensual_R.xlsx")
  # Lista_usinas_geo<-read_xlsx("Data/MACRO Clima/Vazoes/Listado_usinas.xlsx",sheet = 2)
  
}


# Tratamiento de Datos ----


# Extraer VAZOES ----

Vazoes<-Vazoes[,-15]
#Quitar max,min,med
restric <- Vazoes[["ANO"]]=="MIN" | Vazoes[["ANO"]]=="MAX" | Vazoes[["ANO"]]=="MED" |Vazoes[["ANO"]]=="ANO"
restric <- !restric
Vazoes <- Vazoes[restric,]

#Extraer Nombre de Usinas
ind <- 86*(1:178)
Usina_listado <- c(Vazoes[["Usina"]][1],Vazoes[["ANO"]][ind])
Usina <- rep(Usina_listado,rep(85,length(Usina_listado)))

#Quitar filas (conservar solo a?os)
Vazoes <- Vazoes[-ind,]
Vazoes[["ANO"]] <- as.numeric(Vazoes[["ANO"]])

#Agregar nombre de Usinas
Vazoes[["Usina"]] <- Usina

#Escribir Verticalmente las observaciones----

Vazoes_m <- c()
n <- length(Vazoes[["ANO"]])
for(i in 1:n){
  Vazoes_m <- c(Vazoes_m,as.numeric(Vazoes[i,c(-1,-2)]))
}


#Variables listas----
Vazoes_m <- as.numeric(Vazoes_m)
Anio <- rep(Vazoes[["ANO"]],rep(12,n))
MES<-c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC")
Mes <- rep(MES,n)
Usina_m<-rep(Usina,rep(12,n))

#Latitud - Longitud----
Latitud_listado <- Lista_usinas_geo[["Latitude"]]
latitud <- rep(Latitud_listado,rep(85,length(Latitud_listado)))
latitud_m <- rep(latitud,rep(12,length(latitud)))

Longitud_listado <- Lista_usinas_geo[["Longitude"]]
longitud <- rep(Longitud_listado,rep(85,length(Longitud_listado)))
longitud_m <- rep(longitud,rep(12,length(longitud)))

#Funcion para ?ndices Mensuales ----
indice_mes <- function(B,nas=NA){
  nombres<-names(B)
  B<-as.data.frame(B)
  B[,1]<-as.numeric(B[,1])
  
  anios_todos <- rep(1931:2015,rep(12,85))
  meses_todos <- rep(1:12,85)
  
  datos <- B[B[,1]>=1931 & B[,1]<=2015,]
  lr<-length(datos[,1])
  abajo <- 12*(datos[1,1]-1931)+(datos[1,2]-1)
  arriba <- 12*(2015-datos[lr,1])+(12-datos[lr,2])
  
  indice<-c(rep(NA,abajo),datos[,3],
            rep(NA,arriba))
  #Correccion Datos Faltantes
  if(!is.na(nas)){
    indice[indice==nas]<-NA
  }
  BDD<-data.frame(anios_todos,meses_todos,indice)
  names(BDD)<-nombres[1:3]
  return(BDD)
}

a<-indice_mes(AA)

#Funcion para ?ndices ANUALES ----
indice_anual<-function(B,nas=NA){
  nombres<-names(B)
  B<-as.data.frame(B)
  B<-B[,1:13]
  A?o<-B[,1];n<-length(A?o);
  A?o<-rep(A?o,rep(12,n))
  Mes<-1:12
  Indice<-c()
  for(i in 1:n){
    Indice<-c(Indice,as.numeric(B[i,-1]))
  }
  datos<-data.frame(A?o,Mes,Indice)
  BDD<-indice_mes(datos,nas)
  return(BDD)
}



#BDD_Consolidada 
BDD_full <- data.frame(A?o=Anio,Mes=Mes,Usina=Usina_m,
                       Latitud=latitud_m,Longitud=longitud_m,
                       Vazoes=Vazoes_m,AA=indice_mes(AA)[,3],
                       NAO=indice_mes(NAO)[,3],
                       SSA=indice_mes(SSA)[,3],
                       SSAN=indice_mes(SSAN)[,3],
                       SSAS=indice_mes(SSAS)[,3],
                       SSN=indice_mes(SSN)[,3],
                       AAO=indice_anual(AAO)[,3],
                       AMM_sst=indice_anual(AMM_sst,-99)[,3],
                       AMM_wind=indice_anual(AMM_wind,-99)[,3],
                       AMO=indice_anual(AMO,-99.99)[,3],
                       AO=indice_anual(AO)[,3],
                       MEI=indice_anual(MEI)[,3],
                       MEI_Ext=indice_anual(MEI_Ext)[,3],
                       MEI_Rank=indice_anual(MEI_Rank)[,3],
                       Nino12=indice_anual(Nino12,-99.99)[,3],
                       Nino3=indice_anual(Nino3,-99.99)[,3],
                       Nino34=indice_anual(Nino34,-99.99)[,3],
                       Nino4=indice_anual(Nino4,-99.99)[,3],
                       PDO=indice_anual(PDO)[,3],
                       QBO=indice_anual(QBO)[,3],
                       SOI=indice_anual(SOI)[,3],
                       SOI_DAR=indice_anual(SOI_DAR,-990)[,3],
                       SOI_TAH=indice_anual(SOI_TAH,-990)[,3],
                       TNA=indice_anual(TNA,-99.99)[,3],
                       TSA=indice_anual(TSA,-99.99)[,3],
                       SAM=indice_anual(SAM)[,3],
                       SAM2=indice_anual(SAM2)[,3]
)


#GUARDAR TODO

save(BDD_full,file = "Base_Consolidada.RData")

write.csv(BDD_full,file = "Base_Consolidada_mensual.csv")


load("Base_Consolidada.RData")  #Tiene Base Consolidada
load("Base_Datos.RData") #Tiene Bases Individuales

